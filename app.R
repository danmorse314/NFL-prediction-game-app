#   Lee's NFL Prediction Game data
#   TESTING

library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(glue)
library(Cairo)
library(patchwork)

options(shiny.usecairo = TRUE)

df_initial <- read_csv("https://github.com/nflverse/nfldata/raw/master/data/predictions.csv",
               col_types = cols())

#   trying to calculate brier scores
#   doesn't match lee's scores
df_results <- readRDS(url("https://github.com/nflverse/nfldata/raw/master/data/games.rds")) |>
    filter(season == 2021)

#df_wins <- df_results |>
#    mutate(win = ifelse(home_score > away_score, 1, 0)) |>
#    select(game_id, team = home_team, score = home_score, win) |>
#    bind_rows(
#        df_results |>
#            mutate(win = ifelse(away_score > home_score, 1, 0)) |>
#            select(game_id, team = away_team, score = away_score, win)
#    ) |>
#    filter(!is.na(score)) |>
#    arrange(game_id)
#   /end brier calcs

df <- df_initial |>
    separate(game_id, into = c("season","week","away_team","home_team"),
             sep = "_", remove = FALSE, convert = TRUE) |>
    filter(week <= max(week)) |>
    left_join(
        df_results |> select(game_id, away_score, home_score),
        by = "game_id"
    )

wk <- max(df$week)

df_home <- df |>
    select(screen_name, week, team = home_team, prediction)

df_away <- df |>
    mutate(prediction = 100-prediction) |>
    select(screen_name, week, team = away_team, prediction)

df_full <- bind_rows(df_home, df_away) |>
    arrange(screen_name, week, team)

#   more brier scores, doesn't match lee yet
#df_scores <- df_full |>
#    filter(prediction > 50) |>
#    left_join(select(df_wins, -score), by = "team") |>
#    mutate(
#        week = as.numeric(substr(game_id,6,7)),
#        m = case_when(
#            week <= 18 ~ 1,
#            week == 19 ~ 2,
#            week == 20 ~ 3,
#            week == 21 ~ 4,
#            week == 25 ~ 5
#        ),
#        brier_score = m * (25 - (100 * (prediction/100 - win)^2))
#        ) |>
#    group_by(screen_name) |>
#    summarize(
#        brier_score = round(sum(brier_score, na.rm = TRUE),1),
#        .groups = "drop"
#        ) |>
#    arrange(-brier_score)

# pulling out the market predictions
market <- df_full |>
    filter(screen_name == "Market") |>
    select(week, team, market_prediction = prediction)

df_favs <- df_full |>
    # removing the market predictions
    filter(screen_name != "Market") |>
    left_join(market, by = c("week","team")) |>
    mutate(
        favoritism = prediction - market_prediction
    ) |>
    group_by(screen_name, team) |>
    summarize(
        favoritism = sum(favoritism),
        .groups = "drop"
    )

alt_teams <- c("SEA","PIT","DEN","DAL","CIN","JAX")

team_colors <- nflfastR::teams_colors_logos |>
    mutate(
        team_color_hold = team_color,
        team_color = ifelse(team_abbr %in% alt_teams,team_color2,team_color),
        team_color2 = ifelse(team_abbr %in% alt_teams, team_color_hold, team_color2)
    ) |>
    select(-team_color_hold) |>
    filter(team_abbr %in% unique(market$team))

# create legend

df_legend <- dplyr::tibble(
    y = rep(1,2),
    x = c(.5,.5),
    alpha = c(1,.1),
    label = c("Result (shaded)","Median Prediction (full color)")
)

df_segments <- dplyr::tibble(
    x = c(0,.5,.75,0,1,0,0,.75),
    xend = c(0,.5,.75,0,1,.5,1,.8),
    y = c(.6,.6,.6,1.4,1.4,.5,1.5,.5),
    yend = c(.5,.5,.5,1.5,1.5,.5,1.5,.5)
)

df_labels <- dplyr::tibble(
    x = c(.25,.5,.9),
    y = c(.5,1.5,.5),
    label = c("Median Prediction","Game Result","Market Prediction")
)

legend_plot <- df_legend |>
    ggplot2::ggplot(ggplot2::aes(x,as.factor(y))) +
    ggplot2::geom_col(
        position = "stack",
        alpha = df_legend$alpha,
        width = .6
    ) +
    ggplot2::geom_segment(
        ggplot2::aes(x=.75, xend=.75, y=.7, yend=1.3)
    ) +
    ggplot2::geom_segment(
        data = df_segments,
        ggplot2::aes(x=x,y=y,xend=xend,yend=yend)
    ) +
    ggplot2::geom_label(
        data = df_labels,
        ggplot2::aes(x,y,label = label),
        label.size = NA,
        size = 2
    ) +
    ggplot2::theme_void() +
    ggplot2::scale_x_continuous(limits = c(0,1))

create_week_plot <- function(wk){
    # get predictions
    df_preds <- df_initial
    
    df_market <- df_preds |>
        dplyr::filter(screen_name == "Market") |>
        dplyr::filter(as.numeric(substr(game_id,6,7)) == wk) |>
        dplyr::mutate(market_prediction = prediction - 50)
    
    df_week <- df_preds |>
        dplyr::filter(screen_name != "Market") |>
        dplyr::group_by(game_id) |>
        dplyr::summarize(
            prediction = round(median(prediction),1)-50,
            .groups = "drop"
        ) |>
        dplyr::filter(as.numeric(substr(game_id,6,7)) == wk)
    
    # get games with results
    df_results <- df_results |>
        dplyr::filter(!is.na(result)) |>
        dplyr::filter(week == wk)
    
    df <- dplyr::tibble(
        game_id = df_results$game_id,
        away_team = df_results$away_team,
        home_team = df_results$home_team,
        result = ifelse(df_results$result > 0, 50, -50)
    ) |>
        dplyr::left_join(
            dplyr::select(team_colors, away_logo = team_logo_espn, away_color = team_color, team_abbr),
            by = c("away_team" = "team_abbr")
        ) |>
        dplyr::left_join(
            dplyr::select(team_colors, home_logo = team_logo_espn, home_color = team_color, team_abbr),
            by =  c("home_team" = "team_abbr")
        ) |>
        dplyr::left_join(
            dplyr::select(df_market, market_prediction, game_id),
            by = "game_id"
        ) |>
        dplyr::left_join(
            df_week, by = "game_id"
        ) |>
        dplyr::mutate(
            y = dplyr::row_number(),
            y = max(y)-y+1,
            home_x = 50,
            away_x = -50,
            result_color = ifelse(result == 50, home_color, away_color),
            prediction_color = ifelse(prediction > 0, home_color, away_color)
        )
    
    dark_teams <- c("HOU","TEN","CHI","LA","NE","MIN","GB","BAL","TEN")
    
    df_market_segments <- df |>
        dplyr::select(home_team,away_team,prediction,market_prediction,result,y) |>
        dplyr::mutate(
            segment_color = dplyr::case_when(
                market_prediction >= prediction &
                    market_prediction < 0 &
                    away_team %in% dark_teams ~ "#ededed",
                market_prediction <= prediction &
                    market_prediction > 0 &
                    home_team %in% dark_teams ~ "#ededed",
                TRUE ~ "black"
            )
        )
        
    
    week_plot <- df |>
        ggplot2::ggplot(ggplot2::aes(y=y)) +
        # result bar
        ggplot2::geom_col(
            ggplot2::aes(x = result, y = as.factor(y)),
            position = "stack",
            fill = df$result_color, alpha = .5, width = .6
        ) +
        # user predictions
        ggplot2::geom_col(
            ggplot2::aes(x = prediction, y = as.factor(y)),
            fill = df$prediction_color, width = .6
        ) +
        # market prediction
        ggplot2::geom_segment(
            data = df_market_segments,
            ggplot2::aes(x = market_prediction, xend = market_prediction,
                         y = y-.3, yend = y+.3),
            color = df_market_segments$segment_color
        ) +
        # logos
        ggimage::geom_image(
            data = df,
            ggplot2::aes(x = home_x, image = home_logo),
            size = 0.07, asp = 1.5
        ) +
        ggimage::geom_image(
            data = df,
            ggplot2::aes(x = away_x, image = away_logo),
            size = 0.07, asp = 1.5
        ) +
        # add legend
        #ggimage::geom_image(
        #  ggplot2::aes(x = 25,y=17.5,image="C:/Users/danmo/Documents/R/nfl_prediction_game/nfl-prediction-game-stats/legend_plot.png"),
        #  size = 0.5
        #) +
        # theming
        ggplot2::scale_x_continuous(
            breaks = seq(-50,50,10),
            labels = c(seq(100,50,-10),seq(60,100,10))
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = .5),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank()
        ) +
        #ggplot2::coord_fixed(ratio = 100/17) +
        # labels
        ggplot2::labs(
            x = "Win Probability (%)", y = NULL,
            caption = "data: @LeeSharpeNFL | chart: @DeryckG_"
        )
    
    title_plot <- ggplot2::ggplot(
        data = df,
        ggplot2::aes(1,2,label=glue::glue("Week {wk} Results"))
    ) +
        ggplot2::geom_text(size=5) +
        theme_void()
    
    return_plot <- (title_plot + legend_plot)/week_plot + plot_layout(heights = c(1,8),)
    
    return(return_plot)
}

ui <- fluidPage(
    titlePanel("Analyzing Lee Sharpe's NFL Prediction Game"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            tags$div(
                id = "inline",
                uiOutput(outputId = "userfilter"),
                uiOutput(outputId = "gamefilter")
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                id = "tabs",
                selected = "home",
                tabPanel(
                    "Home", value = "home",
                    plotOutput("marketplot", width = "100%")
                ),
                tabPanel(
                    "Users", value = "user",
                    plotOutput("favplot", width = "100%")
                ),
                tabPanel(
                    "Teams", value = "teams",
                    uiOutput(outputId = "teamlogo"),
                    DT::dataTableOutput("team_favs", width = "75%")
                ),
                tabPanel(
                    "Games", value = "games",
                    fluidRow(
                        column(
                            width = 7,
                            plotOutput(outputId = "weekplot")
                        ),
                        column(
                            width = 5,
                            plotOutput(outputId = "gameplot")
                        )
                    )
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #   Sidebar filters
    
    output$userfilter <- renderUI({
        
        if(input$tabs == "user"){
            selectInput(
                inputId = "tweeter",
                choices = unique(df$screen_name),
                selected = sample(unique(df$screen_name),1),
                label = "Select user"
            )
        } else if(input$tabs == "teams"){
            
            #   add team logos as choices
            logo_options <- list(
                content =  mapply(
                                pull(team_colors,team_name),
                                pull(team_colors, team_logo_espn),
                                FUN = function(team_name, team_logo_espn) {
                                         HTML(paste(
                                             tags$img(src=team_logo_espn, width=20, height=15),
                                             team_name
                                         ))
                                     }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                                 
            )
            #   create select input--user sees logos, backend sees abbreviations
            pickerInput(
                inputId = "team",
                label = "Select team",
                multiple = FALSE,
                selected = sample(pull(team_colors,team_abbr),1),
                choices = pull(team_colors, team_abbr),
                choicesOpt = logo_options
            )

        } else if(input$tabs == "games") {
            
            selectInput(
                inputId = "wk",
                choices = unique(df$week),
                selected = max(df$week),
                label = "Select week"
            )
            
        } else if(input$tabs == "home"){
            tags$div(
                glue("Welcome! Come and see how the {length(unique(df_full$screen_name))-1}
                  NFL fans participating in Lee's NFL prediction game
                  stack up against the betting market!"),
                br(),
                br(),
                "You can find the leaderboard and update your picks ",
                a(href = "https://nflgamedata.com/predict/leaderboard.php",
                  "here"),
                br(),
                br(),
                "The data for this app can be found ",
                a(href = "https://github.com/nflverse/nfldata/tree/master/data",
                  "here")
            )
        }
        
    })
    
    observeEvent(input$tabs,{
        
        if(input$tabs == "games"){
            observeEvent(input$wk,{
                
                output$gamefilter <- renderUI({
                    done_games <- df |>
                        dplyr::filter(week == input$wk) |>
                        distinct(game_id, week, away_team, home_team)
                    
                    #   create clean list of game IDs for user
                    game_options <- list(
                        content = mapply(
                            done_games$week, done_games$away_team, done_games$home_team,
                            FUN = function(week,away_team,home_team){
                                glue::glue("Wk{week}: {away_team} @ {home_team}")
                            },
                            SIMPLIFY = FALSE, USE.NAMES = FALSE
                        )
                    )
                    
                    pickerInput(
                        inputId = "game",
                        choices = done_games$game_id,
                        choicesOpt = game_options,
                        selected = sample(done_games$game_id,1),
                        label = "Select game:"
                    )
                })
                
            })
        }
        else{
            output$gamefilter <- renderUI({
                NULL
            })
        }
    })
    
    #   HOME TAB
    
    output$marketplot <- renderPlot({
        
        df_agg <- df_full |>
            filter(screen_name != "Market") |>
            group_by(team, week) |>
            summarize(
                prediction = mean(prediction),
                .groups = "drop"
            ) |>
            left_join(market, by = c("week","team")) |>
            mutate(
                favoritism = prediction - market_prediction
            ) |>
            group_by(team) |>
            summarize(
                favoritism = round(mean(favoritism),2),
                .groups = "drop"
            ) |>
            left_join(team_colors, by = c("team" = "team_abbr"))
        
        aspect <- 2/3*32/(max(df_agg$favoritism) - min(df_agg$favoritism) + 1)
        
        df_agg |>
            ggplot(aes(reorder(team, desc(favoritism)), favoritism)) +
            geom_col(
                fill = df_agg$team_color
            ) +
            geom_segment(aes(x = 15.5, xend = 15.5, y = max(favoritism)-3, yend = max(favoritism)),
                         color = "red", arrow = arrow(length = unit(0.1, "inches"), type = "closed")) +
            geom_segment(aes(x = 15.5, xend = 15.5, y = min(favoritism)+3, yend = min(favoritism)),
                         color = "red", arrow = arrow(length = unit(0.1, "inches"), type = "closed")) +
            annotate("text", x = 15, y = max(df_agg$favoritism),
                     hjust = 1, vjust = 1, color = "red",
                     label = "Fans like relative\nto market") +
            annotate("text", x = 16, y = min(df_agg$favoritism),
                     hjust = 0, vjust = 0, color = "red",
                     label = "Fans dislike\nrelative to market") +
            ggimage::geom_image(
                aes(y = ifelse(favoritism >= 0, favoritism + .5, favoritism - .5),
                    image = team_logo_wikipedia),
                size = 0.04, asp = 3/2
            ) +
            theme_bw() +
            theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
            ) +
            scale_y_continuous(breaks = scales::pretty_breaks()) +
            coord_fixed(ratio = aspect) +
            labs(
                x = NULL, y = "Favoritism",
                title = "Fan opinion vs the market",
                subtitle = glue("as of {Sys.Date()}"),
                caption = "data: @LeeSharpeNFL | chart: @danmorse_"
            )
        
    })
    
    #   USER TAB
    
    output$favplot <- renderPlot({
        
        req(input$tweeter)
        
        #   add user scores, in subtitle or on plot
        #   add twitter profile pics if can figure out scraping
        
        user <- input$tweeter
        
        user_favs <- df_favs |>
            filter(screen_name == user) |>
            left_join(team_colors, by = c("team" = "team_abbr"))
        
        aspect <- 2/3*32/(max(user_favs$favoritism) - min(user_favs$favoritism) + 2)
        
        user_favs |>
            ggplot(aes(reorder(team, desc(favoritism)), favoritism)) +
            geom_col(
                fill = user_favs$team_color
            ) +
            ggimage::geom_image(
                aes(y = ifelse(favoritism >= 0, favoritism + 1, favoritism - 1),
                    image = team_logo_wikipedia),
                size = 0.04, asp = 1.5
            ) +
            theme_bw() +
            theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
            ) +
            scale_y_continuous(breaks = scales::pretty_breaks()) +
            coord_fixed(ratio = aspect) +
            labs(
                x = NULL, y = "Favoritism",
                title = glue("{user}'s favorite teams"),
                subtitle = glue("as of {Sys.Date()}"),
                caption = "data: @LeeSharpeNFL | chart: @danmorse_"
            )
    })
    
    #   TEAM TAB
    
    output$teamlogo <- renderUI({
        
        if(input$tabs == "teams"){
            req(input$team)
            logo_url <- dplyr::filter(team_colors, team_abbr == input$team) |>
                dplyr::pull(team_wordmark)
            team_name <- filter(team_colors, team_abbr == input$team) |>
                pull(team_name)
            HTML(glue("<center><img src={logo_url} height = '60px'><br>{team_name}' biggest fans</center>"))
        }
        
    })
    
    output$team_favs <- DT::renderDataTable({
        
        req(input$team)
        
        df_team <- df_favs |>
            filter(team == input$team) |>
            arrange(desc(favoritism)) |>
            select(User = screen_name,
                   Favoritism = favoritism)
        
        DT::datatable(
            df_team,
            callback = JS("
    table.on( 'order.dt search.dt', function () {
        table.column(0, {search:'applied', order:'applied'}).nodes().each(
            function (cell, i) {
                cell.innerHTML = i+1;
            } );
    } )"),
            selection = "single",
            options = list(
                columnDefs = list(list(
                    className = "dt-center", targets = c(1,2)
                )),
                lengthMenu = list(c(10, 25, 50), c('10', '25', '50'))
                #pageLength = 32
                #)
            )
        )
        
    })
    
    #   GAME TAB
    
    output$weekplot <- renderPlot({
        
        #   Deryck's weekly plot
        
        
        
    })
    
    output$gameplot <- renderPlot({
        
        req(input$game)
        
        #   add game final scores somewhere, can probably get from lee
        
        df_game <- filter(df, game_id == input$game)
        
        home_team <- unique(df_game$home_team)
        home_score <- unique(df_game$home_score)
        home_color <- filter(team_colors, team_abbr == home_team) |>
            pull(team_color)
        away_team <- unique(df_game$away_team)
        away_score <- unique(df_game$away_score)
        away_color <- filter(team_colors, team_abbr == away_team) |>
            pull(team_color)
        game_week <- unique(df_game$week)
        if(home_score > away_score){
            final_score <- glue("Final: {home_team} {home_score} | {away_team} {away_score}")
        } else {
            final_score <- glue("Final: {away_team} {away_score} | {home_team} {home_score}")
        }
        
        transparent <- function(img) {
            magick::image_fx(img, expression = "0.5*a", channel = "alpha")
        }
        
        logos <- team_colors |>
            filter(team_abbr %in% c(home_team,away_team)) |>
            mutate(x = 1, y = ifelse(team_abbr == home_team, 75,25))
        
        df_game |>
            ggplot(aes(1, prediction)) +
            annotate("rect", xmin = .9, xmax = 1.1, ymin = 50, ymax = 101,
                     fill = home_color, alpha = .2) +
            annotate("rect", xmin = .9, xmax = 1.1, ymin = -1, ymax = 50,
                     fill = away_color, alpha = .2) +
            ggimage::geom_image(
                data = logos, aes(x = x, y = y, image = team_logo_wikipedia),
                image_fun = transparent, size = 0.3, asp = 1.5
            ) +
            geom_jitter(height = 0, width = .1, size = 3, alpha = .7) +
            geom_hline(yintercept = filter(df_game, screen_name == "Market") |> pull(prediction),
                       color = "red", linetype = "dashed") +
            geom_label(aes(x = .901, y = (filter(df_game, screen_name == "Market") |> pull(prediction))+1),
                       hjust = 0, vjust = 0, color = "red", label = "Market", alpha = .8) +
            #annotate("text", x = .901, y = (filter(df_game, screen_name == "Market") |> pull(prediction))+1,
            #         hjust = 0, vjust = 0, color = "red", label = "Market") +
            scale_y_continuous(breaks = c(0,25,50,75,100),
                               limits = c(-1,101),
                               labels = c("100%","75%","50%","75%","100%"),
                               expand = c(0,0)) +
            scale_x_continuous(expand = c(0,0)) +
            #coord_fixed(ratio = 2/3*.2/100) +
            theme_bw() +
            theme(
                axis.ticks.x = element_blank(),
                axis.text.x = element_blank()
            ) +
            labs(
                title = final_score,
                x = "this axis has no meaning", y = "Prediction",
                caption = "data: @LeeSharpeNFL | chart: @danmorse_"
            )
    })
    
    output$weekplot <- renderPlot({
        
        req(input$wk)
        
        create_week_plot(input$wk)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
