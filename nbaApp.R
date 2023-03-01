library(plotly) 
library(dplyr)
library(shiny)
library(shinythemes)
library(reshape2)
library(RCurl)
library(tidytext)
library(reactable)

pos <- read.csv(text = getURL("https://raw.githubusercontent.com/poudelananda/NBA/main/playerPos.csv"))
pos <- pos %>% select(Player, Pos)
#pos$Player <- iconv(pos$Player,from="UTF-8",to="ASCII//TRANSLIT")
pos$Player <- gsub("'", "", pos$Player)
colnames(pos) <- c("namePlayer", "Position")
df <- read.csv(text = getURL("https://raw.githubusercontent.com/poudelananda/NBA/main/nba_df.csv"))
df$namePlayer <- gsub("'", "", df$namePlayer)
df <- left_join(df, pos, by = "namePlayer")


ui <- fluidPage(
  theme = shinytheme("sandstone"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "input.tabselected == 2",
        selectInput(
          inputId = "teamSummary",
          label = "Select Team",
          choices = c(sort(unique(df$slugTeam)))
        )
      ),
      conditionalPanel(
        condition = "input.tabselected == 3",
        selectInput(
          inputId = "playerName",
          label = "Select Player",
          choices = c(sort(unique(df$namePlayer)))
        ),
        checkboxGroupInput(
          inputId = "playerLocation",
          label = "Select Location",
          choices = c("H", "A"),
          selected = c("H", "A")
        )
      ),
      conditionalPanel(
        condition = "input.tabselected == 4",
        selectInput(
          inputId = "playerStat",
          label = "Select Player",
          choices = c(unique(df$namePlayer))
        ),
        selectInput(
          inputId = "selectPoints",
          label = "Select Points to Predict",
          choices = c(1:100)
        ),
        selectInput(
          inputId = "selectAssists",
          label = "Select Assists to Predict",
          choices = c(1:100)
        ),
        selectInput(
          inputId = "selectRebounds",
          label = "Select Rebounds to Predict",
          choices = c(1:100)
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Overall Summary",
          value = 1,
          fluidRow(
            column(width = 8,div(style = 'padding:10px'),
                   reactableOutput("overallSummary"))
          )
        ),
        tabPanel(
          "Team Summary",
          value = 2,
          fluidRow(
            column(width = 8,style = 'padding:10px',
                   plotlyOutput("teamOffensePlot"))
          ),
          fluidRow(
            column(width = 8,style = 'padding:10px',
                   plotlyOutput("teamDefensePlot"))
          )
        ),
        tabPanel(
          "Player Summary",
          value = 3,
          fluidRow(
            column(width = 8,style = 'padding:10px',
                   plotlyOutput("playerPlot", height = 700, width = 900))
          ),
          fluidRow(
            column(width = 8,style = 'padding:10px',
                   plotlyOutput("playerPlot2", height = 800, width = 900))
          )
        ),
        tabPanel(
          "Poisson Analysis",
          value = 4,
          fluidRow(
            column(width = 8,style = 'padding:10px',
                   plotlyOutput("poissonDistribution"))
          ),
          fluidRow(
            column(width = 8,style = 'padding:10px',
                   verbatimTextOutput("poissonPointsPrediction"))
          ),
          fluidRow(
            column(width = 8,style = 'padding:10px',
                   verbatimTextOutput("poissonAssistsPrediction"))
          ),
          fluidRow(
            column(width = 8,style = 'padding:10px',
                   verbatimTextOutput("poissonReboundsPrediction"))
          )
        ),
        id = "tabselected"
      )
    )
  )
)

server = function(input, output) {
  
  
  # 1) summary: 
  #   data table with overall number of wins, loss, average points, fg2pct, fg3pct, avg. off reb, avg. def reb, avg steal, avg block,
  # 2) team: 
  #   Track which Position.y scores the most amount of points. 
  #   Track which Position.y concedes the most amount of points.
  # 3) player:
  #   pie chart for shooting/defense overall or each game.
  #   over time trend for p+r+a
  # 4) statistics:
  #   first choose: player or team
  #   poisson distribution: calculate the probability of a player/team scoring certain amount of points given that they average (____) when playing 20+ minutes
  #     graph, summary
  #   regression: predict the number of points scored by player/team based on opPosition.y, backtoback, and location.
  #     graph, summary
  
  output$overallSummary <- renderReactable ({
    player_df <- df %>% select(namePlayer,slugTeam, pts, fg2m, fg2a, fg3m, fg3a, oreb, dreb, stl, blk, minutes)
    player_df <- player_df[which(player_df$minutes > 20),]
    player_df <- player_df %>% group_by(namePlayer, slugTeam) %>%
      summarise(avgpts = mean(pts), totfg2m = sum(fg2m), totfg2a = sum(fg2a), totfg3m = sum(fg3m), 
                totfg3a = sum(fg3a), avgstl = mean(stl), avgblk = mean(blk), avgoreb = mean(oreb), avgdred = mean(dreb), avgmins = mean(minutes),
                games = n())
    player_df$fg2pct <- player_df$totfg2m/player_df$totfg2a
    player_df$fg3pct <- player_df$totfg3m/player_df$totfg3a
    player_df <- player_df %>% select(-c(totfg2m, totfg3m))
    player_df <- player_df %>% mutate_if(is.numeric, round, 2)
    theme <- reactableTheme(
      color = "hsl(233, 9%, 87%)",
      backgroundColor = "hsl(233, 9%, 19%)")
    reactable(
      player_df,
      groupBy = "slugTeam",
      theme = theme,
      style = list(fontFamily = "Work Sans, sans-serif", fontSize = "1rem"),
      resizable = TRUE,
      wrap = TRUE,
      bordered = TRUE
    )
  })
  
  output$teamOffensePlot <- renderPlotly ({
    #tempdf <- df[which(df$slugTeam == "LAL"),]
    tempdf <- df[which(df$slugTeam == input$teamSummary),]
    tempdf <- tempdf %>% select(Position.y, pts, minutes)
    tempdf <- tempdf[which(tempdf$minutes > 20),]
    colnames(tempdf) <- c("Position.y", "Points")
    tempdf$Position.y <- as.factor(tempdf$Position.y)
    tempdf <- aggregate(list(tempdf$Points), by = list(tempdf$Position.y),
                        FUN = mean)
    colnames(tempdf) <- c("Position.y", "Points")
    tempdf <- melt(tempdf, id.vars = c("Position.y"))
    p <- ggplot(tempdf, aes(x = reorder_within(Position.y, -value, variable), 
                            y = value, fill = Position.y)) +
      geom_bar(stat = "identity") +
      ggtitle("Average Points Scored by Position.y (20+mins)") +
      facet_wrap(~variable, nrow = 3, scales = "free")
    ggplotly(p, width = 950, height = 400)
  })
  
  output$teamDefensePlot <- renderPlotly ({
    tempdf <- df[which(df$slugOpponent == input$teamSummary),]
    tempdf <- tempdf %>% select(Position.y, pts, minutes)
    tempdf <- tempdf[which(tempdf$minutes > 20),]
    colnames(tempdf) <- c("Position.y", "Points")
    tempdf$Position.y <- as.factor(tempdf$Position.y)
    tempdf <- aggregate(list(tempdf$Points), by = list(tempdf$Position.y),
                        FUN = mean)
    colnames(tempdf) <- c("Position.y", "Points")
    tempdf <- melt(tempdf, id.vars = c("Position.y"))
    p <- ggplot(tempdf, aes(x = reorder_within(Position.y, -value, variable), 
                            y = value, fill = Position.y)) +
      geom_bar(stat = "identity") +
      ggtitle("Average Points Conceded by Position.y (20+mins)") +
      facet_wrap(~variable, nrow = 3, scales = "free")
    ggplotly(p, width = 950, height = 400)
  })
  
  
  
  output$playerPlot <- renderPlotly({
    #tempdf <- df[which(df$namePlayer == "Anthony Davis" & df$locationGame %in% c("H", "A")),]
    tempdf <- df[which(df$namePlayer == input$playerName & df$locationGame %in% input$playerLocation),]
    tempdf <- tempdf %>% select(dateGame, pts, treb, ast, minutes, slugOpponent)
    colnames(tempdf) <- c("Date", "Points", "Rebounds", "Assists", "Minutes", "Opponents")
    tempdf <- melt(tempdf, id.vars = c("Date", "Opponents"))
    meandf <- tempdf %>% group_by(variable) %>% summarize(mean_val = round(mean(value)),3)
    tempdf <- left_join(tempdf, meandf, by = "variable")
    
    p <- ggplot(tempdf, aes(x = Date, y = value)) + geom_point() + geom_line(aes(group = 1)) +
      geom_hline(data = meandf, aes(yintercept = mean_val), linetype = "dotted") +
      #geom_text(aes(x = mean(Date), y = mean_val-1, label = paste("Mean:", mean_val))) +
      geom_text(aes(x = Date, y = value + 1, label = Opponents)) +
      facet_wrap(~variable, scales = "free", nrow = 4)
    ggplotly(p, width = 900, height = 700)
    
  })
  
  output$playerPlot2 <- renderPlotly({
    #tempdf <- df[which(df$namePlayer == "Anthony Davis" & df$locationGame %in% c("H", "A")),]
    tempdf <- df[which(df$namePlayer == input$playerName & df$locationGame %in% input$playerLocation),]
    tempdf <- tempdf %>% select(dateGame, locationGame, slugOpponent, outcomeGame, fg3m, fg3a, fg2m, fg2a)
    tempdf$fg3missed <- tempdf$fg3a - tempdf$fg3m
    tempdf$fg3made <- tempdf$fg3m
    tempdf$fg2missed <- tempdf$fg2a - tempdf$fg2m
    tempdf$fg2made <- tempdf$fg2m
    
    fig1 <- plot_ly(tempdf, x = ~dateGame, y = ~fg2made, type = 'bar', name = 'FG2Made') %>%
      add_trace(y = ~fg2missed, name = 'FG2Missed') %>%
      layout(barmode = "stack")
    fig2 <- plot_ly(tempdf, x = ~dateGame, y = ~fg3made, type = 'bar', name = 'FG3Made') %>%
      add_trace(y = ~fg3missed, name = 'FG3Missed') %>%
      layout(barmode = "stack")
    subplot(fig1, fig2, nrows = 2, margin = 0.08, shareY = FALSE, shareX = TRUE)
  })
  
  output$poissonDistribution <- renderPlotly ({
    #tempdf <- df[which(df$namePlayer == "Anthony Davis"),]
    tempdf <- df[which(df$namePlayer == input$playerStat),]
    tempdf <- tempdf %>% select(dateGame, pts, treb, ast)
    colnames(tempdf) <- c("Date", "Points", "Rebounds", "Assists")
    tempdf <- melt(tempdf, id.vars = c("Date"))
    p <- ggplot(tempdf, aes(x = value)) + 
      geom_histogram(aes(y=..density..),      #Histogram with density instead of count on y-axis
                     binwidth=.5,
                     colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666") +
      facet_wrap(~variable, scales = "free", nrow = 3)
    ggplotly(p)
  })
  
  output$poissonPointsPrediction <- renderText ({
    tempdf <- df[which(df$namePlayer == input$playerStat),]
    paste("Probability of scoring",input$selectPoints,":"
          ,1 - ppois(as.numeric(input$selectPoints), mean(tempdf$pts)))
  })
  
  output$poissonAssistsPrediction <- renderText ({
    tempdf <- df[which(df$namePlayer == input$playerStat),]
    paste("Probability of assisting",input$selectAssists,":"
          ,1 - ppois(as.numeric(input$selectAssists), mean(tempdf$ast)))
  })
  
  output$poissonReboundsPrediction <- renderText ({
    tempdf <- df[which(df$namePlayer == input$playerStat),]
    paste("Probability of rebounding",input$selectRebounds,":"
          ,1 - ppois(as.numeric(input$selectRebounds), mean(tempdf$treb)))
  })
  
  
  
}

shinyApp(ui, server)