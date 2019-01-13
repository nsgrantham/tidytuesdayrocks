library(shiny)
library(dplyr)
library(purrr)
library(magrittr)

tidytuesday_tweets <- readRDS("data/tidytuesday_tweets.rds")

ui <- fluidPage(
  
  tags$head(HTML('<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')),
  
  titlePanel('#TidyTuesday Tweets'),
  
  sidebarPanel(
    selectInput('screen_name', 'Twitter username', unique(tidytuesday_tweets$screen_name))
  ),
  
  mainPanel(
    uiOutput('tweets')
  )
)

embed_tweet <- function(tweet) {
  tags$blockquote(class = "twitter-tweet", tags$a(href = paste0("https://twitter.com/", tweet$screen_name, "/status/", tweet$status_id)))
}

server <- function(input, output, session) {
  
  selected_tweets <- reactive({
    tidytuesday_tweets %>%
      filter(screen_name == input$screen_name) %>%
      select(screen_name, status_id) %>%
      transpose()
  })
  
  output$tweets <- renderUI({
    tagList(lapply(selected_tweets(), embed_tweet), tags$script('twttr.widgets.load(document.getElementById("tweets"));'))
  })
}


shinyApp(ui = ui, server = server)