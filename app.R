library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(magrittr)

tidytuesday_tweets <- readRDS("data/tidytuesday_tweets.rds")

datasets <- read_tsv("data/datasets.tsv")

tweet_tags <- read_tsv("data/tweets.tsv", col_types = list(status_id = col_character())) %>%
  select(status_id, dataset_id, is_dataset_intro) %>%
  left_join(datasets, by = "dataset_id")

tidytuesday_tweets <- tidytuesday_tweets %>%
  left_join(tweet_tags, by = "status_id") %>%
  filter(dataset_id != "x")

ui <- fluidPage(
  
  tags$head(HTML('<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')),
  
  titlePanel('tidytuesday.rocks'),
  
  sidebarPanel(
    uiOutput('intro'),
    selectInput('topic', 'Choose a dataset', datasets$topic),
    selectInput('sort_by', 'Sort tweets', c("Most recent", "Most likes", "Most retweets"), selected = "Most recent")
  ),
  
  mainPanel(
    h1(textOutput('topic')),
    uiOutput('tweets')
  )
)

embed_tweet <- function(tweet) {
  tags$blockquote(class = "twitter-tweet", tags$a(href = paste0("https://twitter.com/", tweet$screen_name, "/status/", tweet$status_id)))
}

server <- function(input, output, session) {
  

  filtered_tweets <- reactive({
    tidytuesday_tweets %>%
      filter(!is_dataset_intro, topic == input$topic) %>%
      select(screen_name, status_id, created_at, favorite_count, retweet_count)
  })
  
  selected_intro <- reactive({
    tidytuesday_tweets %>%
      filter(is_dataset_intro, topic == input$topic) %>%
      select(screen_name, status_id) %>%
      transpose()
  })
  
  
  sorted_tweets <- reactive({
    switch(input$sort_by,
           "Most recent" = filtered_tweets() %>% arrange(desc(created_at)),
           "Most likes" = filtered_tweets() %>% arrange(desc(favorite_count)),
           "Most retweets" = filtered_tweets() %>% arrange(desc(retweet_count)))
  })
  
  output$topic <- reactive({
    input$topic
  })
  
  output$intro <- renderUI({
    tagList(lapply(selected_intro(), embed_tweet), tags$script('twttr.widgets.load(document.getElementById("intro"));'))
  })
  
  output$tweets <- renderUI({
    tagList(lapply(transpose(sorted_tweets()), embed_tweet), tags$script('twttr.widgets.load(document.getElementById("tweets"));'))
  })
}


shinyApp(ui = ui, server = server)