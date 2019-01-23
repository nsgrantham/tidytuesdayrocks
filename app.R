library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
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
  
  tags$head(HTML('<style>
                  * {
                 font-size: 100%;
                 font-family: Roboto Regular;
                 }</style>')),
  tags$head(HTML('<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')),
  
   
  fluidRow(
    column(1),
    column(3,
    h2('tidytuesday.rocks', style="font-family:Roboto Mono;"),
    selectInput('filter_dataset', 'Choose a dataset', datasets$dataset_name, 
                selected = datasets$dataset_name[sample.int(nrow(datasets))]),
    selectInput('sort_by', 'Sort tweets', c("Most recent", "Most likes", "Most retweets"), 
                selected = "Most recent")
    ),
    column(5,
      h2(textOutput('dataset_name')),
      tags$hr(),
      h3('Dataset links'),
      p(uiOutput('dataset_files')),
      p(uiOutput('dataset_articles')),
      p(uiOutput('dataset_sources')),
      h3(textOutput('tweets_sorted_by')),
      uiOutput('tweets')
    ),
    column(3)
  )
)

embed_tweet <- function(tweet) {
  tags$blockquote(class = "twitter-tweet", tags$a(href = paste0("https://twitter.com/", tweet$screen_name, "/status/", tweet$status_id)))
}

server <- function(input, output, session) {
  
  filtered_tweets <- reactive({
    tidytuesday_tweets %>%
      filter(!is_dataset_intro, dataset_name == input$filter_dataset) %>%
      select(screen_name, status_id, created_at, favorite_count, retweet_count)
  })
  
  sorted_tweets <- reactive({
    switch(input$sort_by,
           "Most recent"   = filtered_tweets() %>% arrange(desc(created_at)),
           "Most likes"    = filtered_tweets() %>% arrange(desc(favorite_count)),
           "Most retweets" = filtered_tweets() %>% arrange(desc(retweet_count)))
  })
  
  output$tweets_sorted_by <- reactive({paste("Tweets sorted by", tolower(input$sort_by))})
  
  dataset_info <- reactive({
    datasets %>%
      filter(dataset_name == input$filter_dataset) %>%
      transpose() %>%
      extract2(1)
  })
  
  output$dataset_name <- renderText({dataset_info()$dataset_name})
  
  output$dataset_files <- renderUI({
    tagList(lapply(unlist(str_split(dataset_info()$dataset_files, ",")), 
                   function(x) shiny::a(paste("💾", x), href = x, style = "cont-family:Roboto Mono;")))
  })
  
  output$dataset_articles <- renderUI({
    tagList(lapply(unlist(str_split(dataset_info()$dataset_article, ",")), 
                   function(x) shiny::a(paste("🗞", x), href = x, style = "font-family:Roboto Mono;")))
  })
  
  output$dataset_sources <- renderUI({
    tagList(lapply(unlist(str_split(dataset_info()$dataset_article, ",")), 
                   function(x) shiny::a(paste("📍", x), href = x, style = "font-family:Roboto Mono;")))
  })
  
  output$tweets <- renderUI({
    tagList(lapply(transpose(sorted_tweets()), embed_tweet), tags$script('twttr.widgets.load(document.getElementById("tweets"));'))
  })
}


shinyApp(ui = ui, server = server)