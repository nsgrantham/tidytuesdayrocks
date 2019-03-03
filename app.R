library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(magrittr)

tidytuesday_tweets <- readRDS("data/tidytuesday_tweets.rds")

datasets <- read_tsv("data/datasets.tsv")

tweet_tags <- read_tsv("data/tweets.tsv", col_types = list(status_id = col_character())) %>%
  select(status_id, dataset_id) %>%
  left_join(datasets, by = "dataset_id")

tidytuesday_tweets <- tidytuesday_tweets %>%
  left_join(tweet_tags, by = "status_id") %>%
  filter(dataset_id != "x")

ui <- fluidPage(
  tags$head(HTML('<style>* {font-size: 100%; font-family: Roboto Mono;}</style>')),
  tags$head(HTML('<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')),
  fluidRow(
    column(1),
    column(4,
      h2("tidytuesday.rocks"),
      HTML("<p><a href='https://github.com/rfordatascience/tidytuesday'>Tidy Tuesday</a> is a weekly social data project in <a href='https://www.r-project.org/'>R</a>. Every week <a href='https://twitter.com/thomas_mock'>@thomas_mock</a> and <a href='https://twitter.com/R4DSCommunity'>@R4DSCommunity</a> post a new dataset and ask users to explore it and share their findings on Twitter with <a href='https://twitter.com/search?src=typd&q=%23tidytuesday'>#TidyTuesday</a>.</p>"),
      HTML("<p>Since the first dataset was posted on April 2nd, 2018, there are now over 40 datasets and more than 800 #TidyTuesday tweets from 221 users! Use the options below to filter the tweets by dataset and sort them by date, likes, and retweets.</p>"),
      HTML("<p>I built tidytuesday.rocks with <a href='https://shiny.rstudio.com/'>Shiny</a> using tweets collected at the end of 2018 with <a href='https://rtweet.info/'>rtweet</a> (tweets from 2019 are coming soon). You can find the source code <a href='https://github.com/nsgrantham/tidytuesdayrocks'>on GitHub</a>, where issues and PRs are welcome. I'd love to hear your feedback, say hi <a href='https://twitter.com/nsgrantham'>@nsgrantham</a>.</p>"),
      HTML("<p>Happy plotting!</p>"),
      p(selectInput('dataset_name', 'Choose a dataset', datasets$dataset_name, selected = datasets$dataset_name[sample.int(nrow(datasets), size = 1)]),
        selectInput('sort_by', 'Sort tweets', c("Most recent", "Most likes", "Most retweets"), selected = "Most recent"))
      ),
    column(6,
      h2(textOutput('dataset_name')),
      p(uiOutput('dataset_links')), 
      h3(textOutput('tweets_sorted_by')),
      uiOutput('tweets')
    ),
    column(1)
  )
)

embed_tweet <- function(tweet) {
  tags$blockquote(class = "twitter-tweet", tags$a(href = paste0("https://twitter.com/", tweet$screen_name, "/status/", tweet$status_id)))
}

make_links <- function(urls, text, icon = "") {
  if (is.na(urls)) return("")
  split_urls <- unlist(str_split(urls, ","))
  if (length(split_urls) > 1) {
    text <- paste(text, 1:length(split_urls))
  }
  names(split_urls) <- text 
  links <- imap(split_urls, ~ shiny::a(.y, href = .x))
  c(icon, links)
}

server <- function(input, output, session) {
  
  filtered_tweets <- reactive({
    tidytuesday_tweets %>%
      filter(dataset_name == input$dataset_name) %>%
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
      filter(dataset_name == input$dataset_name) %>%
      transpose() %>%
      extract2(1)
  })
  
  output$dataset_name <- renderText({dataset_info()$dataset_name})
  
  output$dataset_links <- renderUI({
    tagList(make_links(dataset_info()$dataset_files, "Data", "💾"),
            make_links(dataset_info()$dataset_articles, "Article", "🗞"),
            make_links(dataset_info()$dataset_sources, "Source", "📍"))
  })
  
  output$tweets <- renderUI({
    tagList(lapply(transpose(sorted_tweets()), embed_tweet), tags$script('twttr.widgets.load(document.getElementById("tweets"));'))
  })
}


shinyApp(ui, server)
