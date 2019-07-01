library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(magrittr)

datasets <- read_tsv(file.path("data", "datasets.tsv"))

tweets <- read_tsv(file.path("data", "tweets.tsv")) %>%
  filter(dataset_id != "x")

ui <- fluidPage(
  tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
  tags$head(HTML('<style>* {font-size: 100%; font-family: Roboto Mono;}</style>')),
  tags$head(HTML('<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')),
  fluidRow(
    column(1),
    column(4,
      h2("tidytuesday.rocks"),
      HTML(paste("<p><a href='https://github.com/rfordatascience/tidytuesday'>Tidy Tuesday</a>",
                 "is a weekly social data project in <a href='https://www.r-project.org/'>R</a>.",
                 "Every week <a href='https://twitter.com/thomas_mock'>@thomas_mock</a> and",
                 "<a href='https://twitter.com/R4DSCommunity'>@R4DSCommunity</a> post a new dataset",
                 "and ask R users to explore it and share their findings on Twitter with",
                 "<a href='https://twitter.com/search?src=typd&q=%23tidytuesday'>#TidyTuesday</a>.</p>")),
      HTML(paste("<p>Since the first dataset was posted on April 2nd, 2018, there are now over",
                 "60 datasets and 2,187 #TidyTuesday tweets from 524 users! Use the options",
                 "below to filter the tweets by dataset and sort them by date, likes, and retweets.</p>")),
      HTML(paste("<p>tidytuesday.rocks is only about 100 lines of R code and relies on your #TidyTuesday",
                 "tweets, which I scrape and manually label once every couple weeks. ",
                 "It is built with <a href='https://shiny.rstudio.com/'>Shiny</a> and <a href='https://rtweet.info/'>rtweet</a>",
                 "and its source code is <a href='https://github.com/nsgrantham/tidytuesdayrocks'>on GitHub</a>.</p>")),
      HTML(paste("<p>The response to tidytuesday.rocks has been amazing! It was even awarded ", 
                 "<a href='https://blog.rstudio.com/2019/04/05/first-shiny-contest-winners/'>a runner up spot in the 1st Shiny Contest</a>",
                 "among 136 submissions. \U0001F57A</p>")),
      HTML("<p>I'd love to hear your feedback, say hi <a href='https://twitter.com/nsgrantham'>@nsgrantham</a>.</p>"),
      p("Happy plotting!"),
      p(selectInput('dataset_name', 'Choose a dataset', rev(datasets$dataset_name),
                    selected = rev(datasets$dataset_name)[1]),
        selectInput('sort_by', 'Sort tweets', c("Most recent", "Most likes", "Most retweets"), 
                    selected = base::sample(c("Most recent", "Most likes", "Most retweets"), 1)))
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
  tags$blockquote(class = "twitter-tweet", tags$a(href = tweet$status_url))
}

make_links <- function(urls, text, icon = NULL) {
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
  
  chosen_dataset <- reactive({
    datasets %>%
      filter(dataset_name == input$dataset_name) %>%
      transpose() %>%
      extract2(1)
  })
  
  filtered_tweets <- reactive({
    tweets %>%
      filter(dataset_id == chosen_dataset()$dataset_id) %>%
      select(status_url, created_at, favorite_count, retweet_count)
  })
  
  sorted_tweets <- reactive({
    switch(input$sort_by,
           "Most recent"   = filtered_tweets() %>% arrange(desc(created_at)),
           "Most likes"    = filtered_tweets() %>% arrange(desc(favorite_count)),
           "Most retweets" = filtered_tweets() %>% arrange(desc(retweet_count)))
  })
  
  output$tweets_sorted_by <- reactive({paste("Tweets sorted by", tolower(input$sort_by))})
  
  output$dataset_name <- renderText({chosen_dataset()$dataset_name})
  
  output$dataset_links <- renderUI({
    tagList(make_links(chosen_dataset()$dataset_files, "Data", "\U0001F4BE"),        # floppy disk
            make_links(chosen_dataset()$dataset_articles, "Article", "\U0001F5DE"),  # rolled up newspaper
            make_links(chosen_dataset()$dataset_sources, "Source", "\U0001F4CD"))    # red pin
  })
  
  output$tweets <- renderUI({
    tagList(map(transpose(sorted_tweets()), embed_tweet), 
            tags$script('twttr.widgets.load(document.getElementById("tweets"));'))
  })
}


shinyApp(ui, server)
