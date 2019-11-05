library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(magrittr)

datasets <- read_tsv(file.path("data", "datasets.tsv"))

tweets <- read_tsv(file.path("data", "tweets.tsv")) %>%
  filter(dataset_id != "x")

users <- tweets %>%
  pull(screen_name) %>%
  unique() %>%
  sort()

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
      HTML(paste("<p>Since the first dataset was posted on April 2nd, 2018, there are now",
                 "84 datasets and 3,069 #TidyTuesday tweets from 700 users! Use the options",
                 "below to filter the tweets by dataset or Twitter user and sort them by date, likes, and retweets.</p>")),
      HTML(paste("<p>tidytuesday.rocks is about 150 lines of R code and relies on your #TidyTuesday",
                 "tweets, which I scrape and manually label every few weeks. ",
                 "It is built with <a href='https://shiny.rstudio.com/'>Shiny</a> and <a href='https://rtweet.info/'>rtweet</a>",
                 "and its source code is <a href='https://github.com/nsgrantham/tidytuesdayrocks'>on GitHub</a>.</p>")),
      HTML(paste("<p>The response to tidytuesday.rocks has been amazing! It was even awarded ", 
                 "<a href='https://blog.rstudio.com/2019/04/05/first-shiny-contest-winners/'>a runner up spot in the 1st Shiny Contest</a>",
                 "among 136 submissions. \U0001F57A</p>")),
      HTML("<p>I'd love to hear your feedback, say hi <a href='https://twitter.com/nsgrantham'>@nsgrantham</a>.</p>"),
      p("Happy plotting!"),
      HTML("<p>(P.S. you may need to disable the DuckDuckGo Privacy Essentials browser extension for this website because it appears to block the JavaScript that embeds the tweets. If anybody knows a fix to this, please <a href='https://github.com/nsgrantham/tidytuesdayrocks/issues'>open an issue</a>!)</p>"),
      br(),
      tabsetPanel(id = "selected_tab", type = "tabs", selected = "dataset",
          tabPanel("Filter by Dataset", value = "dataset",
            br(),
            selectInput('dataset_name', 'Choose a dataset', rev(datasets$dataset_name), 
                        selected = rev(datasets$dataset_name)[1]),
            selectInput('dataset_sort_by', 'Sort tweets', c("Most recent", "Most likes", "Most retweets"), 
                        selected = base::sample(c("Most recent", "Most likes", "Most retweets"), 1))),
          tabPanel("Filter by User", value = "user",
            br(),
            selectizeInput("user_name", "Choose a user", users, selected = sample(users, 1)),
            selectInput('user_sort_by', 'Sort tweets', c("Most recent", "Most likes", "Most retweets"), 
                        selected = "Most recent")))
    ),
    column(6,
      conditionalPanel(
        condition = "input.selected_tab == 'dataset'",
        h2(textOutput('dataset_name')),
        p(uiOutput('dataset_links')), 
        h3(textOutput('dataset_tweets_sorted_by')),
        uiOutput('embedded_dataset_tweets')
      ),
      conditionalPanel(
        condition = "input.selected_tab == 'user'",
        h2(textOutput('user_name')),
        p(uiOutput('user_links')), 
        h3(textOutput('user_tweets_sorted_by')),
        uiOutput('embedded_user_tweets')
      )
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
  
  dataset_tweets <- reactive({
    tweets %>%
      filter(dataset_id == chosen_dataset()$dataset_id) %>%
      select(status_url, created_at, favorite_count, retweet_count)
  })
  
  sorted_dataset_tweets <- reactive({
    switch(input$dataset_sort_by,
           "Most recent"   = dataset_tweets() %>% arrange(desc(created_at)),
           "Most likes"    = dataset_tweets() %>% arrange(desc(favorite_count)),
           "Most retweets" = dataset_tweets() %>% arrange(desc(retweet_count)))
  })
  
  output$dataset_tweets_sorted_by <- reactive({paste("Tweets sorted by", tolower(input$dataset_sort_by))})
  
  output$dataset_name <- renderText({chosen_dataset()$dataset_name})
  
  output$dataset_links <- renderUI({
    tagList(make_links(chosen_dataset()$dataset_files, "Data", "\U0001F4BE"),        # floppy disk
            make_links(chosen_dataset()$dataset_articles, "Article", "\U0001F5DE"),  # rolled up newspaper
            make_links(chosen_dataset()$dataset_sources, "Source", "\U0001F4CD"))    # red pin
  })
  
  output$embedded_dataset_tweets <- renderUI({
    tagList(map(transpose(sorted_dataset_tweets()), embed_tweet), 
            tags$script('twttr.widgets.load(document.getElementById("tweets"));'))
  })
  
  user_tweets <- reactive({
    tweets %>%
      filter(screen_name == input$user_name) %>%
      select(status_url, created_at, favorite_count, retweet_count)
  })
  
  sorted_user_tweets <- reactive({
    switch(input$user_sort_by,
           "Most recent"   = user_tweets() %>% arrange(desc(created_at)),
           "Most likes"    = user_tweets() %>% arrange(desc(favorite_count)),
           "Most retweets" = user_tweets() %>% arrange(desc(retweet_count)))
  })
  
  output$user_tweets_sorted_by <- reactive({paste("Tweets sorted by", tolower(input$user_sort_by))})
 
  output$user_name <- renderText({input$user_name})
  
  output$user_links <- renderUI({
    tagList(make_links(paste0("https://twitter.com/", input$user_name), "Twitter", "\U0001F4AC"))  # speech bubble
  })
  
  output$embedded_user_tweets <- renderUI({
    tagList(map(transpose(sorted_user_tweets()), embed_tweet), 
            tags$script('twttr.widgets.load(document.getElementById("tweets"));'))
  }) 
}


shinyApp(ui, server)
