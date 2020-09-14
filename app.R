library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(magrittr)
library(reactable)

datasets <- read_tsv(file.path("data", "datasets.tsv"))

tweets <- read_tsv(file.path("data", "tweets.tsv")) %>%
  filter(dataset_id != "x")

users <- tweets %>%
  pull(screen_name) %>%
  unique() %>%
  sort()

ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Roboto+Mono", rel = "stylesheet"),
    tags$style(HTML('
      * {
        font-family: Roboto Mono;
        font-size: 100%;
      }
      #sidebar {
         background-color: #fff;
         border: 0px;
      }
      .rt-th {
        display: none;
      }
      .rt-noData {
        display: none;
      }
    '))
  ),
  
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      tags$h1("tidytuesday.rocks"),
      tags$br(),
      HTML(paste("<p><a href='https://github.com/rfordatascience/tidytuesday'>Tidy Tuesday</a>",
                 "is a weekly social data project in <a href='https://www.r-project.org/'>R</a>.",
                 "Every week <a href='https://twitter.com/thomas_mock'>@thomas_mock</a> and",
                 "<a href='https://twitter.com/R4DSCommunity'>@R4DSCommunity</a> post a new dataset",
                 "and ask R users to explore it and share their findings on Twitter with",
                 "<a href='https://twitter.com/search?src=typd&q=%23tidytuesday'>#TidyTuesday</a>.</p>")),
      HTML(paste("<p>Since the first dataset was posted on April 2nd, 2018, there are now",
                 "111 datasets and 4,613 #TidyTuesday tweets from 1,027 users! Use the options",
                 "below to filter the tweets by dataset or Twitter user and sort them by date, likes, and retweets.</p>")),
      #HTML(paste("<p>tidytuesday.rocks is about 150 lines of R code and relies on your #TidyTuesday",
      #            "tweets, which I scrape and label every few weeks. ",
      #            "It is built with <a href='https://shiny.rstudio.com/'>Shiny</a> and ",
      #            "and its source code is <a href='https://github.com/nsgrantham/tidytuesdayrocks'>on GitHub</a>.</p>")),
      # HTML(paste("<p>The response to tidytuesday.rocks has been amazing! It was even awarded ", 
      #            "<a href='https://blog.rstudio.com/2019/04/05/first-shiny-contest-winners/'>a runner up spot in the 2019 Shiny Contest</a>",
      #            "among 136 submissions. \U0001F57A</p>")),
      HTML("<p> <a href='https://twitter.com/nsgrantham'>Follow me on Twitter</a> and <a href='https://github.com/nsgrantham/tidytuesdayrocks'>view the source code on GitHub</a>.</p>"),
      tags$p("Happy plotting!"),
      tags$br(),
      tabsetPanel(
        id = "selected_tab",
        type = "tabs",
        selected = "dataset",
        tabPanel("Dataset tweets", value = "dataset",
          tags$br(),
          selectInput("dataset_name", NULL, rev(datasets$dataset_name), selected = rev(datasets$dataset_name)[1]),
          selectInput("dataset_sort_by", NULL, c("Sorted by most likes", "Sorted by most retweets", "Sorted by most recent"), selected = "Sorted by most likes"),
          tags$br(),
          uiOutput("dataset_links") 
        ),
        tabPanel("User tweets", value = "user",
          tags$br(),
          selectizeInput("user_name", NULL, users, selected = sample(users, 1)),
          selectInput("user_sort_by", NULL, c("Sorted by most likes", "Sorted by most retweets", "Sorted by most recent"), selected = "Sorted by most likes"),
          tags$br(),
          uiOutput("user_links") 
        )
      ),
      width = 5
    ),
    mainPanel(
      tags$br(),
      conditionalPanel(
        condition = "input.selected_tab == 'dataset'",
        reactableOutput("embedded_dataset_tweets_table")
      ),
      conditionalPanel(
        condition = "input.selected_tab == 'user'",
        reactableOutput("embedded_user_tweets_table")
      ),
      width = 7
    )
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
      select(screen_name, status_url, created_at, favorite_count, retweet_count)
  })
  
  sorted_dataset_tweets <- reactive({
    switch(
      input$dataset_sort_by,
      "Sorted by most recent" = arrange(dataset_tweets(), desc(created_at)),
      "Sorted by most likes" = arrange(dataset_tweets(), desc(favorite_count)),
      "Sorted by most retweets" = arrange(dataset_tweets(), desc(retweet_count))
    )
  })
  
  output$dataset_links <- renderUI({
    tagList(
      make_links(chosen_dataset()$dataset_files, "Data", "\U0001F4BE"),        # floppy disk
      make_links(chosen_dataset()$dataset_articles, "Article", "\U0001F5DE"),  # rolled up newspaper
      make_links(chosen_dataset()$dataset_sources, "Source", "\U0001F4CD")     # red pin
    )    
  })
  
  output$embedded_dataset_tweets_table <- renderReactable({
    reactable(
      transmute(sorted_dataset_tweets(), url_datetime = paste(status_url, created_at, sep = "|")),
      showPageInfo = FALSE,
      defaultPageSize = 1,
      sortable = FALSE,
      columns = list(
        url_datetime = colDef(
          cell = function(url_datetime) {
            split_url_datetime <- unlist(str_split(url_datetime, "\\|"))
            url <- URLencode(split_url_datetime[1], reserved = TRUE)
            datetime <- as.numeric(as.POSIXct(split_url_datetime[2]))
            tags$iframe(
              src = paste0("https://twitframe.com/show?url=", url, "&datetime=", datetime),
              border = 0,
              frameborder = 0,
              height = 600,
              width = 600
            )
          }
        )
      )
    )
  })
  
  user_tweets <- reactive({
    tweets %>%
      filter(screen_name == input$user_name) %>%
      select(status_url, created_at, favorite_count, retweet_count)
  })
  
  sorted_user_tweets <- reactive({
    switch(
      input$user_sort_by,
      "Sorted by most recent" = arrange(user_tweets(), desc(created_at)),
      "Sorted by most likes" = arrange(user_tweets(), desc(favorite_count)),
      "Sorted by most retweets" = arrange(user_tweets(), desc(retweet_count))
    )
  })
  
  output$user_links <- renderUI({
    tagList(make_links(paste0("https://twitter.com/", input$user_name), "Twitter", "\U0001F4AC"))  # speech bubble
  })
  
  output$embedded_user_tweets_table <- renderReactable({
    reactable(
      select(sorted_user_tweets(), status_url),
      showPageInfo = FALSE,
      defaultPageSize = 1,
      sortable = FALSE,
      columns = list(
        status_url = colDef(
          cell = function(value) {
            tags$iframe(
              border = 0,
              frameborder = 0,
              height = 600,
              width = 600,
              src = paste0(
                "https://twitframe.com/show?url=", str_replace_all(str_replace(value, ":", "%3A"), "/", "%2F")
              )
            )
          }
        )
      )
    )
  })
}


shinyApp(ui, server)
