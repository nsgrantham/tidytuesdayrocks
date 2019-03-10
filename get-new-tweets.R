library(tidyverse)
library(jsonlite)
library(lubridate)

# Find datetime from which to begin querying tweets
current_tweets <- read_tsv(file.path("data", "tweets.tsv"))

latest_datetime <- current_tweets %>%
  arrange(desc(created_at)) %>%
  pull(created_at) %>%
  first() %>%
  force_tz(tzone = "UTC")

from_date <- (latest_datetime - weeks(1)) %>%
  format(format = "%Y%m%d%H%M")

to_date <- min(latest_datetime + weeks(2), with_tz(now(), tzone = "UTC")) %>%
  format(format = "%Y%m%d%H%M")

# Query Twitter API and collect tweets

results <- list()
page_number <- 1
next_page_id <- NULL
more_pages <- TRUE
while (more_pages) {
  cat(paste("Getting page", page_number, "...\n"))
  post_data <- paste0("-d '{\"query\":\"#tidytuesday\",\"fromDate\":\"", from_date, "\",\"toDate\":\"", to_date, "\"")
  if (!is.null(next_page_id)) {
    post_data <- paste0(post_data, ", \"next\":\"", next_page_id, "\"")
  }
  post_data <- paste0(post_data, "}' ")
  raw_response <- system(
    paste0("curl -X POST \"https://api.twitter.com/1.1/tweets/search/fullarchive/tidytuesdayrocks.json\" ",
           post_data, "-H \"Authorization: Bearer ", Sys.getenv("TWITTER_BEARER_TOKEN"), "\""),
    intern = TRUE
  )
  parsed_response <- parse_json(raw_response, simplifyVector = TRUE)
  results[[page_number]] <- parsed_response$results
  if ("next" %in% names(parsed_response)) {
    next_page_id <- parsed_response[["next"]]
    page_number <- page_number + 1
  } else {
    cat("Done.")
    more_pages <- FALSE
  }
}

# Tidy up the tweets

returned_tweets <- map_dfr(results, function(df) {
    df %>%
      select(created_at, status_id = id_str, favorite_count, retweet_count) %>%
      bind_cols(select(df$user, screen_name)) %>%
      bind_cols(select(df$retweeted_status, id))
  }) %>%
  mutate(is_retweet = !is.na(id)) %>%
  select(-id) %>%
  filter(!is_retweet) %>%
  mutate(status_url = paste0("https://twitter.com/", screen_name, "/status/", status_id),
         created_at = parse_date_time(str_replace(created_at, "\\+0000", ""), "abdHMSY"),
         created_at = force_tz(created_at, tzone = "UTC")) %>%
  select(status_url, screen_name, created_at, favorite_count, retweet_count)

# Combine returned_tweets with current_tweets, and update favorite_count and 
# retweet_count for tweets in both current_tweets and returned_tweets

returned_tweets %>%
  left_join(select(current_tweets, status_url, dataset_id), by = "status_url") %>%
  bind_rows(filter(current_tweets, !(status_url %in% returned_tweets$status_url))) %>%
  write_tsv(file.path("data", "tweets.tsv"))

