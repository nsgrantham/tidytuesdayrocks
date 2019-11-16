library(tidyverse)
library(lubridate)

read_tsv(file.path("data", "tweets.tsv")) %>%
  filter(dataset_id != "x") %>%
  select(created_at) %>%
  transmute(date = round_date(as_date(created_at), "day")) %>%
  group_by(date) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(cumsum_n = cumsum(n)) %>%
  ggplot(aes(date, cumsum_n)) + 
    geom_line() +
    annotate("point", x = as_date("2018-04-02"), y = 0) +
    annotate("point", x = as_date("2019-02-02"), y = 1000) +
    annotate("point", x = as_date("2019-06-11"), y = 2000) +
    annotate("point", x = as_date("2019-10-28"), y = 3000) +
    annotate("text", x = as_date("2018-04-02"), y = 0, hjust = -0.1, vjust = 1.6, label = "Apr 02, 2018") +
    annotate("text", x = as_date("2019-02-02"), y = 1000, hjust = 1.1, vjust = -0.7, label = "Feb 02, 2019") +
    annotate("text", x = as_date("2019-06-11"), y = 2000, hjust = 1.1, vjust = -0.7, label = "Jun 11, 2019") +
    annotate("text", x = as_date("2019-10-28"), y = 3000, hjust = 1.1, vjust = -0.7, label = "Oct 28, 2019") +
    labs(title = "There are now over 3,000 Tidy Tuesday visualizations!", 
         x = NULL, y = NULL) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%b '%y", date_breaks  = "1 month") +
    theme_minimal(base_family = "Fira Sans Extra Condensed Light",
                  base_size = 14) +
    theme(plot.title = element_text(family = "Fira Sans Extra Condensed", size = 16),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("tidy-tuesday-3k.png", width = 7, height = 5.1, dpi = 300)

