# Data Clean

library(rio)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

clean <- function(d) {
  d <- d %>%
    mutate(after_id = strsplit(response_tweet_id, split = ","),
           after_id = map(after_id, as.numeric),
           after_id = map_if(after_id, ~length(.) == 0, ~NA_integer_),
           was_missing = FALSE) %>% 
    complete(tweet_id = full_seq(tweet_id, 1),
             fill = list(after_id = NA_integer_,
                         inbound = TRUE,
                         text = NA_character_,
                         was_missing = TRUE)) %>% 
    mutate(is_responded_to = tweet_id %in% in_response_to_tweet_id,
           no_previous = is.na(in_response_to_tweet_id),
           is_start = is_responded_to & (no_previous | was_missing))
  for (i in seq_along(d$tweet_id)) {
    if (d$was_missing[i] & d$is_responded_to[i] == TRUE) {
      d$after_id[i] <- which(d$in_response_to_tweet_id %in% d$tweet_id[i])
    }
  }
    d <- d %>% arrange(tweet_id) %>%
      select(tweet_id, after_id, is_start, author_id, inbound, text)
    
  return(d)
}

d <- import("../data/twcs.csv")
d <- clean(d)
export(d, "../data_clean/twcs_clean.rda")