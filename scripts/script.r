library(dplyr)
library(rio)
library(ggplot2)
library(purrr)
library(tidyr)

source("C:/Projects/r/10-28/scripts/data.r")
source("C:/Projects/r/10-28/scripts/conversation.r")

# draw <- import("../data/twcs.csv")
# dconv <- import("../data_clean/twcs_conv_unlisted.rda")
# d <- get_data()

# d2 <- import("../data_clean/twcs_conv_unlisted.rda")
# d2 <- import("../data_clean/twcs_clean.rda")

# Gets all the tweet ids that start the conversation
get_start_ids <- function(d) {
  start_ids <- d$tweet_id[d$is_start]
  return(start_ids)
}

# Returns character vector of the company names
get_company_names <- function(d) {
  names <- d %>% filter(inbound == F) %>% 
    pull(author_id) %>% 
    unique()
  
  return(names)
}

# start_ids <- get_start_ids(dd)
# print(length(start_ids))
# conv_list <- vector(mode = "list", length(start_ids))

# conv_list <- start_ids %>% map(Conversation.constructor3, dd)


# for (i in seq_along(start_ids)) {
#   conv_list[[i]] <- Conversation.constructor3(d, start_ids[i])
# }


# dr <- do.call(rbind, conv_list)
# dn <- dr %>%
#   mutate(conv_id = map(conv, 1)) %>%
#   unnest_longer(conv) %>%
#   rename(tweet_id = conv) %>%
#   select(tweet_id, conv_id, comp_id) %>%
#   arrange(tweet_id)
# 
# export(dn, "../data_clean/twcs_conv_unlisted.rda")

# d3 <- inner_join(d, d2)

d1 <- d %>% 
  filter(comp_id == "VirginAtlantic") %>%
  group_by(conv_id) %>% 
  summarize(conv_len = n()) %>%
  arrange(desc(conv_len))

d2 <- d %>% 
  group_by(comp_id, conv_id) %>% 
  summarize(conv_len = n()) %>% 
  ungroup(conv_id) %>% 
  summarize(conv_len_mean = mean(conv_len)) %>% 
  arrange(desc(conv_len_mean))
