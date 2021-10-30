# Data Read

library(rio)

get_data <- function() {
  d <- import("../data_clean/twcs_clean.rda")
  return(d)
}