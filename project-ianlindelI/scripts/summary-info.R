# A function that takes in a dataset and returns a list of info about it:

setwd("~/project-ianlindelI")

get_summary_info <- function(dataset) {
  ret <- list()
  ret$length <- length(dataset)
  ret$rows <- nrow(dataset)
  ret$col <- ncol(dataset)
  return(ret)
} 
