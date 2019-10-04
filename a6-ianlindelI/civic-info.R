library(httr)
library(jsonlite)
library(dplyr)
library(kableExtra)

source("api-keys.R")

base_uri <- "https://www.googleapis.com/civicinfo/v2/"

resource <- "representatives"
google <- paste0(base_uri, resource)

query_param <- list(address = address, key = api_google)
response <- GET(google, query = query_param)
body <- content(response, "text")
results <- fromJSON(body)

# Data wrangling (assuming that `parsed_data` is the parsed JSON response)
offices <- results$offices
officials <- results$officials

# Expand officies by the number of elements in the `indices` column
# See: https://stackoverflow.com/questions/2894775/replicate-each-row-of-data-frame-and-specify-the-number-of-replications-for-each
# (Links to an external site.)Links to an external site.
num_to_rep <- unlist(lapply(results$offices$officialIndices, length))

expanded <- offices[rep(row.names(offices), num_to_rep), ]

officials <- officials %>% mutate(index = row_number() - 1)

expanded <- expanded %>%
  mutate(index = row_number() - 1) %>%
  rename(position = name)

# Then, join officials and offices by index....

complete <- merge(expanded, officials)

colnames(complete[c(7, 2, 9, 14, 10, 12)]) <- 
  c("Name", "Position", "Party", "Email", "Phone", "Photo")

complete$name <- paste0("[", complete$name, "](", complete$urls, ")")

complete$emails[complete$emails == "NULL"] <- "Not available"

table <- complete[c(7, 2, 9, 14, 10, 12)]
