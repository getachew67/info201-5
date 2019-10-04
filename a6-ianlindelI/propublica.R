library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

source("api-keys.R")

base_uri <- "https://api.propublica.org/congress/v1/"

resource <- "members"
endpoint <- paste0(base_uri, resource, "/house", "/", state, "/current.json")

response <- GET(endpoint, add_headers("X-API-KEY" = api_propublica))
body <- content(response, "text")
results <- fromJSON(body)

propublica_df <- flatten(results$results)

gender <- propublica_df %>%
  group_by(gender) %>%
  count(gender)

gender_plot <- ggplot(gender) +
  ggtitle("Representatives by gender") +
  geom_col(mapping = aes(x = gender, y = n)) +
  coord_flip() +
  ylab("Number of representatives")

party <- propublica_df %>%
  group_by(party) %>%
  count(party)

party_plot <- ggplot(party) +
  ggtitle("Representatives by party") +
  geom_col(mapping = aes(x = party, y = n)) +
  coord_flip() +
  ylab("Number of representatives")

person <- GET(
  "https://api.propublica.org/congress/v1/members/D000617.json",
  add_headers("X-API-KEY" = api_propublica)
)
info <- content(person, "text")
result <- fromJSON(info)

name <- paste0(result$results$first_name, " ", result$results$last_name)

age <- floor(as.numeric(difftime(
  Sys.Date(), result$results$date_of_birth,
  "years"
)) / 365.25)

twitter_link <- result$results$twitter_account

votes <-
  GET(
    "https://api.propublica.org/congress/v1/members/D000617/votes.json",
    add_headers("X-API-KEY" = api_propublica)
  )
vote <- content(votes, "text")
total_vote <- fromJSON(vote)

position <- total_vote[["results"]][["votes"]][[1]][["total"]]

position$position <- total_vote[["results"]][["votes"]][[1]][["position"]]

agree <- position %>%
  filter(yes > no & position == "Yes" | no > yes & position == "No") %>%
  nrow()
