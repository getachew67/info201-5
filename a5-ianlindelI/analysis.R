library("dplyr")

# creates a dataframe of the US shooting data from 2018
shooting <- read.csv("data/shootings-2018.csv",
  stringsAsFactors = F
)

# total shootings in US in 2018
how_many <- nrow(shooting)

# total lost in 2018
total_lost <- sum(shooting$num_killed)

# shooting with most injuries
most_injuries <- shooting %>%
  filter(num_injured == max(num_injured))

# how many shootings were in Washington
washington <- shooting %>%
  filter(state == "Washington") %>%
  nrow()

# shooting with most deaths
most_killed <- shooting %>%
  filter(num_killed == max(num_killed))

# Thousand Oaks California
cali <- shooting %>%
  filter(city == "Thousand Oaks")

# Sum of killed and injured per state
shooting_data_per_state <- aggregate(. ~ state,
  data = shooting[, c(2, 5:6)],
  sum
)

# Sum of killed and injured per city
shooting_data_per_city <- shooting %>%
  group_by(city, lat, long) %>%
  summarise(num_killed = sum(num_killed), num_injured = sum(num_injured))
