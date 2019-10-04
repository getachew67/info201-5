# A4 Data Wrangling

# Loading and Exploring Data -------------------------------- (**28 points**)

# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package
library("dplyr")

# Load your data, making sure to not interpret strings as factors
ks_df <- read.csv("data/ks-projects-201801.csv",
  stringsAsFactors = FALSE
)

# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
# - How many rows is the data frame?
# - How many columns are in the data frame?
colnames(ks_df)
nrow(ks_df)
ncol(ks_df)
# Use the `summary` function to get some summary information
summary(ks_df)

# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
# column name and a dataframe. If the values in the column are of type *double*,
# the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* double and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* double and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type
get_col_info <- function(col_name, ks_df) {
  if (typeof(ks_df[[col_name]]) == "double") {
    data <- list(
      min = min(select(ks_df, col_name)),
      max = max(select(ks_df, col_name)),
      mean = mean(ks_df[[col_name]])
    )
  } else if (typeof(ks_df[[col_name]]) != "double" &
    n_distinct(ks_df[[col_name]]) < 10) {
    unique_val <- list(
      n_values = n_distinct(select(ks_df, col_name), na.rm = F),
      unique_values = unique(select(ks_df, col_name))
    )
  } else {
    unique_val <- list(
      n_values = n_distinct(select(ks_df, col_name), na.rm = F),
      sample_values = sample_n(ks_df, 10, replace = F)
    )
  }
}

# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name
col_info <- get_col_info("goal", ks_df)

# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop
get_summary_info <- function(ks_df) {
  sapply(colnames(ks_df), get_col_info, USE.NAMES = T, ks_df)
}

# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable
summary <- get_summary_info(ks_df)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# YOUR COMMENTS HERE
# LIKELY ON MULTIPLE LINES

# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!

# What was the name of the project(s) with the highest goal?
highest_goal <- ks_df %>%
  filter(goal == max(goal)) %>%
  select(name) %>%
  pull(name)

# What was the category of the project(s) with the lowest goal?
lowest_goal <- ks_df %>%
  filter(goal == min(goal)) %>%
  select(category) %>%
  pull(category)

# How many projects had a deadline in 2018?
deadline_2018 <- ks_df %>%
  filter(between(
    as.Date(deadline), as.Date("2018-1-1"),
    as.Date("2018-12-31")
  )) %>%
  nrow()

# What proportion or projects weren't successful? Your result can be a decimal
was_successful <- ks_df %>%
  filter(state == "successful") %>%
  nrow() / nrow(ks_df)

# What was the amount pledged for the project with the most backers?
amount_most_backers <- ks_df %>%
  filter(backers == max(backers)) %>%
  select(usd_pledged_real) %>%
  pull(usd_pledged_real)

# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?
failed_with_most_pledged <- ks_df %>%
  filter(state == "failed") %>%
  filter(usd_pledged_real == max(usd_pledged_real)) %>%
  pull(usd_pledged_real)

# How much total money was pledged to projects that weren't successful?
total_donated_not_successful <- ks_df %>%
  filter(state != "successful") %>%
  select(usd_pledged_real) %>%
  sum()

# Write (and answer) two meaningful questions of the data that can be answered
# using similar operations (`filter`, `pull`, `summarize`, `mutate`, etc.).

# What was the minimum pledged with a successful project
minimum_with_success <- ks_df %>%
  filter(state == "successful") %>%
  filter(usd_pledged_real == min(usd_pledged_real)) %>%
  pull(usd_pledged_real)

# What was the name of the project with the most backers
project_name_most_backers <- ks_df %>%
  filter(backers == max(backers)) %>%
  pull(name)

# Performing analysis by *grouped* observations ----------------- (38 Points)

# Which category had the most money pledged (total)?
most_pledged <- ks_df %>%
  group_by(category) %>%
  summarise(usd_pledged_real = max(usd_pledged_real)) %>%
  filter(usd_pledged_real == max(usd_pledged_real)) %>%
  pull(category)

# Which country had the most backers?
country_w_most_backers <- ks_df %>%
  group_by(country) %>%
  summarise(backers = sum(backers)) %>%
  filter(backers == max(backers)) %>%
  pull(country)

# Which year had the most money pledged (hint: you may have to create a new
# column)?
year_most_pledged <- ks_df %>%
  mutate(year = substr(ks_df$deadline, 1, 4)) %>%
  group_by(year) %>%
  summarise(usd_pledged_real = sum(usd_pledged_real)) %>%
  filter(usd_pledged_real == max(usd_pledged_real)) %>%
  pull(year)

# What were the top 3 main categories in 2018 (as ranked by number of backers)?
top_3_categories <- ks_df %>%
  mutate(year = substr(ks_df$deadline, 1, 4)) %>%
  filter(year == "2018") %>%
  arrange(-backers) %>%
  select(category) %>%
  head(3)

# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)
common_day <- ks_df %>%
  mutate(days = weekdays(as.Date(substr(ks_df$launched, 1, 10)))) %>%
  summarise(days = max(days)) %>%
  pull(days)


# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were successful)? This might require some creative problem solving....
least_successful_day <- ks_df %>%
  mutate(days = weekdays(as.Date(substr(ks_df$launched, 1, 10)))) %>%
  group_by(days)



# Write (and answer) two meaningful questions of the data that can be answered
# by _grouping_ the data and performing summary calculations.

# What game(s) got the least amount of money pledged in 2018 in the US
least_successful_game <- ks_df %>%
  mutate(year = substr(ks_df$deadline, 1, 4)) %>%
  filter(year == "2018" & country == "US" & category == "Games") %>%
  arrange(usd_pledged_real) %>%
  pull(name)

# What is the name of the most pledged project in 2017
most_pledged_2017 <- ks_df %>%
  mutate(year = substr(ks_df$deadline, 1, 4)) %>%
  filter(year == "2017") %>%
  filter(usd_pledged_real == max(usd_pledged_real)) %>%
  pull(name)
