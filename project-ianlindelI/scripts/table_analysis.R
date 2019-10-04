library(dplyr)
library(tidyr)
library(lintr)
setwd("~/project-ianlindelI")

school_data <- read.csv("data/specific_schools.csv", stringsAsFactors = F)
school_data <- school_data[1:3, 1:5]

summary_data <- school_data %>%
  mutate(hours_76 = cost_1976/farm_min_wage_1976,
         hours_14 = cost_2014/state_min_wage_2014,
         perweek_76 = hours_76/52,
         perweek_14 = hours_14/52,
         persummer_76 = hours_76/12,
         persummer_14 = hours_14/12
  )

working_table <- summary_data %>%
  select(university, cost_1976, cost_2014, hours_76, hours_14)

mean_week_76 <- round(mean(summary_data$perweek_76))
mean_week_14 <- round(mean(summary_data$perweek_14))
mean_summer_76 <- round(mean(summary_data$persummer_76))
mean_summer_14 <- round(mean(summary_data$persummer_14))
week_difference <- round(mean_week_14 - mean_week_76)
summer_difference <- round(mean_summer_14 - mean_summer_76)
