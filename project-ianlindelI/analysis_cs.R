combined_data <- read.csv("data/combined-data.csv", stringsAsFactors = F)

summary_combined <- combined_data %>%
  group_by(STABBR, start_year) %>%
  summarise(total_ugds = count(UGDS),
            women = mean(UGDS_WOMEN, na.rm = T),
            men = mean(UGDS_MEN, na.rm = T)
  )
