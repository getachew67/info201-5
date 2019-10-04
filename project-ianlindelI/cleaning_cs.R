library(dplyr)
library(tidyr)

# original raw data saved on Natalia's local drive due to size of data issue

#loading data
raw_96 <- read.csv("data/MERGED1996_97_PP.csv", stringsAsFactors = F)
raw_97 <- read.csv("data/MERGED1997_98_PP.csv", stringsAsFactors = F)
raw_98 <- read.csv("data/MERGED1998_99_PP.csv", stringsAsFactors = F)
raw_99 <- read.csv("data/MERGED1999_00_PP.csv", stringsAsFactors = F)
raw_00 <- read.csv("data/MERGED2000_01_PP.csv", stringsAsFactors = F)
raw_01 <- read.csv("data/MERGED2001_02_PP.csv", stringsAsFactors = F)
raw_02 <- read.csv("data/MERGED2002_03_PP.csv", stringsAsFactors = F)
raw_03 <- read.csv("data/MERGED2003_04_PP.csv", stringsAsFactors = F)
raw_04 <- read.csv("data/MERGED2004_05_PP.csv", stringsAsFactors = F)
raw_05 <- read.csv("data/MERGED2005_06_PP.csv", stringsAsFactors = F)
raw_06 <- read.csv("data/MERGED2006_07_PP.csv", stringsAsFactors = F)
raw_07 <- read.csv("data/MERGED2007_08_PP.csv", stringsAsFactors = F)
raw_08 <- read.csv("data/MERGED2008_09_PP.csv", stringsAsFactors = F)
raw_09 <- read.csv("data/MERGED2009_10_PP.csv", stringsAsFactors = F)
raw_10 <- read.csv("data/MERGED2010_11_PP.csv", stringsAsFactors = F)
raw_11 <- read.csv("data/MERGED2011_12_PP.csv", stringsAsFactors = F)
raw_12 <- read.csv("data/MERGED2012_13_PP.csv", stringsAsFactors = F)
raw_13 <- read.csv("data/MERGED2013_14_PP.csv", stringsAsFactors = F)
raw_14 <- read.csv("data/MERGED2014_15_PP.csv", stringsAsFactors = F)

# cleaning data
clean_data <- function(df, year){
  df %>% mutate(start_year = year) %>%
    select(STABBR, UGDS, UGDS_WOMEN, UGDS_MEN, start_year)
}

data_96 <- clean_data(raw_96, 1996)
data_97 <- clean_data(raw_97, 1997)
data_98 <- clean_data(raw_96, 1999)
data_99 <- clean_data(raw_96, 1999)
data_00 <- clean_data(raw_96, 2000)
data_01 <- clean_data(raw_96, 2001)
data_02 <- clean_data(raw_96, 2002)
data_03 <- clean_data(raw_96, 2003)
data_04 <- clean_data(raw_96, 2004)
data_05 <- clean_data(raw_96, 2005)
data_06 <- clean_data(raw_96, 2006)
data_07 <- clean_data(raw_96, 2007)
data_08 <- clean_data(raw_96, 2008)
data_09 <- clean_data(raw_96, 2009)
data_10 <- clean_data(raw_96, 2010)
data_11 <- clean_data(raw_96, 2011)
data_12 <- clean_data(raw_96, 2012)
data_13 <- clean_data(raw_96, 2013)
data_14 <- clean_data(raw_96, 2014)

# combining data

combined_data <- rbind(data_96, data_97, data_98, data_99, data_00, data_01,
                           data_02, data_03, data_04, data_05, data_06, data_07,
                           data_08, data_09, data_10, data_11, data_12, data_13,
                           data_14)

# wrangling data
# UGDS_ASIAN, UGDS_WOMEN, UGDS_MEN, start_year

combined_data$UGDS <- as.numeric(combined_data$UGDS)
combined_data$UGDS_MEN <- as.numeric(combined_data$UGDS_MEN)
combined_data$UGDS_WOMEN <- as.numeric(combined_data$UGDS_WOMEN)

write.csv(combined_data, "data/combined-data.csv")
