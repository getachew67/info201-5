library(ggplot2)
setwd("~/project-ianlindelI")

tuition <- read.csv("data/tuition-fees-by-state.csv", stringsAsFactors = FALSE)

tuition <- tuition[56:108, c(1, 20:36)]

tuition[1,1] <- "State"

colnames(tuition) <- tuition[1,]

tuition <- tuition[-1,]

tuition_chart <- ggplot(data = tuition) +
  geom_col(
    mapping = aes(x = state.abb[match(tuition[,1], state.name)], y = as.numeric(gsub("[\\$,]", "", tuition$`2018-19`)), fill = "Tuiton 2018-19")
  ) +
  geom_col(
    mapping = aes(x = state.abb[match(tuition[,1], state.name)], y = as.numeric(gsub("[\\$,]", "", tuition$`2004-05`)), fill = "Tuition 2004-05")
  ) +
  labs(
    title = "How Tuition has Changed by State from 2004-2019",
    x = "State",
    y = "Tuition in Dollars",
    fill = "Key"
  ) +
  scale_y_continuous(expand=c(0,0))
