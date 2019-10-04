student_debt <- read.csv("~/project-ianlindelI/data/balance_by_age.csv", stringsAsFactors = FALSE)
View(student_debt)

library(ggplot2)


debt_plot <- 
  ggplot(data = student_debt, mapping = aes(x = Year, y = Debt, group = Age, color = Age)) +
  geom_point() +
  geom_line() +
  ggtitle("Student Loan Debt by Age Group") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Year", y = "Debt in Billions of Dollars") +
  scale_color_discrete(name = "Age",
                       breaks = c("under30", "30_39", "40_49", "50_59", "60"),
                       labels = c("Under 30", "30-39", "40-49", "50-59", "60+"))




