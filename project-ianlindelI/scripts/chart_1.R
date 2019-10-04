library(plotly)
library(stringr)

fed_min_wage <- read.csv("~/project-ianlindelI/data/Fed_min_wage.csv")
fed_min_wage

library(plotly)


p <- plot_ly(
  fed_min_wage, x = ~(fed_min_wage$YEAR), y = ~(fed_min_wage$Min_wage),
  # Hover text:
  text = ~paste("Mininum Wage: $", fed_min_wage$Min_wage,'Year:', fed_min_wage$DATE),
  size = 20, 
  color = "Set1") %>%
    layout(title = 'FEDERAL MINIMUM WAGE 1938-2019',
    xaxis = list(title = 'Years', yaxis = list(title ='Brain Weight (g)',
                        zeroline = TRUE
                  
)
  )
)
    

p





