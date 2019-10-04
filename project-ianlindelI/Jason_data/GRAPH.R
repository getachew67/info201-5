library(plotly)


data <- data.frame(MyData)

Graph2 <- plot_ly(data, x = ~MyData$YEAR, y = MyData$Min_wage, name = 'MIN_WAGE'
             ,type = 'scatter', mode = 'lines+markers')%>%
  layout(xaxis = list(title = "YEAR"),
    yaxis = list(title = "MINIMUM WAGE IN $"))

Graph2


library(plotly)
library(gapminder)
MyData$DATE
p <- 
  plot_ly(
    x = MyData$YEAR, 
    y = MyData$Min_wage, 
    frame = ~YEAR,
    hoverinfo = ~MyData$DATE,
    type = 'scatter',
    size = 1,
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )
  )%>%
  layout(
    xaxis = list(title = "YEAR"),
    yaxis = list(title = "MINIMUM WAGE IN $")
  )
p




library(plotly)

df <- data.frame(
  x = c(1,2,1), 
  y = c(1,2,1), 
  f = c(1,2,3)
)

p <- df %>%
  plot_ly(
    x = ~x,
    y = ~y,
    frame = ~f,
    type = 'scatter',
    mode = 'markers',
    showlegend = F
  )
p
