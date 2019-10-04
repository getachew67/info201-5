library(shiny)
library(dplyr)
library(ggplot2)
library("shiny")
library("ggplot2")
library("plotly")
library("tidyverse")


MyData <- read.csv(file="./data/Fed_min_wage.csv", header=TRUE, sep=",")
states_tuition <- read.csv(file="./data/current_tuiton_cleaned.csv",)
states_tuition
MyData
states_tuition
states_tuition <- states_tuition[!is.infinite(rowSums(states_tuition)),]
states_tuition
s1
#___________________________________________________________________________________________________________________
#map wrangle 
state_codes <- read.csv("./data/state_codes.csv", stringsAsFactors = FALSE)
states_tuition
choices <- colnames(states_tuition[3:19])
#___________________________________________________________________________________________________________________
 us_map <- ui <- fluidPage(
  mainPanel(
    selectInput("mapvar",
                label = "Select A Year to compare",
                choices = list(
                 "2004-5" = "X2004.05",
                  "2005-6" = "X2005.06",
                  "2006-7" = "X2006.07",
                  "2007-8" = "X2007.08",
                  "2008-9" = "X2008.09",
                  "2009-10" = "X2009.10",
                  "2010-11" = "X2010.11",
                  "2011-12" = "X2011.12",
                  "2012-13" = "X2012.13",
                  "2013-14" = "X2013.14",
                  "2014-15" = "X2014.15",
                  "2015-16" = "X2015.16",
                  "2016-17" = "X2016.17",
                  "2017-18" = "X2017.18",
                  "2018-19" = "X2018.19"
                )
    ),
    
    plotlyOutput("map")
  )
)
#___________________________________________________________________________________________________________________
us_map_color <- ui <- fluidPage(
  mainPanel(
    selectInput(
      "color_s",
      label = "Color",
      choices = list(
        "Red" = "Reds", "Blue" = "Blues", "Green" = "Greens",
        "!!!!Purple!!!" = "Purples"
      ),
      plotlyOutput("map")
    )
  )
)
#___________________________________________________________________________________________________________________
states_tuition$hover <- with(states_tuition, paste(state))
build_map <- function(data, map_var, color_s) {
  g <- list(
    scope = "usa",
    projection = list(type = "albers usa"),
    showlakes = TRUE,
    lakecolor = toRGB("white")
  )
  
  # Plot
  p <- plot_geo(data, locationmode = "USA-states") %>%
    add_trace(
      z = data[, map_var], text = ~state, locations = ~code,
      color = data[, map_var], colors = color_s
    ) %>%
    colorbar(title = "Color Title") %>%
    layout(
      title = str_to_title(map_var),
      geo = g
    )
  return(p)
}
#___________________________________________________________________________________________________________________
min_report <- function(input){
  my_datas <- MyData %>% filter(Min_wage , YEAR == input) %>% select(Min_wage)
  max(my_datas)
}

highest_total_tuition <- function(input){
  states_tuition$X2004
}
highest_total_tuition(X2004)


#___________________________________________________________________________________________________________________
Graph2 <- plot_ly(MyData, x = ~MyData$YEAR, y = MyData$Min_wage, name = 'MIN_WAGE'
                  ,type = 'scatter', mode = 'lines+markers')%>%
  layout(xaxis = list(title = "YEAR"),
         yaxis = list(title = "MINIMUM WAGE IN $"))
#___________________________________________________________________________________________________________________
graph <- MyData %>% 
  plot_ly(
    x = ~YEAR, 
    y = ~Min_wage, 
    frame = ~YEAR,
    hoverinfo = ~DATE,
    type = 'scatter',
    size = 1,
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    ))%>%
  layout(
    xaxis = list(title = "YEAR"),
    yaxis = list(title = "MINIMUM WAGE IN $")%>%
      add_trace(y = MyData$Min_wage, name = 'trace 2', mode = 'lines')
  )
#___________________________________________________________________________________________________________________
year_inputs <- selectInput(
  "first_year",
  label = "Select a year to see the minimum wage of that year.",
  choices = MyData$YEAR,
  selected = "2019")
#___________________________________________________________________________________________________________________
ui <- fluidPage(
  titlePanel("FEDERAL MINIMUM WAGE AND TUITION RATES OVER TIME"),
  sidebarLayout(
      year_inputs,
    mainPanel(
      textOutput("selected_var"),
      graph,
      Graph2,
      us_map,
      us_map_color,
      textOutput("selected_vars")
    )
  )
)


#___________________________________________________________________________________________________________________
server <- function(input, output) {
  output$selected_var <- renderText({ 
    paste("In", input$first_year,"the minimum wage was $", min_report(input$first_year),"an hour")
  })
  output$map <- renderPlotly({
    return(build_map(states_tuition, input$mapvar, input$color_s))
  })
  output$selected_vars <- renderText({ 
    paste("In", input$mapvar,"the highest tuition for the year was,",get_col_max(input$mapvar), "from the state of .")
  })
}
#___________________________________________________________________________________________________________________
shinyApp(ui = ui, server = server)

dates <- colnames(states_tuition[3:16])
strDates <- as.character(dates)
strDates


states_tuition <- read.csv(
    file = "./data/current_tuiton_cleaned.csv",
  header = TRUE, sep = ",", stringsAsFactors = FALSE
)
states_tuition
# Write a function `get_col_mean()` that takes in a column name and a data frame
# and returns the mean of that column. Make sure to properly handle NA values

get_col_max <- function(col_name) {
  return(max(states_tuition[, col_name],
              na.rm = TRUE
  ))
}




return_max_state <- function(col_name) {
  my_return <- return(max(states_tuition[, col_name], na.rm = TRUE))

}
return_max_state("X2006.07")


return_max_state_two <- function(col_name) {
  column_names
  find_max_state <- states_tuition %>% group_by(column_names) %>% select(state,column_names) %>% arrange(-column_names)
  # find_max_state
  # find_max_state[1:2,2:1]
}

return_max_state_two(2004.05)

find_max_state <- states_tuition %>% group_by(X2004.05) %>% select(state,X2004.05) %>% arrange(-X2004.05)
find_max_state
find_max_state[1:2,2:1]


