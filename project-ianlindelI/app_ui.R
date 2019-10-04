library(shiny)
library(tidyr)
library(dplyr)
library(plotly)
library(ggplot2)
library("tidyverse")
#setwd("~/project-ianlindelI")

# DATA WRANGLING -----------------------------------------------------------
debt_data <- read.csv("data/balance_by_age.csv")
debt_age <- spread(debt_data, key = Age, value = Debt)
names(debt_age) <- c("Year", "Ages_30_to_39", "Ages_40_to_49", "Ages_50_to_59",
                     "Age_60", "Ages_Under_30")
# JASON DATA WRANGLING -----------------------------------------------------------
MyData <- read.csv(file="./data/Fed_min_wage.csv", header=TRUE, sep=",")
states_tuition <- read.csv(file="./data/current_tuiton_cleaned.csv")
state_codes <- read.csv("./data/state_codes.csv", stringsAsFactors = FALSE)
get_col_max <- function(col_name) {
  return(max(states_tuition[, col_name],
             na.rm = TRUE
  ))
  
}
get_col_min <- function(col_name) {
  return(min(states_tuition[, col_name],
             na.rm = TRUE
  ))
}


# --------------------- -----------------------------------------------------------
tuition <- read.csv("data/tuition-fees-by-state.csv", stringsAsFactors = FALSE)
tuition <- tuition[56:108, c(1, 20:36)]
tuition[1,1] <- "State"
colnames(tuition) <- tuition[1,]
tuition <- tuition[-1,]
tuition_dates <- colnames(tuition[2:16])
col_names <- colnames(debt_age)
select_ages <- col_names[2:6]

age_input <- selectInput(
  "age",
  label = "age",
  choices = select_ages,
  selected = "Ages_30_to_39"
)

size_input <- sliderInput(
  "size",
  label = "size",
  min = 4,
  max = 10,
  value = 4
)

page_one <- tabPanel(
  "page 1", # label for the tab in the navbar
  titlePanel("College Tuition"), # show with a displayed title
  
  # sidebar layout
  sidebarLayout(
    sidebarPanel(
      # widgets/summary
    ),
    mainPanel(
      # some graph or plot
    )
  )
)

page_three <- tabPanel(
  "page 3", # label for the tab in the navbar
  titlePanel("College Tuition"), # show with a displayed title
  
  # sidebar layout
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "year1",
        label = "Year 1",
        choices = tuition_dates,
        selected = "2004-05"
      ),
      selectInput(
        "year2",
        label = "Year 2",
        choices = tuition_dates,
        selected = "2018-19"
      ),
      p("This graph compares average tuition by state between two years.
        The starting graph shows that in most states college tuition has
        effectively doubled in only 16 years.")
      ),
    mainPanel(
      plotlyOutput("bar")
    )
      )
)
color_input <- selectInput(
  "color",
  label = "color",
  choices = c("Magenta" = "magenta4", "Olive" = "olivedrab3",
              "Salmon" = "salmon", "Gray" = "slategray"),
  selected = "Olive"
)

#___________________________________________________________________________________________________________________
us_map <- ui <- fluidPage(
  mainPanel(
    selectInput("mapvar",
                label = "Select a year to see a heat-map of the tuition
                rates by State",
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
#USER INTERFACE -----------------------------------------------------------

ui <- navbarPage(title = "Student Economics",
                 #INTRO PAGE
                 tabPanel(title = "Student Economics",
                          h1("Is College Affordable?", align = "center"),
                          p("This project takes a look at the cost of college tuition, 
                 student loan debt, and the federal minimum wage in order to gain 
                 an understanding of the", strong("economic feasibility"), "of 
                 attending college in the United States.This is a relevant 
                 socio-economic issue worthy of investigating given the", 
                            strong("increasingly high cost"), "of accessing postsecondary
                 education. The information provided seeks to answer:"),
                          tags$ul(
                            tags$li("How has student loan debt fluctuated over time?"),
                            tags$li("How is the cost of tuition different by year and by state?"),
                            tags$li("How many hours would a minimum wage worker have to work
                           in order to afford college tuition?")),
                          tags$img(src = "http://www.cartoonistgroup.com/properties/varvel/art_images/cg511265e8c9195.jpg",
                                   width = "550px", height = "550px", style ="display: block; margin-left: auto; margin-right:
                          auto;"),
                          p("Public discussion regarding the exorbitant cost to attend college is a hot topic
                 across both social and political landscapes.", a("The Atlantic", href = "https://www.theatlantic.com/education/archive/2018/09/why-is-college-so-expensive-in-america/569884/"), 
                            "published an article highlighting some of the complex economic and legistlative 
                 factors contributing to such high prices in the United States, citing the 
                 business-like nature and unregulated fee structure of the education system 
                 as major culprits. Hopefully the future holds greater affordability, but
                 trends in the data indicate otherwise.") ),
                 tabPanel("Student Debt",
                          h1("College Economoics"),
                          h2("Student Debt Across Age Groups"),
                          age_input,
                          size_input,
                          plotOutput("scatter"),
                          color_input),
                 
                 #INTERACTIVE PAGE 2
                 tabPanel(("College Economoics map"),
                          us_map,
                          textOutput("selected_max"),
                          textOutput("selected_min"),
                          us_map_color),
                 
                 #INTERACTIVE PAGE 3
                 tabPanel("Tuition Data",
                          page_three),
                 #INTERACTIVE PAGE 4
                 tabPanel("Minimum Wage Data",
                          year_inputs,
                          textOutput("selected_minimum_wage"),
                          graph,
                          Graph2),
                 #SUMMARY PAGE
                 tabPanel("Summary",
                          "contents")
                          )

