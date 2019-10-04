library(shiny)
library(ggplot2)

# Get a vector of column names (from `mpg`) to use as select inputs
select_values <- colnames(midwest)


page_one <- tabPanel(
  "Midwest Data", # label for the tab in the navbar
  titlePanel("Population by State"), # show with a displayed title

  # sidebar layout
  sidebarLayout(
    sidebarPanel(
      h1("Midwest Dataset Exploration"),
      selectInput(
        "x_var",
        label = "X Variable",
        choices = select_values,
        selected = "state"
      ),
      selectInput(
        "y_var",
        label = "Y Variable",
        choices = select_values[c(5, 7:11)],
        selected = "poptotal"
      ),
      sliderInput(
        "size",
        label = "Size of point", min = 1, max = 10, value = 5
      )
    ),
    mainPanel(
      # graphs a scatter plot
      plotOutput("scatter")
    )
  )
)

page_two <- tabPanel(
  "Continued", # label for the tab in the navbar
  titlePanel("Percentage by State"), # show with a displayed title

  # sidebar layout
  sidebarLayout(
    sidebarPanel(
      h1("Midwest Dataset Exploration"),

      selectInput(
        "x_per",
        label = "X Variable",
        choices = select_values[c(12:16, 18:20, 22:26)],
        selected = "percollege"
      ),
      # Your y input
      selectInput(
        "y_per",
        label = "Y Variable",
        choices = select_values,
        selected = "state"
      ),
      selectInput(
        "color",
        label = "Color",
        choices = c("blue", "yellow", "red", "orange", "black"),
        selected = "blue"
      )
    ),
    mainPanel(
      plotOutput("box")
    )
  )
)

# Create a variable `ui` that is a navbarPage ui element.
ui <- navbarPage(
  "Midwest",
  page_one,
  page_two
)
