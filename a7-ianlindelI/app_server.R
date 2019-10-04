library(shiny)
library(ggplot2)
library(plotly)

server <- function(input, output) {
  output$scatter <- renderPlot({

    # Store the title of the graph in a variable indicating the x/y variables
    title <- paste0("Midwest Dataset: ", input$x_var, " v.s.", input$y_var)

    # Create ggplot scatter
    g <- ggplot(midwest) +
      geom_point(
        mapping = aes_string(x = input$x_var, y = input$y_var),
        size = input$size
      ) +
      labs(x = input$x_var, y = input$y_var, title = title)
    g
  })
  output$box <- renderPlot({
    title <- paste0("Midwest Dataset: ", input$x_per, " v.s.", input$y_per)

    p <- ggplot(midwest) +
      geom_boxplot(aes_string(x = input$y_per, y = input$x_per),
        color = input$color
      ) + coord_flip()
    p
  })
}
