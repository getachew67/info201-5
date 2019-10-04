library(shiny)
library(plotly)


server <- function(input, output) {
  output$bar <- renderPlotly({
    p <- plot_ly(data = tuition,
                 x = state.abb[match(tuition[,1], state.name)],
                 y = as.numeric(gsub("[\\$,]", "", tuition[,input$year2])),
                 type = 'bar',
                 name = input$year2,
                 opacity = .8) %>%
      add_trace(y = as.numeric(gsub("[\\$,]", "", tuition[,input$year1])), name = input$year1) %>%
      layout(yaxis = list(title = 'Tuition'), xaxis = list(title = 'State'), barmode = 'overlay')
    p
  })
  output$scatter <- renderPlot({
    debt_plot <- ggplot(debt_age, aes_string(x = "Year", y= input$age)) +
      geom_point() +
      geom_line() +
      ggtitle("Student Loan Debt (2004-2014)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = "Year", y = "Debt in Billions of Dollars") +
      geom_point(color = input$color, size = input$size)
    debt_plot
  })
  output$selected_minimum_wage <- renderText({ 
    paste("In", input$first_year,"the minimum wage was $", min_report(input$first_year),"an hour")
  })
  output$map <- renderPlotly({
    return(build_map(states_tuition, input$mapvar, input$color_s))
  })
  output$selected_max <- renderText({ 
    paste("In(", input$mapvar,")the highest tuition for the year was,( $",get_col_max(input$mapvar)," ) - Find the state that had this value!")
  })
  output$selected_min <- renderText({ 
    paste("In the same year (", input$mapvar,") the lowest tuition for the year was, $",get_col_min(input$mapvar),"can you find that state?")
  })
}

