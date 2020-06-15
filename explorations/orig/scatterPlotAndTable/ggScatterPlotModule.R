library(shiny)
library(ggplot2)

ggScatterPlotUI <- function(id){
    div(textOutput(NS(id, "title")),
        plotOutput(NS(id, "plot"), brush = NS(id, "brush"))
    )
  }

ggScatterPlotServer <- function(input, output, session, plotTitle, data, colnames) {
  # Yields the data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  dataWithSelection <- reactive({
     brushedPoints(data, input$brush, allRows = TRUE)
     })

  output$title <- renderText({
     plotTitle
     })

  output$plot <- renderPlot({
    #print(head(dataWithSelection()))
    scatterPlot(dataWithSelection(), colnames())
    })

  selectedRows <- reactive(which(dataWithSelection()$selected_))
  selectedRowNames <- reactive(rownames(dataWithSelection())[selectedRows()])
  #return(list(tbl=dataWithSelection, selectedRows=selected))
  return(list(rows=selectedRows, names=selectedRowNames))
  }

scatterPlot <- function(data, cols) {
  #printf("scatterPlot selection:")
  #print(selection)
  ggplot(data, aes_string(x = cols[1], y = cols[2])) +
     geom_point(aes(color = selected_)) +
     scale_color_manual(values = c("black", "red"), guide = FALSE)
     }
