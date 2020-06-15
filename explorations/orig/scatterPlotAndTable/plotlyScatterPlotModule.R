library(shiny)
library(plotly)

plotlyScatterPlotUI <- function(id){
    plotlyOutput(NS(id, "plot"))
    }

plotlyScatterPlotServer <- function(input, output, session,
                                    plotTitle, xAxisLabel, yAxisLabel,
                                    tbl.data, colnames){

  reactiveInputs <- reactiveValues(selectedPointNames="")

  observe({
    eventData <- event_data("plotly_click")
    if(!is.null(eventData))
       reactiveInputs$selectedPointNames <- eventData$customdata
    })

  observe({
    eventData <- event_data("plotly_selected")
    if(!is.null(eventData))
       reactiveInputs$selectedPointNames <- eventData$customdata
    })

  output$plot <- renderPlotly({
      p <- plot_ly(data=tbl.data,
                   x = tbl.data[, colnames[1]],
                   y = tbl.data[, colnames[2]],
                   customdata = rownames(tbl.data),
                   type="scatter",
                   name=rownames(tbl.data),
                   mode="markers",
                   #symbol = ~as.factor(),
                   symbols = "circle",
                   #symbols = c("circle", "x", "o"),
                   #color = ~brain,
                   size = 100
                   #color= ~as.factor(cyl),
                   #colors= c("red", "blue", "green")
                   )
      p %>%
          layout(dragmode = "select",
                 title=plotTitle,
                 xaxis=list(title=xAxisLabel),
                 yaxis=list(title=yAxisLabel)) %>%
         event_register("plotly_selected") %>%
         event_register("plotly_click")
    })

  rows <- 1
  rowNames <- "Mazda RX4"
  selectedRows <- reactive(rows)
  selectedRowNames <- reactive(rowNames)
  selectedPointNames <- reactive(reactiveInputs$selectedPointNames)
  return(list(selectedPointNames=selectedPointNames, rows=selectedRows, names=selectedRowNames))
  }

