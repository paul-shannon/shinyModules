library(shiny)
source("ggScatterPlotModule.R")
source("plotlyScatterPlotModule.R")
source("messageBoxModule.R")
source("dataTableModule.R")
#----------------------------------------------------------------------------------------------------
ui <- fluidPage(
      div(plotlyScatterPlotUI(id="plotlyWidget"),
          style="margin: 50px; padding: 10px; padding-right: 20px; border: 3px solid black; border-radius: 10px;"),
      messageBoxUI("plotlySelectionMessageBoxModule", title="point names"),
      div(ggScatterPlotUI("plot1"),
          style="margin: 50px; padding: 10px; padding-right: 20px; border: 3px solid black; border-radius: 10px;"),
      messageBoxUI("rowNumbersMessageBoxModule", title="row numbers"),
      messageBoxUI("rowNamesMessageBoxModule", title="row names"),
      div(dataTableUI("table"),
          style="margin: 20px; padding: 10px; border: 3px solid black; border-radius: 10px;"),
      messageBoxUI("tableSelectionMessageBoxModule", title="table")
      )

server <- function(input, output, session){

  cols.top  <- c("mpg", "disp")
    plotlyValues <- callModule(plotlyScatterPlotServer,
                               id="plotlyWidget",
                               plotTitle="Growth of Loblolly pine trees",
                               xAxisLabel="age",
                               yAxisLabel="height",
                               tbl.data=Loblolly[, 1:2],
                               colnames=c("age", "height"))
                               # c("body", "brain"))
                               # plotTitle="Average brain and body weights <br>for 28 species of land animals",
                               # xAxisLabel="Body Weight (kg)",
                               # yAxisLabel="Brain Weight (g)",
                               # tbl.data=Animals,
                               # c("body", "brain"))

  plotterSelectionList <- callModule(ggScatterPlotServer, "plot1",
                                     plotTitle="vanilla ggplot",
                                     data=mtcars,
                                     reactive(cols.top))
  callModule(messageBoxServer, "plotlySelectionMessageBoxModule", newContent=plotlyValues[[1]])
  callModule(messageBoxServer, "rowNumbersMessageBoxModule", newContent=plotterSelectionList$rows)
  callModule(messageBoxServer, "rowNamesMessageBoxModule",   newContent=plotterSelectionList$names)
  tableSelectionList <- callModule(dataTableServer,  "table", tbl=mtcars)
  callModule(messageBoxServer, "tableSelectionMessageBoxModule", newContent=tableSelectionList)
  }

runApp(shinyApp(ui, server), port=9028)

