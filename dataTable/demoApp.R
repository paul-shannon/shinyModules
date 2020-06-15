library(shiny)
source("../messageBox/messageBoxModule.R")
source("dataTableModule.R")
#----------------------------------------------------------------------------------------------------
ui <- fluidPage(
      div(dataTableUI("table"),
          style="margin: 20px; padding: 10px; border: 3px solid black; border-radius: 10px;"),
      messageBoxUI(id="messageBox.1", title=NULL, titleSpan=0, boxSpan=10),
      div(dataTableUI("subtable"),
          style="margin: 20px; padding: 10px; border: 3px solid black; border-radius: 10px;")
      )

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session){

  rowNames <- reactiveVal("none")

  rowNames <- callModule(dataTableServer, "table", tbl=mtcars,
                         selectionPolicy="multiple",
                         pageLength=10,
                         visibleRows = reactive("all"))

  callModule(messageBoxServer, "messageBox.1", newContent=rowNames)

  callModule(dataTableServer, "subtable", tbl=mtcars,
             selectionPolicy="none",
             pageLength=10,
             visibleRows=rowNames)

  }

#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9028)

