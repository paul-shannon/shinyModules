library(shinyModules)
#----------------------------------------------------------------------------------------------------
tbl.demo <- mtcars
fatLine <- paste(LETTERS, collapse="")
multiFatLine <- sprintf("%s\n%s\n%s\n", fatLine, fatLine, fatLine, fatLine)
tbl.demo$fatLine <- multiFatLine
#----------------------------------------------------------------------------------------------------
ui <- fluidPage(

    div(
      dataTableUI("table"),
      style="margin: 20px; padding: 10px; border: 2px solid black; border-radius: 10px;"),

    messageBoxUI(id="messageBox.1", title="", boxWidth=800, boxHeight=40, fontSize=14),
    selectInput("carSelector", "Car:", rownames(mtcars)),

    div(
      dataTableUI("subtable"),
      style="margin: 20px; padding: 10px; border: 2px solid black; border-radius: 10px;")

    ) # fluidPage

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session){

  rowNames <- reactiveVal("none")

  rowNames <- callModule(dataTableServer, "table", tbl=tbl.demo,
                         selectionPolicy="multiple",
                         pageLength=6,
                         visibleRows=reactive("all"))

  callModule(messageBoxServer, "messageBox.1", newContent=rowNames)

  callModule(dataTableServer, "subtable", tbl=tbl.demo,
             selectionPolicy="single",
             pageLength=6,
             visibleRows=rowNames)

  observeEvent(input$carSelector, ignoreInit=TRUE, {
     printf("carSelector event")
     #tbl <- mtcars
     carName <- input$carSelector
     printf("%s", carName)
     callModule(dataTableServer,
                "subtable",
                tbl.demo,
                selectionPolicy="single",
                pageLength=10,
                visibleRows=reactive("all"),
                searchTerm=reactive(carName))
     })

  }

#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9033)

