library(shinyModules)
#----------------------------------------------------------------------------------------------------
tbl.demo <- mtcars
fatLine <- paste(LETTERS, collapse="")
multiFatLine <- sprintf("%s\n%s\n%s\n", fatLine, fatLine, fatLine, fatLine)
tbl.demo$fatLine <- multiFatLine
searchTerms <- c("RX4", "710", "4", "Sportabout")
#----------------------------------------------------------------------------------------------------
ui <- fluidPage(

    div(selectInput("carSelector", "Select Car", c(" - ", rownames(mtcars))),
        style="display: inline-block;vertical-align:top; width: 200px;"),
    div(selectInput("termSearcher", "Search", c(" - ", searchTerms)),
        style="display: inline-block;vertical-align:top; margin-left: 20px; width: 200px;"),
    div(radioButtons("wrapOrNoWrap", "Wrap text in rows", choices=c("yes", "no")),
        style="display: inline-block;vertical-align:top; margin-left: 20px; width: 200px;"),
    div(
       dataTableUI("table"),
       style="margin: 20px; padding: 10px; border: 2px solid black; border-radius: 10px;"
       ),
    div(messageBoxUI(id="messageBox.1", title="", boxWidth=800, boxHeight=35, fontSize=20),
        style="margin-left: 100px;"),
    div(
      dataTableUI("subtable"),
      style="margin: 20px; padding: 10px; border: 2px solid black; border-radius: 10px;")
    ) # fluidPage

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session){

  rowNames <- reactiveVal("none")

  rowNames <- callModule(dataTableServer, "table", tbl=tbl.demo,
                         selectionPolicy="multiple",
                         pageLength=reactive(5),
                         visibleRows=reactive("all"))

  callModule(messageBoxServer, "messageBox.1", newContent=rowNames)


  callModule(dataTableServer, "subtable", tbl=tbl.demo,
             selectionPolicy="multiple",
             pageLength=reactive(5),
             visibleRows=rowNames)

  observeEvent(input$carSelector, ignoreInit=TRUE, {
     printf("carSelector event")
     carName <- input$carSelector
     if(carName == " - ")
         carName <- NULL
     wrapLongTextInCells <- input$wrapOrNoWrap == "yes"
     printf("%s", carName)
     callModule(dataTableServer,
                "table",
                tbl.demo,
                selectionPolicy="multiple",
                pageLength=reactive(5),
                visibleRows=reactive("all"),
                selectedRows=reactive(carName),
                searchTerm=reactive(NULL),
                wrapLongTextInCells=reactive(wrapLongTextInCells))
     })

  observeEvent(input$wrapOrNoWrap, ignoreInit=TRUE, {

  observeEvent(input$termSearcher, ignoreInit=TRUE, {
     printf("termSearcher event")
     wrapLongTextInCells <- input$wrapOrNoWrap == "yes"
     searchTerm <- input$termSearcher
     if(searchTerm == " - ")
         searchTerm <- ""
     printf("%s", wrapLongTextInCells)
     callModule(dataTableServer,
                "table",
                tbl.demo,
                selectionPolicy="multiple",
                pageLength=reactive(5),
                visibleRows=reactive("all"),
                selectedRows=reactive(NULL),
                searchTerm=reactive(searchTerm),
                wrapLongTextInCells=reactive(wrapLongTextInCells))
     })



  }

#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9033)

