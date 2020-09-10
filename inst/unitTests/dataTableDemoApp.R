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
    div(radioButtons("rowSelectionPolicy", "Selection Policy", choices=c("none", "single", "multiple")),
        style="display: inline-block;vertical-align:top; margin-left: 20px; width: 200px;"),
    div(
       dataTableUI("mainTable"),
       style="margin: 20px; padding: 10px; border: 2px solid black; border-radius: 10px;"
       ),
    div(messageBoxUI(id="messageBox.1", title="", boxWidth=800, boxHeight=35, fontSize=20),
        style="margin-left: 100px;"),
    div(
      dataTableUI("subtable"),
      style="margin: 20px; padding: 10px; border: 2px solid black; border-radius: 10px;")
    ) # fluidPage

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session)
{
      # some startup values, later set repsonsively
  selectedRows <- reactiveVal("none")
  selectionPolicy <- reactiveVal("single")
  wrapLongTextInCells <- reactiveVal(TRUE)
  selectedCar <- reactiveVal(NULL)
  searchTargetCar <- reactiveVal(NULL)

      # dynamically updated whenever a selection is made in the "mainTable" instance
  selectedRows <- callModule(dataTableServer, "mainTable", tbl=tbl.demo,
                             selectionPolicy=reactive("multiple"),
                             pageLength=reactive(5),
                             visibleRows=reactive("all"))

      # displays the selectedRows (mtcars rownames) at every selection event
  callModule(messageBoxServer, "messageBox.1", newContent=selectedRows)

      # update the subtable every time selectedRows changes
  callModule(dataTableServer, "subtable", tbl=tbl.demo,
             selectionPolicy=input$rowSelectionPolicy, # reactive("single"),
             #selectionPolicy=reactive("none"),
             pageLength=reactive(5),
             visibleRows=selectedRows,
             wrapLongTextInCells=reactive(input$wrapOrNoWrap == "yes"))

      # called when either of the pulldown menus fires: for selection, for search
      # select and search are orthogonal, only one or the other happens
  refreshMainTable <- function(carName, searchTerm){
     wrapLongTextInCells <- input$wrapOrNoWrap == "yes"
     selection.policy <- input$rowSelectionPolicy
     callModule(dataTableServer,
                "table",
                tbl.demo,
                #selectionPolicy=reactive("multiple"),
                selectionPolicy=reactive(selection.policy),
                pageLength=reactive(5),
                visibleRows=reactive("all"),
                selectedRows=reactive(carName),
                searchTerm=reactive(searchTerm),
                wrapLongTextInCells=reactive(wrapLongTextInCells))
       } # refreshMainTable


      # just the carSelector value changes
  observeEvent(input$carSelector, ignoreInit=TRUE, {
     carName <- input$carSelector
     if(carName == " - ") carName <- NULL
     refreshMainTable(carName, searchTerm=NULL)
     })

      # just the termSearcher value changed
  observeEvent(input$termSearcher, ignoreInit=TRUE, {
     searchTerm <- input$termSearcher
     if(searchTerm == " - ") searchTerm <- NULL
     refreshMainTable(carName=NULL, searchTerm=searchTerm)
     })

  observeEvent(input$wrapOrNoWrap, ignoreInit=TRUE, {
     searchTerm <- isolate(input$termSearcher)
     if(searchTerm == " - ") searchTerm <- NULL
     carName <- isolate(input$carSelector)
     if(carName == " - ") carName <- NULL
     refreshMainTable(carName=carName, searchTerm=searchTerm)
     })


} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9033)

