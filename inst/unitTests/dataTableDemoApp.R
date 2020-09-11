library(shinyModules)
#----------------------------------------------------------------------------------------------------
tbl.demo <- mtcars
fatLine <- paste(LETTERS, collapse="")
multiFatLine <- sprintf("%s\n%s\n%s\n", fatLine, fatLine, fatLine, fatLine)
tbl.demo$fatLine <- multiFatLine
searchTerms <- c("RX4", "710", "4", "Sportabout")
#----------------------------------------------------------------------------------------------------
ui <- fluidPage(

   div(selectInput("termSearcher", "Search", c(" - ", searchTerms)),
       style="display: inline-block;vertical-align:top; margin-left: 20px; width: 200px;"),

   div(radioButtons("rowSelectionPolicy", "Selection Policy",
                    choices=c("none", "single", "multiple"), selected="single"),
       style="display: inline-block;vertical-align:top; margin-left: 20px; width: 200px;"),

   div(radioButtons("wrapOrNoWrap", "Wrap text in rows", choices=c("yes", "no"), selected="no"),
       style="display: inline-block;vertical-align:top; margin-left: 20px; width: 200px;"),

   div(dataTableUI("mainTable"),
       style="margin: 20px; padding: 10px; border: 2px solid black; border-radius: 10px;"
       ),
   div(messageBoxUI(id="selectResultsDisplay", title="selection", boxWidth=800, boxHeight=50,
                    fontSize=20),
       style="margin-left: 30px"),
   div(dataTableUI("subTable"),
       style="margin: 20px; padding: 10px; border: 2px solid black; border-radius: 10px;"
       ),

   ) # fluidPage

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session)
{
    mainTable.selectedRows <- dataTableServer("mainTable", tbl=tbl.demo,
                                              selectionPolicy=reactive(input$rowSelectionPolicy),
                                              wrapLongTextInCells=reactive(input$wrapOrNoWrap=="yes"),
                                              searchString=reactive(input$termSearcher),
                                              rownames.to.display=reactive("all"))

    messageBoxServer("selectResultsDisplay", newContent=mainTable.selectedRows)

    subTable.selectedRows <- dataTableServer("subTable", tbl=tbl.demo,
                                             selectionPolicy=reactive("none"),
                                             rownames.to.display=mainTable.selectedRows)

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9033)

