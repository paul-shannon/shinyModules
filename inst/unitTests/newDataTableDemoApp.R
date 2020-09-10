library(shinyModules)
#----------------------------------------------------------------------------------------------------
tbl.demo <- mtcars
fatLine <- paste(LETTERS, collapse="")
multiFatLine <- sprintf("%s\n%s\n%s\n", fatLine, fatLine, fatLine, fatLine)
tbl.demo$fatLine <- multiFatLine
searchTerms <- c("RX4", "710", "4", "Sportabout")
#----------------------------------------------------------------------------------------------------
ui <- fluidPage(

    div(radioButtons("rowSelectionPolicy", "Selection Policy",
                     choices=c("none", "single", "multiple"), selected="single"),
        style="display: inline-block;vertical-align:top; margin-left: 20px; width: 200px;"),

    div(dataTableUI("mainTable"),
        style="margin: 20px; padding: 10px; border: 2px solid black; border-radius: 10px;"
        ),
    ) # fluidPage

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session)
{

    selectedRows <- dataTableServer("mainTable", tbl=tbl.demo,
                                    selectionPolicy=reactive(input$rowSelectionPolicy))

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9033)

