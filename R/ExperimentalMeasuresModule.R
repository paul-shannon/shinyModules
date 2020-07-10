#----------------------------------------------------------------------------------------------------
#' the UI for a very simple message box module
#'
#' @import shiny
#'
#' @param id  character string, the html document's widget id
#' @param title  character string, title for the box
#' @param boxWidth integer pixels, 200 by default
#' @param boxHeight  integer pixels, 30 by default
#' @param fontSize  integer pixels, 20 by default
#' @param backgroundColor character string, uses standard CSS naming conventions, "beige" by default
#'
#' @aliases ExperimentalMeasuresUI
#' @rdname ExperimentalMeasuresUI
#'
#' @export
#'
ExperimentalMeasuresUI <- function(id, title, boxWidth=300, boxHeight=300, fontSize=20, backgroundColor="beige"){

   tabsetPanel(id=NS(id, "tabset"), type = "tabs",
               tabPanel("Plot",
                        plotOutput(NS(id, "barplot"))),
               tabPanel("Table",
                        tableOutput(NS(id, "dataTable"))),
                        #DT::DTOutput(NS(id, "dataTable")),
                        #style = "height:500px; overflow-y: scroll;overflow-x: scroll;"),
               selected="Plot"
    )
}
#----------------------------------------------------------------------------------------------------
#' the server for a ExperimentalMesures shiny module
#'
#' @param input enviroment provide by shiny
#' @param output enviroment provide by shiny
#' @param session enviroment provide by shiny
#' @param tbl, a data.frame
#'
#' @aliases ExperimentalMeasuresServer
#' @rdname ExperimentalMeasuresServer
#'
#' @export
#'
ExperimentalMeasuresServer <- function(id, tbl) {

    moduleServer(id, function(input, output, session){
        printf("--- executing ExperimentalMeasuresServer, id: %s", id)
        doNotKnowWhyThisIsNeeded <- tbl
        #output$dataTable <- DT::renderDataTable({DT::datatable(tbl, rownames=FALSE)
        output$dataTable <- renderTable(tbl)

        output$barplot <- renderPlot({
            tbl$x.axis.value <- seq_len(nrow(tbl))
            ggplot(data=tbl, aes(x=x.axis.value, y=area)) + geom_bar(stat='identity') + ylim(0,5)
            })
        }) # moduleServer

    } # ExperimentalMeasuresServer
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))


