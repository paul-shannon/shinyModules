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
ExperimentalMeasuresUI <- function(id, title, boxHeight=300, boxWidth=320, fontSize=20, backgroundColor="beige"){

   tabsetPanel(id=NS(id, "tabset"), type = "tabs",
               tabPanel("Plot",
                        plotOutput(NS(id, "barplot"), height=boxHeight, width=boxWidth)),
               tabPanel("Table",
                        tableOutput(NS(id, "dataTable")),
                        style = sprintf("height:%dpx; width: %dpx; overflow-y: scroll;overflow-x: scroll;",
                                        boxHeight, boxWidth)),
               selected="Plot"
    )
}
#----------------------------------------------------------------------------------------------------
#' the server for a ExperimentalMesures shiny module
#'
#' @param id html element identifier
#' @param tbl, a data.frame
#'
#' @aliases ExperimentalMeasuresServer
#' @rdname ExperimentalMeasuresServer
#'
#' @export
#'
ExperimentalMeasuresServer <- function(id, tbl) {

    moduleServer(id, function(input, output, session){
        # printf("--- executing ExperimentalMeasuresServer, id: %s", id)
        doNotKnowWhyThisIsNeeded <- tbl    # a bug in the new shiny 1.5.0? (11 jul 2020)
        output$dataTable <- renderTable(tbl)

        output$barplot <- renderPlot({
            tbl$x.axis.value <- seq_len(nrow(tbl))
            ggplot(data=tbl, aes(x=x.axis.value, y=area)) + geom_bar(stat='identity') + ylim(0,5)
            })
        }) # moduleServer

    } # ExperimentalMeasuresServer
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))


