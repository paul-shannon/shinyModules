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
               tabPanel("Table",
                        DT::DTOutput(NS(id, "dataTable")),
                        style = "height:500px; overflow-y: scroll;overflow-x: scroll;"),
               tabPanel("Plot",
                        plotOutput(NS(id, "barplot")))
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
ExperimentalMeasuresServer <- function(input, output, session, tbl)
{
   printf("--- executing ExperimentalMeasuresServer")
   output$dataTable <- DT::renderDataTable({
      DT::datatable(tbl, rownames=FALSE)
      })

   output$barplot <- renderPlot({
      tbl$x.axis.value <- seq_len(nrow(tbl))
      print(tbl)
      print(tbl$x.axis.value)
      ggplot(data=tbl, aes(x=x.axis.value, y=area)) + geom_bar(stat='identity') + ylim(0,5)
      })

} # ExperimentalMeasuresServer
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))


