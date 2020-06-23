#----------------------------------------------------------------------------------------------------
#' the UI for a very simple message box module
#'
#' @import shiny
#'
#' @param id  character string, the html document's widget id
#' @param title  character string, title for the box
#' @param titleSpan integer, width in bootstrap units (max totaling 12)
#' @param boxSpan  integer, width in bootstrap units (max totaling 12)
#'
#' @aliases messageBoxUI
#' @rdname messageBoxUI
#'
#' @export
#'
messageBoxUI <- function(id, title, titleSpan=1, boxSpan=11){
   fluidRow(
      if(titleSpan > 0) column(titleSpan, span(title)),
      column(boxSpan, verbatimTextOutput(NS(id, "messageBox")))
      )
   }

#----------------------------------------------------------------------------------------------------
#' the server for a MessageBox shiny module
#'
#' @param input enviroment provide by shiny
#' @param output enviroment provide by shiny
#' @param session enviroment provide by shiny
#' @param newContent character string, what to display
#'
#' @aliases messageBoxServer
#' @rdname messageBoxServer
#'
#' @export
#'
messageBoxServer <- function(input, output, session, newContent){
  output$messageBox <- renderPrint(newContent())
  }
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))


