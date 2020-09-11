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
#' @param fontColor character string, uses standard CSS naming conventions, "black" by default
#'
#' @aliases messageBoxUI
#' @rdname messageBoxUI
#'
#' @export
#'
messageBoxUI <- function(id, title, boxWidth=200, boxHeight=30, fontSize=20, fontColor="black", backgroundColor="beige"){
   fluidRow(
    div(
      div(tags$label(title), style="margin-left: 10px;"),
      div(htmlOutput(outputId=NS(id, "messageBox"),
                     style=sprintf("background-color: %s; padding-left: 10px; width: %dpx; height: %dpx; font-size:%dpx; color: %s",
                                   backgroundColor, boxWidth, boxHeight, fontSize, fontColor)),
         style=sprintf("margin-left: 5px; padding=10px; border: 1px solid gray; width: %dpx;", boxWidth+2))
      )
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
messageBoxServer <- function(id, input, output, session, newContent){
   moduleServer(id, function(input, output, session){
     output$messageBox <- renderText(newContent())
     })

} # messageBoxServer
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))


