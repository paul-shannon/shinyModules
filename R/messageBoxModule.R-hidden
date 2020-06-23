library(shiny)

printf <- function(...) print(noquote(sprintf(...)))

messageBoxUI <- function(id, title, titleSpan=1, boxSpan=11){
   fluidRow(
      if(titleSpan > 0) column(titleSpan, span(title)),
      column(boxSpan, verbatimTextOutput(NS(id, "messageBox")))
      )
   }

messageBoxServer <- function(input, output, session, newContent){
  output$messageBox <- renderPrint(newContent())
  }



