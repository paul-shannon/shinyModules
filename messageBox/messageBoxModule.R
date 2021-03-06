library(shiny)

messageBoxUI <- function(id, title, titleSpan=1, boxSpan=11){
   fluidRow(
      if(titleSpan > 0) column(titleSpan, span(title)),
      column(boxSpan, verbatimTextOutput(NS(id, "messageBox")))
      )
   }

messageBoxServer <- function(input, output, session, newContent){
    #incomingValue <- newContent()
    #if(!is.null(incomingValue) && nchar(incomingValue) > 0)
        output$messageBox <- renderPrint(newContent())
    #else
    #    output$message <- renderPrint("")
  }



