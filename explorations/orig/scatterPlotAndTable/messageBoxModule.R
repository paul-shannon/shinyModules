library(shiny)

messageBoxUI <- function(id, title){
   #div(style="display: inline-block;vertical-align:top; width: 150px;",
   fluidRow(
      column(1, span(title)),
      column(10, verbatimTextOutput(NS(id, "messageBox")))
      )
   }

messageBoxServer <- function(input, output, session, newContent){
  output$messageBox <- renderPrint(newContent())
  }



