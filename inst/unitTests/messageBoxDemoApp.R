library(shinyModules)
#----------------------------------------------------------------------------------------------------
ui <- fluidPage(
        messageBoxUI(id="messageBox.1", title="box 1", titleSpan=2, boxSpan=10),
        messageBoxUI(id="messageBox.2", title="box 2", titleSpan=1, boxSpan=8),
        actionButton("randomTextButton", label= "Generate random Text",
                     style="margin-top: 40px; margin-left: 200px;")
        )

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session)
{

  randomText <- reactiveVal("")

  messageBox1.contents <- callModule(messageBoxServer, "messageBox.1",
                                     newContent=randomText)
  messageBox2.contents <- callModule(messageBoxServer, "messageBox.2",
                                     newContent=reactive(tolower(randomText())))

  observeEvent(input$randomTextButton, ignoreInit=TRUE, {
     randomText(paste(sample(c(LETTERS, letters), 10, replace=TRUE), collapse=""))
     })

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9036)
