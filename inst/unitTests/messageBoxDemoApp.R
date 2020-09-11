library(shinyModules)
#----------------------------------------------------------------------------------------------------
ui <- fluidPage(
    messageBoxUI(id="messageBox.1", title="random mixed text"), # , boxWidth=200, fontSize=12),

    messageBoxUI(id="messageBox.2", title="lower case", boxWidth=400, boxHeight=50,
                 fontSize=30, backgroundColor="lightgray"),

    actionButton("randomTextButton", label= "Generate random Text",
                     style="margin-top: 40px; margin-left: 200px;")
        )

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session)
{

  randomText <- reactiveVal("")

  messageBox1.contents <- messageBoxServer("messageBox.1", newContent=randomText)
  messageBox2.contents <- messageBoxServer("messageBox.2", newContent=reactive(tolower(randomText())))

  observeEvent(input$randomTextButton, ignoreInit=TRUE, {
     randomText(paste(sample(c(LETTERS, letters), 10, replace=TRUE), collapse=""))
     })

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9086)
