library(shinyModules)
library (RSQLite)
#----------------------------------------------------------------------------------------------------
driver <- dbDriver("SQLite")

dbConnection <- dbConnect(driver, dbname = "sqlite.db")

template.comment <- data.frame(author="",
                               timestamp=Sys.time(),
                               entity="",
                               tags="",
                               text="",
                               stringsAsFactors=FALSE)

if(!"comments" %in% dbListTables(dbConnection))
   dbCreateTable(dbConnection, "comments", template.comment)

printf("==== comments demo app initialized")

#----------------------------------------------------------------------------------------------------
ui <- fluidPage(
    selectInput(inputId="entitySelector", "Entity:", c("", "Myc", "APOE", "bogus", "rs61825286")),
    commentsUI(id="comments"),
    br(),
    actionButton(inputId="dumpDbButton", "Dump")
    )

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session)
{
  printf("==== starting commentsDemoApp server")
  entityName <- reactiveVal("rs483082")

  observeEvent(input$entitySelector, {
     entity <- input$entitySelector
     printf("new entity from selector: %s", entity)
     callModule(commentsServer, "comments", dbConnection, entityName=reactive(entity))
     })

  observeEvent(input$dumpDbButton, {
      printf("--- dump db")
      print(dbReadTable(dbConnection, "comments")[, c(1:3)])
      })

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9079)
