library(shinyModules)
library (RSQLite)
#----------------------------------------------------------------------------------------------------
driver <- dbDriver("SQLite")

dbConnection <- dbConnect(driver, dbname = "sqlite.db")

template.comment <- data.frame(author="",
                               timestamp=Sys.time(),
                               entity="",
                               #tags="",
                               text="",
                               stringsAsFactors=FALSE)

if(!"comments" %in% dbListTables(dbConnection))
   dbCreateTable(dbConnection, "comments", template.comment)

printf("==== commentsWithBrowse demo app initialized")

#----------------------------------------------------------------------------------------------------
ui <- fluidPage(
    selectInput(inputId="entitySelector", "Entity:", c("", "Myc", "APOE", "bogus", "rs61825286")),
    commentsWithBrowseUI(id="commentsWithBrowse"),
    br(),
    actionButton(inputId="dumpDbButton", "Dump")
    )

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session)
{
  printf("==== starting commentsWithBrowseDemoApp server")
   # callModule(commentsWithBrowseServer, "commentsWithBrowse", dbConnection, entityName=reactive(entity))

  entityName <- reactiveVal("rs483082")

  observeEvent(input$entitySelector, {
     entity <- input$entitySelector
     printf("new entity from selector: %s", entity)
     callModule(commentsWithBrowseServer, "commentsWithBrowse", dbConnection, entityName=reactive(entity))
     })

  observeEvent(input$dumpDbButton, {
      printf("--- dump db")
      print(dbReadTable(dbConnection, "comments")[, c(1:3)])
      })

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9079)
