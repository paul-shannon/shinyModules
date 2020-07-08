#----------------------------------------------------------------------------------------------------
#' the UI for a persistent commentsWithBrowse shiny module
#'
#' @import shiny
#' @import shinyjs
#' @import RSQLite
#'
#' @param id  the html document's widget id
#'
#' @aliases commentsWithBrowseUI
#' @rdname commentsWithBrowseUI
#'
#' @export
#'
commentsWithBrowseUI <- function(id){
  fluidPage(
    useShinyjs(debug = TRUE),
    titlePanel("Add and Review Comments and Notations"),
    sidebarPanel(
        radioButtons("enterOrBrowseComments", "Mode", choices=c(Enter="enter", Browse="browse")),
        #actionButton(inputId=NS(id, "loadTableButton"), "Load"),
        width=2
        ),
    mainPanel(
      conditionalPanel(
         condition = "input.enterOrBrowseComments=='enter'",
         wellPanel(
            textInput(inputId=NS(id, "entity"), label="",
                      value = "", width=300, placeholder = NULL),
            textInput(inputId=NS(id, "authorInput"), label="author",
                      value = "", width=300, placeholder = NULL),
            textAreaInput(inputId=NS(id, "commentInputText"), label="comment",
                      value = "", width=800, height=300, placeholder = NULL),
            actionButton(inputId=NS(id, "saveCommentsButton"), "Save"),
            textOutput(outputId=NS(id, "box"))
        )),
      conditionalPanel(
         condition = "input.enterOrBrowseComments=='browse'",
         wellPanel(
           div(dataTableUI(NS(id, "commentsTable")),
               style="margin: 10px; margin-bottom: 30px; padding: 10px; border: 3px solid black; border-radius: 10px;"),
           messageBoxUI(NS(id, "annotationText"), title="", boxWidth=600, boxHeight=400,
                        fontSize=30, backgroundColor="white")
           #textOutput(outputId=NS(id, "annotationTextBox"), height=300)
           )
        ),
      width=10
      ) # mainPanel
    ) # fluidPage

  }  # commentsWithBrowserUI

#----------------------------------------------------------------------------------------------------
#' the server for a commentsWithBrowse shiny module
#'
#' @param input enviroment provide by shiny
#' @param output enviroment provide by shiny
#' @param session enviroment provide by shiny
#' @param dbConnection an sqlite database connetion
#'
#' @aliases commentsWithBrowseServer
#' @rdname commentsWithBrowseServer
#'
#' @export
#'
commentsWithBrowseServer <- function(input, output, session, dbConnection, entityName){

    selectedRows <- reactiveVal("none")

    tbl.dbx  <- reactive({
       invalidateLater(1000, session)
       dbReadTable(dbConnection, "comments")
       })



    tbl.dbx <- reactiveVal(dbReadTable(dbConnection, "comments")[, c(1:3)])

    entity <- entityName()
    printf("=== starting commentsWithBrowseServer: %s", entity)


    #tbl.from.db <- reactiveVal(dbReadTable(dbConnection, "comments")[, c(1:3)])
    #printf("tbl.from.db, rows")
    #print(nrow(tbl.from.db))

    #observeEvent(input$loadTableButton, {
    observe({
        #printf("==== observeEvent, loadTableButton")
        printf("observe, about to read and display db table")
        #tbl.db <- dbReadTable(dbConnection, "comments")[, c(1:3)]
        printf("======= tbl.db")
        #print(tbl.db)
        selectedRows <- callModule(dataTableServer, "commentsTable",
                          tbl=tbl.dbx(),
                          selectionPolicy="single",
                          pageLength=reactive(10),
                          visibleRows=reactive("all"))
        })

    observe({
       entity <- entityName()
       printf("observing entityName: %s", entity)
       updateTextInput(session, 'entity', value=entity)
       })

    observeEvent(input$saveCommentsButton, {
      printf("=== input$saveCommentsButton event")
      entity <- isolate(input$entity)
      author <- isolate(input$authorInput)
      #tagsText <- isolate(input$tagsInput)
      commentText <- isolate(input$commentInputText)
      output$box <- renderText({
        shiny::validate(
           need(author != "", "Please specify author name."),
           #need(tagsText != "", "Need at least one tag."),
           need(commentText != "", "Comment text needed.")
           )
        tbl <-data.frame(author=author,
                         timestamp=Sys.time(),
                         entity=entity,
                         #tags=tagsText,
                         text=commentText,
                         stringsAsFactors=FALSE)
        printf("--- appending");
        status <- dbAppendTable(dbConnection, "comments", tbl)
        #tbl.dbx <-dbReadTable(dbConnection, "comments")[, c(1:3)]
        printf("leaving commentsWithBrowseModule save operation")
        ""
        })  # renderText
      }) # observeEvent

   return("fubar")

} # commentsWithBrowseServer
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
