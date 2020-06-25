#----------------------------------------------------------------------------------------------------
#' the UI for a persistent comments shiny module
#'
#' @import shiny
#' @import shinyjs
#' @import RSQLite
#'
#' @param id  the html document's widget id
#'
#' @aliases commentsUI
#' @rdname commentsUI
#'
#' @export
#'
commentsUI <- function(id){
    fluidPage(
        useShinyjs(debug = TRUE),
        wellPanel(
            textInput(inputId=NS(id, "entity"), label="",
                      value = "", width=300, placeholder = NULL),
            textInput(inputId=NS(id, "authorInput"), label="author",
                      value = "", width=300, placeholder = NULL),
            textInput(inputId=NS(id, "tagsInput"), label="tags",
                      value = "", width=300, placeholder = NULL),
            textAreaInput(inputId=NS(id, "commentInputText"), label="comment",
                      value = "", width=800, height=300, placeholder = NULL),
            actionButton(inputId=NS("comments", "saveCommentsButton"), "Save"),
            textOutput(outputId=NS(id, "box"))
           )
        )
  }

#----------------------------------------------------------------------------------------------------
#' the server for a comments shiny module
#'
#' @param input enviroment provide by shiny
#' @param output enviroment provide by shiny
#' @param session enviroment provide by shiny
#' @param dbConnection an sqlite database connetion
#'
#' @aliases commentsServer
#' @rdname commentsServer
#'
#' @export
#'
commentsServer <- function(input, output, session, dbConnection, entityName){

    entity <- entityName()
    printf("=== starting commentsServer: %s", entity)

    observe({
       entity <- entityName()
       printf("observing entityName: %s", entity)
       updateTextInput(session, 'entity', value=entity)
       })

    observeEvent(input$saveCommentsButton, {
      entity <- isolate(input$entity)
      author <- isolate(input$authorInput)
      tagsText <- isolate(input$tagsInput)
      commentText <- isolate(input$commentInputText)
      output$box <- renderText({
        shiny::validate(
           need(author != "", "Please specify author name."),
           need(tagsText != "", "Need at least one tag."),
           need(commentText != "", "Comment text needed.")
           )
        tbl <-data.frame(author=author,
                         timestamp=Sys.time(),
                         entity=entity,
                         tags=tagsText,
                         text=commentText,
                         stringsAsFactors=FALSE)
        printf("--- appending");
        status <- dbAppendTable(dbConnection, "comments", tbl)
        printf("leaving commentsModule save operation")
        ""
        })  # renderText
      }) # observeEvent

   return("fubar")

} # commentsServer
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
