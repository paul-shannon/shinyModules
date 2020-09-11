#----------------------------------------------------------------------------------------------------
#' the UI for a DataTable shiny module
#'
#' @import shiny
#'
#' @param id  the html document's widget id
#'
#' @aliases dataTableUI
#' @rdname dataTableUI
#'
#' @export
#'
dataTableUI <- function(id){
  tagList(
    DT::DTOutput(NS(id, "dataTable"))
    )
  }

#----------------------------------------------------------------------------------------------------
#' the server for a DataTable shiny module
#'
#' @param input enviroment provided by shiny
#' @param output enviroment provided by shiny
#' @param session enviroment provided by shiny
#' @param tbl data.frame
#' @param selectionPolicy character string, "none", "single", or "multiple"
#' @param wrapLongTextInCells logical, TRUE or FALSE
#' @param searchString character string, selects all rows with this, default "" (no search)
#' @param rownames.to.display character vector, default "all",
#'
#' @aliases dataTableServer
#' @rdname dataTableServer
#'
#' @export
#'
dataTableServer <- function(id, input, output, session,
                            tbl,
                            selectionPolicy=reactive("multiple"),
                            wrapLongTextInCells=reactive(TRUE),
                            searchString=reactive(""),
                            rownames.to.display=reactive("all")
                            ){

  moduleServer(id,  function(input, output, session){

     output$dataTable <- DT::renderDataTable({

        tbl.sub <- tbl
        rownames <- rownames.to.display()
        if(length(rownames) == 0){
            tbl.sub <- data.frame()
        }else{
           if(rownames[1] == "all")
              tbl.sub <- tbl
           else{
              rownames <- intersect(rownames, rownames(tbl))
              if(length(rownames) > 0)
                 tbl.sub <- tbl[rownames,]
              } # else
           } # major else

         selectionOption <- list(mode=selectionPolicy(), selected=NULL)
         searchString <- searchString()
         searchOptions <- list()
         if(searchString != " - ")
            searchOptions <- list(search=searchString, caseInsensitive=TRUE)

         DTclass <- "display"
         if(!wrapLongTextInCells())
            DTclass <- paste0(DTclass, " nowrap")

         DT::datatable(tbl.sub,
                       rownames=TRUE,
                       class=DTclass,
                       options=list(dom='<lfip<t>>',
                                    scrollX=TRUE,
                                    search=searchOptions,
                                    lengthMenu = c(3,5,10,50),
                                    pageLength = 5,
                                    paging=TRUE),
                       selection=selectionOption)
         })  # renderDataTable

      tableSelection <- reactive({
         rownames(tbl)[input$dataTable_rows_selected]
         })
      return(tableSelection)
    }) # moduleServer


} # dataTableServer
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
