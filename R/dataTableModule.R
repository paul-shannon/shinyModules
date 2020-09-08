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
#' @param input enviroment provide by shiny
#' @param output enviroment provide by shiny
#' @param session enviroment provide by shiny
#' @param tbl data.frame
#' @param selectionPolicy character string, single, multiple or none
#' @param pageLength  integer typically 5, 10, 25 or 50
#' @param visibleRows  "all", "none", or a list of rownames
#' @param selectedRows character string, reactive(NULL)  by default
#' @param searchTerm character string, "" by default
#' @param wrapLongTextInCells  logcial default TRUE
#'
#' @aliases dataTableServer
#' @rdname dataTableServer
#'
#' @export
#'
dataTableServer <- function(input, output, session,
                            tbl,
                            selectionPolicy=reactive("single"),
                            pageLength=reactive(5),
                            visibleRows=reactive(head(rownames(tbl))),
                            selectedRows=reactive(NULL),
                            searchTerm=reactive(NULL),
                            wrapLongTextInCells=reactive(FALSE)) {

    output$dataTable <- DT::renderDataTable({

        selectedRowNames <- selectedRows()
        searchTermString <- searchTerm()

             #------------------------------------------------------------
             # search mode, or row selection mode, or neither.
             # cannot be both.  selection is given priority.
             #------------------------------------------------------------

        mode <- "neitherSelectionNorSearch"
        if(!is.null(selectedRowNames)){
           mode <- "selectRows"
           #printf("selectedRowNames: %s", paste(selectedRowNames, collapse=","))
           }
        if(is.null(selectedRowNames)){
           if(!is.null(searchTermString))
              mode <- "search"
           }

        visibleRowsImmediate <- visibleRows()

        tbl.sub <- data.frame()   # an empty table by default

        if(length(visibleRowsImmediate) > 0){
           if(visibleRowsImmediate[1] == "all")
              tbl.sub <- tbl
           else{
              tbl.sub <- tbl[visibleRowsImmediate,]
              }
           } # if

        wrapText <- wrapLongTextInCells()
        DTclass <- "display"
        if(!wrapText)
            DTclass <- paste0(DTclass, " nowrap")

        selectionOption <- list()
        searchOption <- list()

        if(mode == "selectRows")
           selectionOption <- list(mode=selectionPolicy(), selected=selectedRowNames)
        if(mode == "search")
           searchOption <- list(caseInsensitive=TRUE, search=searchTermString)

        DT::datatable(tbl.sub,
                      rownames=TRUE,
                      class=DTclass,
                      options=list(dom='<lfip<t>>',
                                   scrollX=TRUE,
                                   search=searchOption,
                                   lengthMenu = c(3,5,10,50),
                                   pageLength = pageLength(),
                                   paging=TRUE),
                      selection=selectionOption)
        # } # if length(visibleRowsImmediate) > 0
    })  # renderDataTable

  tableSelection <- reactive({
     rownames(tbl)[input$dataTable_rows_selected]
     })

  return(tableSelection)

} # dataTableServer
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
