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
#' @param nowrap  logcial default TRUE
#'
#' @aliases dataTableServer
#' @rdname dataTableServer
#'
#' @export
#'
dataTableServer <- function(input, output, session,
                            tbl,
                            selectionPolicy="single",
                            pageLength=5,
                            visibleRows,
                            nowrap=TRUE) {

    output$dataTable <- DT::renderDataTable({
       visibleRowsImmediate <- visibleRows()
       if(length(visibleRowsImmediate) > 0){
           if(visibleRowsImmediate[1] == "all"){
               tbl.sub <- tbl
           } else if(visibleRowsImmediate[1] == "none") {
               tbl.sub <- data.frame()
           } else {
               tbl.sub <- tbl[visibleRowsImmediate,]
           }
           printf("entering renderDataTable, nrow: %d", nrow(tbl))
           DTclass <- ""
           if(nowrap) DTclass <- "nowrap display"
           DT::datatable(tbl.sub,
                         rownames=TRUE,
                         class=DTclass,
                         #class='nowrap display',
                         options=list(dom='<lfip<t>>',
                                      scrollX=TRUE,
                                      lengthMenu = c(3,5,10,50),
                                      pageLength = pageLength,
                                      paging=TRUE),
                         selection=selectionPolicy)
        } # if length(visibleRowsImmediate) > 0
    })  # renderDataTable

  tableSelection <- reactive({
     rownames(tbl)[input$dataTable_rows_selected]
     })

  return(tableSelection)

} # dataTableServer
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
