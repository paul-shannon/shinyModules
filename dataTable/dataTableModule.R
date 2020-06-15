library(shiny)
library(DT)

dataTableUI <- function(id){
  tagList(
    DTOutput(NS(id, "dataTable"))
    )
  }


dataTableServer <- function(input, output, session,
                            tbl,
                            selectionPolicy="single",
                            pageLength=5,
                            visibleRows) {

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
           DT::datatable(tbl.sub,
                         rownames=TRUE,
                         options=list(dom='<lfip<t>>',
                                      scrollX=TRUE,
                                        #autoWidth=TRUE,
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
