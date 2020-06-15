library(shiny)
library(DT)

dataTableUI <- function(id){
  tagList(
    DTOutput(NS(id, "dataTable"))
    )
  }


dataTableServer <- function(input, output, session, tbl) {

    output$dataTable <- DT::renderDataTable({
       printf("entering renderDataTable, nrow: %d", nrow(tbl))
       DT::datatable(tbl,
                     rownames=TRUE,
                     options=list(dom='<lfip<t>>',
                                  scrollX=TRUE,
                                  #autoWidth=TRUE,
                                  lengthMenu = c(3,5,10,50),
                                  pageLength = 5,
                                  paging=TRUE),
                     selection="multiple")
    })


  tableSelection <- reactive({
     input$dataTable_rows_selected
     })


  #observeEvent(input$dataTable_rows_selected, {
  #   printf("rows selected")
  #   })

  return(tableSelection)

} # dataTableServer
#----------------------------------------------------------------------------------------------------
