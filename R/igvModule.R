printf <- function(...) print(noquote(sprintf(...)))
#----------------------------------------------------------------------------------------------------
#' the UI for a DataTable shiny module
#'
#' @import shiny
#' @import igvShiny
#'
#' @param id  the html document's widget id
#'
#' @aliases igvUI
#' @rdname igvUI
#'
#' @export
#'
#----------------------------------------------------------------------------------------------------
igvUI <- function(id){
  tagList(
    igvShinyOutput(NS(id, 'igv'), height="300px")  # todo: add this to arg list
    )
   }
#----------------------------------------------------------------------------------------------------
#' the server for an igv.js shiny module
#'
#' @param input enviroment provide by shiny
#' @param output enviroment provide by shiny
#' @param session enviroment provide by shiny
#' @param genome  character string, eg "hg38"
#' @param locus  character string, eg "APOE" or "chr13:1000000-1001050"
#' @param geneModelDisplayMode character string, COLLAPSED, SQUISHED, EXPANDED
#' @param geneModeTrackHeight integer, eg 200
#'
#' @aliases igvServer
#' @rdname igvServer
#'
#' @export
#'
igvServer <- function(id, input, output, session,
                      genome,
                      locus,
                      width=800,
                      height=800,
                      geneModelDisplayMode="COLLAPSED",
                      geneModelTrackHeight=200) {

  moduleServer(id, function(input, output, session){

    printf("--- igvServer, locus")
    print(locus)
    printf("width: %d   height: %d", width, height)
    output$igv <- renderIgvShiny(
                      igvShiny(options=list(genomeName=genome,
                                            initialLocus=locus,
                                            displayMode=geneModelDisplayMode,
                                            trackHeight=geneModelTrackHeight
                                            ),
                               width=width,
                               height=height
                               )
        )

    igvSelection <- reactive({
        x <- input$trackClick
        req(x)
        printf("igvModule sees trackClick event")
        print(x)
        entity <- NULL
        name.indices <- grep("name", names(x))
        value.indices <- grep("value", names(x))
        printf("=== name indices:")
        print(name.indices)
        printf("=== value indices:")
        print(value.indices)
        if(length(name.indices) == length(value.indices)){
           clickData <- as.character(x[value.indices])
           names(clickData) <- as.character(x[name.indices])
              # use grep so that "name" and "Name" and "NAME" are all equally discoverable
           nameVariable <- grep("name", names(clickData), ignore.case=TRUE, value=TRUE)
           if(nchar(nameVariable) == 4){
              entity <- clickData[[nameVariable]]
              printf("you clicked on entity '%s'", entity)
              } # there is a name field
           } # the data structure returned from javascript has #name = #value fields
        printf("returning selected entity")
        print(entity)
        entity
        }) # igvSelection

      return(igvSelection)
      }) # moduleServer


} # igvServer
#----------------------------------------------------------------------------------------------------

