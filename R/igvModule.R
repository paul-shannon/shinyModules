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
    igvShinyOutput(NS(id, 'igv'))
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
igvServer <- function(input, output, session,
                      genome,
                      locus,
                      geneModelDisplayMode="COLLAPSED",
                      geneModelTrackHeight=200) {

    printf("--- igvServer, locus")
    print(locus)

    observeEvent(input$bogus_trackClick, {
       x <- input$trackClick
       name.indices <- grep("name", names(x))
       value.indices <- grep("value", names(x))
       if(length(name.indices) == length(value.indices)){
          clickData <- as.character(x[value.indices])
          names(clickData) <- as.character(x[name.indices])
          if("name" %in% names(clickData)){
             entity <- clickData[["name"]]
             printf("you clicked on entity '%s'", entity)
             } # there is a name field
          } # the data structure returned from javascript has #name = #value fields
       })

    output$igv <- renderIgvShiny(
       igvShiny(list(
          genomeName=genome,
          initialLocus=locus,
          displayMode=geneModelDisplayMode,
          trackHeight=geneModelTrackHeight
          ))
        )

    igvSelection <- reactive({
        x <- input$trackClick
        entity <- NULL
        name.indices <- grep("name", names(x))
        value.indices <- grep("value", names(x))
        if(length(name.indices) == length(value.indices)){
            clickData <- as.character(x[value.indices])
            names(clickData) <- as.character(x[name.indices])
            if("name" %in% names(clickData)){
               entity <- clickData[["name"]]
               #printf("you clicked on entity '%s'", entity)
               } # there is a name field
        } # the data structure returned from javascript has #name = #value fields
        #printf("returning selected entity")
        #print(entity)
        entity
        })

  return(igvSelection)


} # igvServer
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))

