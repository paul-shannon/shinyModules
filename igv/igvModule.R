library(shiny)
library(igvShiny)
#----------------------------------------------------------------------------------------------------
igvUI <- function(id){
  tagList(
    igvShinyOutput(NS(id, 'igv'))
    )
   }
#----------------------------------------------------------------------------------------------------
igvServer <- function(input, output, session,
                      genome,
                      locus,
                      geneModelDisplayMode="COLLAPSED",
                      geneModelTrackHeight=200) {

    printf("--- igvServer, locus")
    print(locus)

    output$igv <- renderIgvShiny(
       igvShiny(list(
          genomeName=genome,
          initialLocus=locus,
          displayMode=geneModelDisplayMode,
          trackHeight=geneModelTrackHeight
          ))
        )

} # igvServer
#----------------------------------------------------------------------------------------------------

