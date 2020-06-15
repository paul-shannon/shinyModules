library(shiny)
source("igvModule.R")
source("../dataTable/dataTableModule.R")
source("../messageBox/messageBoxModule.R")

tbl.snps <- read.table("threeSnps.tsv", header=TRUE, sep="\t", as.is=TRUE, row.names=1)


ui <- fluidPage(
   div(igvUI("igv1"),
       style="margin: 10px; margin-bottom: 5px; padding: 10px; border: 3px solid black; border-radius: 10px;"),
   div(dataTableUI("snpDataTable"),
          style="margin: 10px; margin-bottom: 30px; padding: 10px; border: 3px solid black; border-radius: 10px;"),
   actionButton("searchButton", "Search"),
   actionButton("addTrackButton", "Add Track"),
   actionButton("getChromLoc", "Get Region"),
   messageBoxUI(id="messageBox.rsid", title="snps", titleSpan=1, boxSpan=8)
   )
#----------------------------------------------------------------------------------------------------
server <- function(input, output, session){

   roi <- reactiveVal("APOE")

   roi <- callModule(dataTableServer, "snpDataTable",
                     tbl=tbl.snps,
                     selectionPolicy="multiple",
                     pageLength=10,
                     visibleRows = reactive("all"))

   callModule(messageBoxServer, "messageBox.rsid", newContent=roi)

   callModule(igvServer, "igv1",
              genome="hg38",
              geneModelDisplayMode="COLLAPSED",
              locus="APOE")

   observe({
      rsids <- roi()
      tbl.sub <- tbl.snps[rsids,]
      if(nrow(tbl.sub) > 0){
         chrom.first.encountered <- tbl.sub$chrom[1]
         tbl.sub <- subset(tbl.sub, chrom==chrom.first.encountered)
         start.loc <- min(tbl.sub$start)
         end.loc <- max(tbl.sub$end)
         shoulder <- round(1 + 0.1 * (end.loc - start.loc))
         region.string <- sprintf("%s:%d-%d", chrom.first.encountered,
                                  start.loc - shoulder,
                                  end.loc + shoulder)
         showGenomicRegion(session, region.string)
         } # if nrow
      })

   observeEvent(input$addTrackButton, {
      tbl.bed <- tbl.snps[, c("chrom", "start", "end")]
      tbl.bed$name <- rownames(tbl.snps)
      loadBedTrack(session, "snps", tbl.bed, color="red");
      })

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9044)

