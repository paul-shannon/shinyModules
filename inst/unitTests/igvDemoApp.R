library(shinyModules)
library(GenomicRanges)
library(rtracklayer)
#--------------------------------------------------------------------------------------------------------------
file <- system.file(package="shinyModules", "extdata", "threeSnps.tsv")
tbl.snps <- read.table(file, header=TRUE, sep="\t", as.is=TRUE, row.names=1)


ui <- fluidPage(
   div(igvUI("igv"),
       style="margin: 10px; margin-bottom: 5px; padding: 10px; border: 3px solid black; border-radius: 10px;"),
   #actionButton("searchButton", "Search"),
   actionButton("addSnpTrackButton", "Add SNP Track"),
   actionButton("addConservationTrackButton", "Add Conservation Track"),
   #actionButton("getChromLoc", "Get Region"),
   messageBoxUI(id="messageBox.igv", title="igv selection", boxWidth= 600),
   div(dataTableUI("snpDataTable"),
          style="margin: 10px; margin-bottom: 30px; padding: 10px; border: 3px solid black; border-radius: 10px;"),
   messageBoxUI(id="messageBox.snpTable", title="snps", boxWidth=600)
   )
#--------------------------------------------------------------------------------------------------------------
server <- function(input, output, session){

   roi <- reactiveVal("APOE")

   roi <- callModule(dataTableServer, "snpDataTable",
                     tbl=tbl.snps,
                     selectionPolicy="multiple",
                     pageLength=reactive(10),
                     visibleRows = reactive("all"))

   callModule(messageBoxServer, "messageBox.snpTable", newContent=roi)

   selectedEntity <- callModule(igvServer, "igv",
                                genome="hg38",
                                geneModelDisplayMode="COLLAPSED",
                                locus="APOE")

   callModule(messageBoxServer, "messageBox.igv", newContent=selectedEntity)

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

   observeEvent(input$addSnpTrackButton, {
      tbl.bed <- tbl.snps[, c("chrom", "start", "end")]
      tbl.bed$name <- rownames(tbl.snps)
      loadBedTrack(session, "snps", tbl.bed, color="red");
      })

   observeEvent(input$addConservationTrackButton, {
       bw.file <- "~/s/examples/http/flaskForGenomeAnnotations/mimic-trena-igv-data/static/hg38.phastCons7way.bw"
       gr.region <- GRanges(seqnames="chr19", IRanges(start=44862811, end=44952378))
       gr.small <- import(con=bw.file, which=gr.region)
       tbl.bedGraph <- as.data.frame(gr.small)[, c("seqnames", "start", "end", "score")]
       colnames(tbl.bedGraph)[1] <- "chrom"
       tbl.bedGraph$chrom <- as.character(tbl.bedGraph$chrom)
       loadBedGraphTrack(session, "phast7", tbl.bedGraph, autoscale=TRUE)
      })

} # server
#--------------------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9034)

