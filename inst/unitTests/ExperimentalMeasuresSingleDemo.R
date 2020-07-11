library(shinydashboard)
library(shinyModules)
#--------------------------------------------------------------------------------------------------------------
# from the original spreadsheet jeff gave me
load(system.file(package="shinyModules", "extdata", "tbl.proteomics.orig.RData"))         # tbl.orig
tbl <- get(load(system.file(package="shinyModules", "extdata",
                            "proteomicsExperimentSubsetDataFrame.RData")))
analytes <- sort(unique(tbl$analyte))
aoi.01 <- "LMNB1_pS23_AGGP"
exoi.01 <- "DMSO"
coi <- c("experiment", "time", "radiation", "area", "sd")
tbl.sub.01 <- subset(tbl, analyte==aoi.01 & groupName==exoi.01)[, coi]

exoi.02 <- "ATRi -/+"
tbl.sub.02 <- subset(tbl, analyte==aoi.01 & groupName==exoi.02)[, coi]
#--------------------------------------------------------------------------------------------------------------
ui <- fluidPage(

    div(ExperimentalMeasuresUI(id="expMeasures.01", title="CDC25B_pS160_LLGH",
                               boxHeight=220, boxWidth=360),
       style="width: 380px;  margin: 20px; padding: 10px; border: 1px solid gray; border-radius: 10px;"
       )
)
#----------------------------------------------------------------------------------------------------
server <- function(input, output, session){

    ExperimentalMeasuresServer(id="expMeasures.01", tbl=tbl.sub.01)

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9083)
