library(shinydashboard)
library(shinyModules)
tbl <- get(load(system.file(package="shinyModules", "extdata",
                            "proteomicsExperimentSubsetDataFrame.RData")))
analytes <- sort(unique(tbl$analyte))
aoi.01 <- "LMNB1_pS23_AGGP"
exoi.01 <- "DMSO"
coi <- c("experiment", "time", "radiation", "area", "sd")
tbl.sub.01 <- subset(tbl, analyte==aoi.01 & groupName==exoi.01)[, coi]

exoi.02 <- "ATRi -/+"
tbl.sub.02 <- subset(tbl, analyte==aoi.01 & groupName==exoi.02)[, coi]

#----------------------------------------------------------------------------------------------------
ui.simple <- fluidPage(

    div(ExperimentalMeasuresUI(id="expMeasures.01", title="CDC25B_pS160_LLGH"),
       style="width: 600px; margin: 20px; padding: 10px; border: 1px solid gray; border-radius: 10px;"
       )

) # ui.simple
#----------------------------------------------------------------------------------------------------
ui.dashboard <- dashboardPage(

  dashboardHeader(title = "Proteomic Assays"),
  dashboardSidebar(
      selectInput("selectAnalyte",
                  label="Analyte",
                  c("CDC25B_pS160_LLGH", "LMNB1_pS23_AGGP", "MDM2_pS166_AISE", "TP53_pS315_ALPN")
                  ),
    sidebarMenuOutput("menu")
    ),
  dashboardBody(
      uiOutput("plotBoxDiv")  # creates a div to be filled later

      #fluidRow(
          #box(ExperimentalMeasuresUI(id="expMeasures.01", title=sprintf("%s: %s", aoi.01, exoi.01)),
          #    title=sprintf("%s - %s", aoi.01, exoi.01)),
          #box(ExperimentalMeasuresUI(id="expMeasures.02", title=sprintf("%s: %s", aoi.01, exoi.02)),
          #    title=sprintf("%s - %s", aoi.01, exoi.02))
       #  )
    )

) # ui.dashboard
#----------------------------------------------------------------------------------------------------
server <- function(input, output, session)
{
    observeEvent(input$selectAnalyte, ignoreInit=TRUE,{
       analyte <- input$selectAnalyte;
       printf("new analyte: %s", analyte)
       insertUI("#plotBoxDiv", "afterBegin",
                ui=h5("hello plotBoxDiv"))
       #displayAnalyteDataByExperiment(analyte)
       })

  #callModule(ExperimentalMeasuresServer, "expMeasures.01", tbl.sub.01)
  #callModule(ExperimentalMeasuresServer, "expMeasures.02", tbl.sub.02)

} # server
#----------------------------------------------------------------------------------------------------
displayAnalyteDataByExperiment <- function(analyte.name)
{
   coi <- c("experiment", "time", "radiation", "area", "sd")

   tbl.sub <- subset(tbl, analyte==analyte.name)
   experiments <- unique(tbl.sub$groupName)

   for(experiment in experiments){
      tbl.exp <- subset(tbl.sub, groupName==experiment)[, coi]
      callModule(ExperimentalMeasuresServer, experiment, tbl.exp)
      }

} # displayAnalyteDataByExperiment
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui.dashboard, server), port=9082)
#runApp(shinyApp(ui.simple, server), port=9082)
