library(shinydashboard)
library(shinyModules)
tbl <- get(load(system.file(package="shinyModules", "extdata",
                            "proteomicsExperimentSubsetDataFrame.RData")))
analytes <- sort(unique(tbl$analyte))
aoi.01 <- "LMNB1_pS23_AGGP"
exoi.01 <- "DMSO"
tbl.sub.01 <- subset(tbl, analyte==aoi.01 & groupName==exoi.01)  # has good distribution of area

exoi.02 <- "ATRi -/+"
tbl.sub.02 <- subset(tbl, analyte==aoi.01 & groupName==exoi.02)  # has good distribution of area

#----------------------------------------------------------------------------------------------------
ui.simple <- fluidPage(

    div(ExperimentalMeasuresUI(id="expMeasures.01", title="CDC25B_pS160_LLGH"),
       style="width: 600px; margin: 20px; padding: 10px; border: 1px solid gray; border-radius: 10px;"
       )

) # ui.simple
#----------------------------------------------------------------------------------------------------
ui.dashboard <- dashboardPage(

  dashboardHeader(title = "Dynamic sidebar"),
  dashboardSidebar(
    sidebarMenuOutput("menu")
    ),
  dashboardBody(
      fluidRow(
          box(ExperimentalMeasuresUI(id="expMeasures.01", title=sprintf("%s: %s", aoi.01, exoi.01)),
              title=sprintf("%s - %s", aoi.01, exoi.01)),
          box(ExperimentalMeasuresUI(id="expMeasures.02", title=sprintf("%s: %s", aoi.01, exoi.02)),
              title=sprintf("%s - %s", aoi.01, exoi.02))
         )
    )

) # ui.dashboard
#----------------------------------------------------------------------------------------------------
server <- function(input, output, session)
{
  callModule(ExperimentalMeasuresServer, "expMeasures.01", tbl.sub.01)
  callModule(ExperimentalMeasuresServer, "expMeasures.02", tbl.sub.02)

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui.dashboard, server), port=9082)
#runApp(shinyApp(ui.simple, server), port=9082)
