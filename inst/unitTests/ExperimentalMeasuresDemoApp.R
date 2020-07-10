library(shinydashboard)
library(shinyModules)
#------------------------------------------------------------------------------------------------------------------------
# inspired by, and code stealing from:
#   https://roh.engineering/post/shiny-add-removing-modules-dynamically/   # linear models for mtcars mpg
#   https://www.ardata.fr/en/post/2019/07/01/dynamic-module-call/
#   https://mastering-shiny.org/scaling-modules.html
#
#------------------------------------------------------------------------------------------------------------------------
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
       displayAnalyteDataByExperiment(analyte)
       })

  #callModule(ExperimentalMeasuresServer, "expMeasures.01", tbl.sub.01)
  #callModule(ExperimentalMeasuresServer, "expMeasures.02", tbl.sub.02)

} # server
#----------------------------------------------------------------------------------------------------
displayAnalyteDataByExperiment <- function(analyte.name)
{
   coi <- c("experiment", "time", "radiation", "area", "sd")
   tbl.sub <- subset(tbl, analyte==analyte.name) #  & groupName==exoi.01)[, coi]
   experiment.groups <- sort(unique(tbl.sub$groupName))
   printf("--- experiment groups: %d", length(experiment.groups))
   print(experiment.groups)

   experiment.groups <- c("DMSO", "ATMi")

   for(experiment.name in experiment.groups[1:2]){
     tbl.exp <- subset(tbl.sub, groupName==experiment.name)
     box.title <- sprintf("%s: %s", analyte.name, experiment.name)
     box.id <- sprintf("%s-%s", analyte.name, experiment.name)
     printf("========= subsetting on experiment: '%s'   box.id: %s", experiment.name, box.id)
     printf("tbl subsetted by analyte and experiment group name, nrow: %d", nrow(tbl.exp))
     printf("box.title: %s", box.title)
     printf("box.id:    %s", box.id)
     insertUI("#plotBoxDiv", "beforeEnd",
              box(ExperimentalMeasuresUI(id=box.id, title=box.title), width=4, solidHeader=TRUE))
     printf("dim(tbl.exp): %d, %d", nrow(tbl.exp), ncol(tbl.exp))
     callModule(ExperimentalMeasuresServer, box.id, tbl.exp)
     }
#   coi <- c("experiment", "time", "radiation", "area", "sd")
#
#   tbl.sub <- subset(tbl, analyte==analyte.name)
#   experiments <- unique(tbl.sub$groupName)
#
#   for(experiment in experiments){
#      tbl.exp <- subset(tbl.sub, groupName==experiment)[, coi]
#      callModule(ExperimentalMeasuresServer, experiment, tbl.exp)
#      }

} # displayAnalyteDataByExperiment
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui.dashboard, server), port=9082)
#runApp(shinyApp(ui.simple, server), port=9082)
