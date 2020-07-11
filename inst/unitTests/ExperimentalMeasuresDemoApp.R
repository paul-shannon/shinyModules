library(shinydashboard)
library(shinyModules)
#------------------------------------------------------------------------------------------------------------------------
# inspired by, and code stealing from:
#   https://roh.engineering/post/shiny-add-removing-modules-dynamically/   # linear models for mtcars mpg
#   https://www.ardata.fr/en/post/2019/07/01/dynamic-module-call/
#   https://mastering-shiny.org/scaling-modules.html
# see 2018 very similar module/shiny app:
#    https://www.blog.cultureofinsight.com/2018/01/reproducible-shiny-app-development-with-modules/
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
                  label="Choose Analyte",
                  c(" - ", "CDC25B_pS160_LLGH", "LMNB1_pS23_AGGP", "MDM2_pS166_AISE", "TP53_pS315_ALPN")
                  ),
    sidebarMenuOutput("menu")
    ),
  dashboardBody(
    fluidRow(uiOutput("plotBoxDiv"))  # creates a div to be filled later
    )

) # ui.dashboard
#----------------------------------------------------------------------------------------------------
server <- function(input, output, session)
{
    observeEvent(input$selectAnalyte, ignoreInit=TRUE,{
       analyte <- input$selectAnalyte;
       if(analyte != " - "){
          printf("new analyte: %s", analyte)
          displayAnalyteDataByExperiment(analyte)
          }
       })

} # server
#----------------------------------------------------------------------------------------------------
displayAnalyteDataByExperiment <- function(analyte.name)
{
   tbl.sub <- subset(tbl, analyte==analyte.name) # [, coi] #  & groupName==exoi.01)[, coi]
   experiment.groups <- sort(unique(tbl.sub$groupName))
   printf("--- experiment groups: %d", length(experiment.groups))
   print(experiment.groups)

   removeUI("#temporaryDiv")

   for(experiment.name in experiment.groups){
     coi <- c("time", "radiation", "area", "sd")
     tbl.exp <- subset(tbl.sub, groupName==experiment.name)[, coi]
     tbl.exp
     box.title <- sprintf("%s: %s", analyte.name, experiment.name)
     box.id <- sprintf("%s-%s", analyte.name, experiment.name)
     insertUI("#plotBoxDiv", "beforeEnd", div(id="temporaryDiv"))
     insertUI("#temporaryDiv", "beforeEnd",
              box(ExperimentalMeasuresUI(id=box.id, title=box.title),
                  title=box.title, #experiment.name,
                  width=4,
                  solidHeader=TRUE))
     ExperimentalMeasuresServer(id=box.id, tbl=tbl.exp)
     }

} # displayAnalyteDataByExperiment
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui.dashboard, server), port=9082)
#runApp(shinyApp(ui.simple, server), port=9082)
