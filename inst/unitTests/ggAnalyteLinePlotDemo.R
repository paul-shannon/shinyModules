library(shinyModules)
#--------------------------------------------------------------------------------------------------------------
# the first data.frame is optimized for ggplot.  the second is the source data.frame,
# from the original spreadsheet jeff gave me

load(system.file(package="shinyModules", "extdata", "tblAnalytes-ggplot-friendly.RData")) # tbl.friendly
load(system.file(package="shinyModules", "extdata", "tbl.proteomics.orig.RData"))         # tbl.orig
#--------------------------------------------------------------------------------------------------------------
ui <- fluidPage(

      div(ggAnalyteLinePlotUI("plot1"),
          style="margin: 50px; padding: 10px; padding-right: 20px; border: 3px solid black; border-radius: 10px;"),
      div(dataTableUI("analyteTable"),
          style="margin: 20px; padding: 10px; border: 3px solid black; border-radius: 10px;"),
      messageBoxUI(id="messageBox.tbl", title=""),
      )

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session){

   roi <- callModule(dataTableServer, "analyteTable",
                     tbl=tbl.orig,
                     selectionPolicy="single",
                     pageLength=20,
                     visibleRows = reactive("all"))

   observe({
       printf("entering observer block")
       row.name <- roi()
       if(length(row.name) > 0){
          if(!is.null(row.name) & nchar(row.name) > 0){
            analyte.name <- tbl.orig$PeptideLabel[as.integer(row.name)]
            printf("analyte.name: %s", analyte.name)
            callModule(messageBoxServer, "messageBox.tbl", newContent=reactive(analyte.name))
            tbl.sub <- subset(tbl.friendly, analyte==analyte.name)
            new.order <- order(tbl.sub$time, decreasing=FALSE)
            tbl.sub <- tbl.sub[new.order,]
            print(tbl.sub)
            callModule(ggAnalyteLinePlotServer, "plot1",
                       plotTitle="",
                       data=reactive(tbl.sub))
            }
         } # if > 0
      })

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9034)

