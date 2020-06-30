#----------------------------------------------------------------------------------------------------
#' the UI for a ggAnalyteLinePlot shiny module
#'
#' @import shiny
#' @import ggplot2
#'
#' @param id  the html document's widget id
#'
#' @aliases ggAnalyteLinePlotUI
#' @rdname ggAnalyteLinePlotUI
#'
#' @export
#'
ggAnalyteLinePlotUI <- function(id){
    div(textOutput(NS(id, "title")),
        plotOutput(NS(id, "plot"), brush = NS(id, "brush"))
    )
  }

#----------------------------------------------------------------------------------------------------
#' the server for a DataTable shiny module
#'
#' @param input enviroment provide by shiny
#' @param output enviroment provide by shiny
#' @param session enviroment provide by shiny
#' @param plotTitle character string
#' @param data data.frame with area and time columns
#'
#' @aliases ggAnalyteLinePlotServer
#' @rdname ggAnalyteLinePlotServer
#'
#' @export
#'
ggAnalyteLinePlotServer <- function(input, output, session, plotTitle, data) {

  output$title <- renderText({
     plotTitle
     })

  output$plot <- renderPlot({
    plotAnalyte(data())
    })

  selectedRows <- reactive(which(dataWithSelection()$selected_))
  selectedRowNames <- reactive(rownames(dataWithSelection())[selectedRows()])
  #return(list(tbl=dataWithSelection, selectedRows=selected))
  return(list(rows=selectedRows, names=selectedRowNames))
  }

#----------------------------------------------------------------------------------------------------
# private helper function
plotAnalyte <- function(tbl.analyte)
{
   analyteName <- tbl.analyte$analyte[1]
   print(tbl.analyte)
   p <- ggplot(tbl.analyte) +
         geom_line(aes(y = area, x = time, colour = experiment), stat="identity", size=1.5) +
         geom_errorbar(aes(ymin=area-sd, ymax=area+sd, x=time, y=area, colour=experiment), width=0.2) +
         ggtitle(sprintf("%s Time Course", analyteName)) +
         theme(plot.title = element_text(size = 18, face = "bold")) +
         labs(x="Time", y="Ratio") +
         scale_x_continuous(breaks=1:5, labels=c("Mock", "0.25", "1.0", "6", "24"))

   p

} # plotAnalyte
#----------------------------------------------------------------------------------------------------

