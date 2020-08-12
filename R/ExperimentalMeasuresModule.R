#----------------------------------------------------------------------------------------------------
#' the UI for a very simple message box module
#'
#' @import shiny
#'
#' @param id  character string, the html document's widget id
#' @param title  character string, title for the box
#' @param boxWidth integer pixels, 200 by default
#' @param boxHeight  integer pixels, 30 by default
#' @param fontSize  integer pixels, 20 by default
#' @param backgroundColor character string, uses standard CSS naming conventions, "beige" by default
#'
#' @aliases ExperimentalMeasuresUI
#' @rdname ExperimentalMeasuresUI
#'
#' @export
#'
ExperimentalMeasuresUI <- function(id, title, boxHeight=300, boxWidth=320, fontSize=20, backgroundColor="beige"){

   tabsetPanel(id=NS(id, "tabset"), type = "tabs",
               tabPanel("Plot",
                        plotOutput(NS(id, "barplot"), height=boxHeight, width=boxWidth)),
               tabPanel("Table",
                        tableOutput(NS(id, "dataTable")),
                        style = sprintf("height:%dpx; width: %dpx; overflow-y: scroll;overflow-x: scroll;",
                                        boxHeight, boxWidth)),
               selected="Plot"
    )
}
#----------------------------------------------------------------------------------------------------
#' the server for a ExperimentalMesures shiny module
#'
#' @param id html element identifier
#' @param tbl, a data.frame
#'
#' @aliases ExperimentalMeasuresServer
#' @rdname ExperimentalMeasuresServer
#'
#' @export
#'
ExperimentalMeasuresServer <- function(id, tbl) {

    moduleServer(id, function(input, output, session){
        doNotKnowWhyThisIsNeeded <- tbl    # a bug in the new shiny 1.5.0? (11 jul 2020)
        output$dataTable <- renderTable(tbl)

           # preserve the incoming order of the data.frame
        tbl$variable <- factor(tbl$variable, levels=tbl$variable)
        output$barplot <- renderPlot({
            ggplot(tbl, aes(x=variable, y=mean, fill=variable)) +
                geom_bar(position="dodge", stat="identity") +
                geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.4, position=position_dodge(.9)) +
                #ggtitle(title)
                scale_fill_manual(values=tbl$color) +
                theme(text = element_text(size=20, face="bold"),
                      axis.text.x = element_blank(), # element_text(angle = 45, hjust = 1.2, vjust=1.2),
                      plot.title = element_text(color="navajowhite4", size=20, face="bold.italic", hjust=0.5),
                      axis.title.x =  element_blank(),
                      axis.title.y = element_text(color="dimgray", size=20, face="bold"),
                      legend.text = element_text(colour="black", size=10, face="bold"))
            })
        #output$barplot <- renderPlot({
        #    tbl$x.axis.value <- seq_len(nrow(tbl))
        #    ggplot(data=tbl, aes(x=x.axis.value, y=area)) + geom_bar(stat='identity') + ylim(0, yMax) + xlab("time or condition")
        #    })
        }) # moduleServer

    } # ExperimentalMeasuresServer
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))


