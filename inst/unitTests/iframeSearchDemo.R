library(shinyModules)
#----------------------------------------------------------------------------------------------------
ui <- fluidPage(
        selectInput("geneSelector", "Gene:", c("", "Myc", "APOE", "bogus", "rs61825286")),
        selectInput("websiteSelector", "Website:",
                    c("GeneCards", "HomoloGene", "PubMed", "dbSNP", "google",
                      #"GTEx:Gene", "GTEx:SNP",
                      "rVarBase", "ClinVar", "comments")),
         div(iframeSearchUI(id="iframe", title="fubar"),
           style="margin: 10px; margin-bottom: 30px; padding: 10px;"),
        )

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session)
{
   newPage <- reactive({
      list(input$geneSelector, input$websiteSelector)
      })

  observeEvent(newPage(), ignoreInit=TRUE, { # input$geneSelector, ignoreInit=TRUE, {
     goi <- input$geneSelector
     woi <- input$websiteSelector
     if(nchar(goi) > 0)
        callModule(iframeSearchServer, "iframe",  website=reactive(woi), geneSymbol=reactive(goi))
     })

} # server
#----------------------------------------------------------------------------------------------------
runApp(shinyApp(ui, server), port=9037)
