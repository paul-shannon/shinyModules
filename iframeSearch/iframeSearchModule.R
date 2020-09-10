library(shiny)

iframeSearchUI <- function(id, title){

   fluidRow(
      tags$head(tags$script(type="module", src="https://unpkg.com/x-frame-bypass")),
      htmlOutput(NS(id, "iframe"))
      )
   }

iframeSearchServer <- function(input, output, session, website, geneSymbol){

  output$iframe <- renderUI({
     goi <- geneSymbol()
     woi <- tolower(website())
     print(goi)
     print(woi)
     printf("iframeServer, goi: %s   woi: %s", goi, woi)
     url <- switch(woi,
             "genecards"  = "https://www.genecards.org/cgi-bin/carddisp.pl?gene=%s",
             "pubmed"     = "https://pubmed.ncbi.nlm.nih.gov/?term=%s",
             "homologene" = "https://www.ncbi.nlm.nih.gov/homologene/?term=%s",
             "kegg"       = "https://www.genome.jp/dbget-bin/www_bfind_sub?mode=bfind&max_hit=1000&dbkey=kegg&keywords=%s",
             "dbsnp"      = "https://www.ncbi.nlm.nih.gov/snp/?term=%s",
             "google"     = "https://www.google.com/search?q=%s",
             "wiki"       = "http://localhost:3000/%s")

     print(url)
     printf("this is the url picked by switch: '%s'", url)
     uri <- sprintf(url, goi)
     #printf("uri: %s", uri)
     htmlText <- tags$iframe(src=uri, is="x-frame-bypass", height=1000, width="100%")
     htmlText
     })

} # iframeServer


