#------------------------------------------------------------------------------------------------------------
#' the UI for a shiny module to display (in an iframe) a search of a 3rd party website (pubmed, google, ...)
#'
#' @import shiny
#'
#' @param id  the html document's widget id
#'
#' @aliases iframeSearchUI
#' @rdname iframeSearchUI
#'
#' @export
#'
#----------------------------------------------------------------------------------------------------
iframeSearchUI <- function(id, title){

   fluidRow(
      tags$head(tags$script(type="module", src="https://unpkg.com/x-frame-bypass")),
      htmlOutput(NS(id, "iframe"))
      )
   }
#----------------------------------------------------------------------------------------------------
#' the server for a DataTable shiny module
#'
#' @param input enviroment provide by shiny
#' @param output enviroment provide by shiny
#' @param session enviroment provide by shiny
#' @param website character string, one of genecards, pubmed, homologene, kegg, dbsnp, google
#' @param searchTerm character string, eg a geneSymbol, an rsid, ...
#'
#' @aliases iframeSearchServer
#' @rdname iframeSearchServer
#'
#' @export
#'
iframeSearchServer <- function(input, output, session, website, geneSymbol){

  output$iframe <- renderUI({
     goi <- geneSymbol()
     woi <- tolower(website())
     url <- switch(woi,
             "genecards"  = "https://www.genecards.org/cgi-bin/carddisp.pl?gene=%s",
             "pubmed"     = "https://pubmed.ncbi.nlm.nih.gov/?term=%s",
             "homologene" = "https://www.ncbi.nlm.nih.gov/homologene/?term=%s",
             "kegg"       = "https://www.genome.jp/dbget-bin/www_bfind_sub?mode=bfind&max_hit=1000&dbkey=kegg&keywords=%s",
             "dbsnp"      = "https://www.ncbi.nlm.nih.gov/snp/?term=%s",
             "google"     = "https://www.google.com/search?q=%s",
             "rvarbase"   = "http://rv.psych.ac.cn/quickSearch.do?keyword=%s&submit=Search",
             "comments"   = "https://docs.google.com/document/d/e/2PACX-1vRJqN1rnkywno3WeiV16-gVF50KlIR81Xztbl7ZDtqaqVqtWEzckdXFppb2N-kWgEGupnzmY8tY_cSt/pub?embedded=true")

     uri <- sprintf(url, goi)
     htmlText <- tags$iframe(src=uri, is="x-frame-bypass", height=1000, width="100%")
     htmlText
     })

} # iframeServer
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))

