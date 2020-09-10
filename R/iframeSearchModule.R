#------------------------------------------------------------------------------------------------------------
#' the UI for a shiny module to display (in an iframe) a search of a 3rd party website (pubmed, google, ...)
#'
#' @import shiny
#' @import httr
#' @import jsonlite
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

  printf("--- starting iframeSearchServer")

  output$iframe <- renderUI({
     goi <- geneSymbol()
     woi <- tolower(website())
     print(goi)
     print(woi)
     url <- switch(woi,
             "ucsc"       = "http://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&position=chr1:35000-40000",
             "genecards"  = "https://www.genecards.org/cgi-bin/carddisp.pl?gene=%s",
             "pubmed"     = "https://pubmed.ncbi.nlm.nih.gov/?term=%s",
             "homologene" = "https://www.ncbi.nlm.nih.gov/homologene/?term=%s",
             "kegg"       = "https://www.genome.jp/dbget-bin/www_bfind_sub?mode=bfind&max_hit=1000&dbkey=kegg&keywords=%s",
             "dbsnp"      = "https://www.ncbi.nlm.nih.gov/snp/?term=%s",
             #"gtex:gene"   = "https://www.gtexportal.org/home/gene/%s",
             #"gtex:snp"    = "https://www.gtexportal.org/home/snp/%s",
             "google"     = "https://www.google.com/search?q=%s",
             "rvarbase"   = "http://rv.psych.ac.cn/quickSearch.do?keyword=%s&submit=Search",
             "clinvar"    = "https://www.ncbi.nlm.nih.gov/clinvar/?term=%s[gene]",
             "wiki"       = "http://localhost:9091/%s")

     uri <- sprintf(url, goi)
     if(woi == "ucsc"){
        uri <- sprintf("http://localhost:8000/geneLoc")
        printf("ucsc %s", goi)
        body.jsonString <- sprintf('%s', toJSON(list(gene=goi, genome="hg19", shoulder=100)))
        printf("body.jsonString: ", body.jsonString)
        r <- POST(uri, body=body.jsonString)
        x <- fromJSON(content(r)[[1]])

        uri <- sprintf("http://genome.ucsc.edu/cgi-bin/hgTracks?db=%s&position=%s:%d-%d",
                       "hg19", x$chrom, x$start, x$end)
        browseURL(uri)
        return("")
        }
     if(woi %in% c("wiki")){
        browseURL(uri)
        return("")
        }
     else{
        printf("uri for iframe: %s", uri)
        htmlText <- tags$iframe(src=uri, is="x-frame-bypass", height=1000, width="100%")
        htmlText
        }
     })

} # iframeServer
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))

