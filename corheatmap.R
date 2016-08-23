#' Shiny correlation-based heatmap
#'
#'
#' @param input,output,session standard shiny arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
corHeatmap <- function(input, output, session) {
  ns <- session$ns
  
  ## Set up palettes using RColorBrewer::brewer.pal.info
  palettes <- reactive({
    cat <- req(input$category)
    cats <- c(divergent="div",qualitative="qual",sequential="seq")
    if(cat != "all")
      cats <- cats[cat]
    row.names(brewer.pal.info)[brewer.pal.info$colorblind &
                                 brewer.pal.info$category %in% cats]
  })
  output$palette <- renderUI({
    selectInput(ns("palette"), "Palette", palettes())
  })
  
  ## Read in file with name supplied by user.
  file_mx <- reactive({
    file_nm <- req(input$file)
    numrow <- as.integer(req(input$numrow))
    
    out <- switch(file_nm$type,
           ".csv" =, "text/csv" =, 'text/comma-separated-values' =
             read.csv(file_nm$datapath),
           ".tsv" =, 'text/tab-separated-values' = 
             read.csv(file_nm$datapath, sep="\t"),
           {
             cat(stderr(), "only CSV and TSV files accepted\n")
           })
    
    ## Move first column into row names.
    row.names(out) <- paste(seq_len(nrow(out)), out[[1]], sep = ".")
    out <- out[,-1]
    
    ## Eliminate rows with half or more missing.
    out <- out[apply(out, 1, function(x) sum(is.na(x))) < ncol(out) / 2, ]
    
    ## Restrict to first 100 entries
    out[seq_len(numrow),]
})
  
  list(palette = reactive({input$palette}),
       file = file_mx)
}
#' @export
#' @rdname corHeatmap
corHeatmapUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fileInput(ns("file"), "Choose File", 
                accept=c(".csv",".tsv",".xlsx",".xls")),
      textInput(ns("numrow"), "Number of Rows", "500"),
      uiOutput(ns("palette")),
      selectInput(ns("category"), "Type", 
                  c("divergent","qualitative","sequential","all")))
  )
}