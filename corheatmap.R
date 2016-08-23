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
  
  file_mx <- reactive({
    browser()
    file_nm <- req(input$file)
    numrow <- as.integer(req(input$numrow))
    switch(file_nm$type,
           ".csv" =, "text/csv" =, 'text/comma-separated-values' =
             read.csv(file_nm$datapath, row.names=1),
           ".tsv" =, 'text/tab-separated-values' = 
             read.csv(file_nm$datapath, sep="\t", row.names=1),
           ".xls" =, ".xlsx" =, 
           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = {
             browser()
             out <- read_excel(file_nm$datapath)
             ## Move first column into row names.
             row.names(out) <- paste(seq_len(nrow(out)), out[[1]], sep = ".")
             out <- out[,-1]
             ## Eliminate rows with half or more missing.
             out <- out[apply(out, 1, function(x) sum(is.na(x))) < ncol(out) / 2, ]
             ## Restrict to first 100 entries
             out[seq_len(numrow),]
           })
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
      textInput(ns("numrow"), "Number of Rows", "100"),
      uiOutput(ns("palette")),
      selectInput(ns("category"), "Type", 
                  c("divergent","qualitative","sequential","all")))
  )
}