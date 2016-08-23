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
    file_nm <- req(input$file)
    switch(file_nm$type,
           ".csv" =, "text/csv" =, 'text/comma-separated-values' =
             read.csv(file_nm$datapath, row.names=1),
           ".tsv" =, 'text/tab-separated-values' = {}
             read.csv(file_nm, sep="\t", row.names=1),
           ".xls" =, ".xlsx" = {
             out <- read_excel(file_nm)
             row.names(out) <- paste(seq_len(nrow(out)), out[[1]], sep = ".")
             out[,-1]
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
      uiOutput(ns("palette")),
      selectInput(ns("category"), "Type", 
                  c("divergent","qualitative","sequential","all")))
  )
}