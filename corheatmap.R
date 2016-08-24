#' Shiny correlation-based heatmap
#'
#'
#' @param input,output,session standard shiny arguments
#' @param cluster reactive arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
corHeatmap <- function(input, output, session, 
                       cluster) {
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
             NULL
           })
    if(!nrow(out))
      return(NULL)
    
    ## Move first column into row names.
    row.names(out) <- paste(seq_len(nrow(out)), out[[1]], sep = ".")
    out <- out[,-1]
    
    ## Eliminate rows with half or more missing.
    out <- out[apply(out, 1, function(x) sum(is.na(x))) < ncol(out) / 2, ]
    if(!nrow(out))
      return(NULL)
    
    ## Order by decreasing variability.
    out <- out[-apply(out,1,var, na.rm=TRUE),]
    
    ## Restrict to first numrow entries
    out <- out[seq_len(min(numrow, nrow(out))),]
    if(!nrow(out))
      out <- NULL
    
    out
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      file.path(str_replace(basename(req(input$file$name)), 
                            "\\.[tc]sv", ".pdf")) },
    content = function(file) {
      dat <- as.matrix(req(file_mx()))
      pal <- req(input$palette)
      clus.type <- req(cluster())
      if(is.logical(clus.type))
        clus.type <- c("none","both")[1 + clus.type]
      switch(clus.type,
             none = {
               Rowv <- Colv <- NA
             },
             row = {
               Rowv <- NULL
               Colv <- NA
             },
             row = {
               Rowv <- NA
               Colv <- NULL
             },
             {
               Rowv <- Colv <- NULL
             }
             )
      dist_fun <- function(x) {
        as.dist((1 - cor(t(x), use = "pairwise.complete.obs")) / 2)
      }
      pdf(file)
      heatmap(dat, scale = "column",
              distfun = dist_fun, 
              col = brewer.pal(brewer.pal.info[pal,"maxcolors"], pal),
              Rowv=Rowv, Colv=Colv)
      dev.off()
    }
  )
  
  
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
      textInput(ns("numrow"), "Most Variable Rows", "200"),
      uiOutput(ns("palette")),
      selectInput(ns("category"), "Palette Type", 
                  c("divergent","qualitative","sequential","all")),
      downloadButton(ns("downloadPlot"), "Plot"))
  )
}