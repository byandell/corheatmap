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
  file_in <- reactive({
    file_nm <- req(input$file)
    
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
    out
  })
  file_mx <- reactive({
    out <- req(file_in())
    numrow <- as.integer(req(input$numrow))
    
    ## Move first column into row names.
    row.names(out) <- paste(seq_len(nrow(out)), out[[1]], sep = ".")
    out <- out[,-1]
    
    ## Eliminate rows with half or more missing.
    out <- out[apply(out, 1, function(x) sum(is.na(x))) < ncol(out) / 2, ]
    if(!nrow(out)) {
      cat(stderr(), "\nall rows have missing data\n")
      return(NULL)
    }
    
    ## Order by decreasing variability.
    out <- out[-apply(out,1,var, na.rm=TRUE),]
    
    ## Restrict to first numrow entries
    nout <- min(numrow, nrow(out))
    if(nout < 5) {
      cat(stderr(), "\nmust have at least 5 rows with little missing data\n")
      return(NULL)
    }
    as.matrix(out[seq_len(nout),])
  })

  dist_fun <- function(x) {
    beta <- as.numeric(req(input$beta))
    if(!is.numeric(beta))
      beta <- 1
    if(beta < 0)
      beta <- 1
    if(beta > 6)
      beta <- 6
    dist_cor(x, beta)
  }
  ## Interactive D3 Heatmap.
  output$d3heatmap <- renderD3heatmap({
    pal <- req(input$palette)
    dat <- file_mx()
    if(input$rowname) {
      labRow <- rownames(dat)
      yaxis_width = 120
    } else {
      labRow <- rep("",nrow(dat))
      yaxis_width = 1
    }
    if(input$colname) {
      labCol <- colnames(dat)
      xaxis_height = 80
    } else {
      labCol <- rep("",ncol(dat))
      xaxis_height = 1
    }
    d3heatmap(dat,
              scale = "column",
              colors = pal,
              distfun = dist_fun,
              anim_duration = 0,
              brush_color = "#000000",
              labCol=labCol, labRow=labRow,
              xaxis_height = xaxis_height,
              yaxis_width = yaxis_width,
              dendrogram = if (input$cluster) "both" else "none")
  })
  output$heatmap <- renderUI({
    height <- req(input$height)
    d3heatmapOutput(ns("d3heatmap"), height = paste0(input$height, "px"))
  })
  
  ## Download Heatmap Data.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("new_", 
             str_replace(basename(req(input$file$name)),
                         "\\.[tc]sv", ".csv")) },
    content = function(file) {
      dat <- req(file_mx())
      if(input$cluster) 
        dat <- heatmap_reorder(dat, dist_fun)
      write.csv(dat, file)
    }
  )
  ## Download Heatmap Plot.
  output$downloadPlot <- downloadHandler(
    filename = function() {
      str_replace(basename(req(input$file$name)), 
                  "\\.[tc]sv", ".pdf") },
    content = function(file) {
      dat <- req(file_mx())
      pal <- req(input$palette)
      if(input$cluster) {
        Rowv <- Colv <- NULL
      } else {
        Rowv <- Colv <- NA
      }
      margins <- c(5,5)
      if(input$rowname) {
        labRow <- NULL
      } else {
        labRow <- NA
        margins[2] <- 0
      }
      if(input$colname) {
        labCol <- NULL
      } else {
        labCol <- NA
        margins[1] <- 0
      }
      pdf(file)
      heatmap(dat, scale = "column",
              distfun = dist_fun, 
              na.rm = FALSE,
              margins = margins,
              col = brewer.pal(brewer.pal.info[pal,"maxcolors"], pal),
              Rowv=Rowv, Colv=Colv, labRow=labRow, labCol=labCol)
      dev.off()
      heatmap_obj <- reactive({out})
    }
  )
}
#' @param id session identifier
#' @export
#' @rdname corHeatmap
corHeatmapUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      fileInput(ns("file"), "Choose File", 
                accept=c(".csv",".tsv",".xlsx",".xls")),
      fluidRow(
        column(6, textInput(ns("numrow"), "# Rows", "200")),
        column(6, selectInput(ns("height"), "Height", 
                  c("400","500","600","700","800","900","1000"),
                  "600"))),
      fluidRow(
        column(6, checkboxInput(ns("rowname"), "Row Names", TRUE)),
        column(6, checkboxInput(ns("colname"), "Col Names", TRUE))),
      fluidRow(
        column(6, checkboxInput(ns("cluster"), "Cluster", TRUE)),
        column(6, textInput(ns("beta"), "Beta", "1"))),
      fluidRow(
        column(6, uiOutput(ns("palette"))),
        column(6, selectInput(ns("category"), "Palette Type", 
                    c("sequential","divergent","qualitative")))),
      fluidRow(
        column(6, downloadButton(ns("downloadData"), "CSV")),
        column(6, downloadButton(ns("downloadPlot"), "Plot"))))
  )
}
#' @export
#' @rdname corHeatmap
corHeatmapOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      uiOutput(ns("heatmap"))
    )
  )
}
