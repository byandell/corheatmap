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
    
    out <- corheatmap(out, row_names = 1, 
                  cluster = FALSE, beta = 1, num_rows = numrow)
    
    if(!nrow(out) | is.null(out)) {
      cat(stderr(), "\nno data rows to plot\n")
      return(NULL)
    }
    out
  })

  dist_fun <- function(x) {
    dist_cor(x, as.numeric(req(input$beta)))
  }
  ## Interactive D3 Heatmap.
  output$d3heatmap <- renderD3heatmap({
    pal <- req(input$palette)
    dat <- req(file_mx())
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
              scale = "none",
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
        column(6, textInput(ns("numrow"), "Hi Var Rows", "200")),
        column(6, selectInput(ns("height"), "Height", 
                  c("400","500","600","700","800","900","1000"),
                  "600"))),
      fluidRow(
        column(6, checkboxInput(ns("rowname"), "Row Names", TRUE)),
        column(6, checkboxInput(ns("colname"), "Col Names", TRUE))),
      fluidRow(
        column(6, checkboxInput(ns("cluster"), "Cluster", TRUE)),
        column(6, selectInput(ns("beta"), "(1-cor)^beta", 
                              as.character(c(.1,.2,.3,.5,1,2,3,4,5,6)),
                              "1"))),
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
