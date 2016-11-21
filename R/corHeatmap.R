#' Shiny correlation-based heatmap
#'
#'
#' @param input,output,session standard shiny arguments
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @importFrom shiny selectInput renderUI reactive req
#' @importFrom d3heatmap d3heatmap renderD3heatmap d3heatmapOutput
#' @importFrom stringr str_replace
#' @export
corHeatmap <- function(input, output, session) {
  ns <- session$ns
  
  ## Set up palettes using RColorBrewer::brewer.pal.info
  palettes <- shiny::reactive({
    cat <- req(input$category)
    cats <- c(divergent="div",qualitative="qual",sequential="seq")
    if(cat != "all")
      cats <- cats[cat]
    row.names(RColorBrewer::brewer.pal.info)[
      RColorBrewer::brewer.pal.info$colorblind &
        RColorBrewer::brewer.pal.info$category %in% cats]
  })
  output$palette <- shiny::renderUI({
    shiny::selectInput(ns("palette"), "Palette", palettes())
  })
  
  ## Read in file with name supplied by user.
  file_in <- shiny::reactive({
    file_nm <- shiny::req(input$file)
    
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
  file_mx <- shiny::reactive({
    out <- req(file_in())
    numrow <- as.integer(req(input$numrow))
    
    out <- cor_matrix(out, row_names = 1, 
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
  output$d3heatmap <- d3heatmap::renderD3heatmap({
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
    d3heatmap::d3heatmap(dat,
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
  output$heatmap <- shiny::renderUI({
    height <- req(input$height)
    d3heatmap::d3heatmapOutput(ns("d3heatmap"), 
                               height = paste0(input$height, "px"))
  })
  
  ## Download Heatmap Data.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("new_", 
             stringr::str_replace(basename(req(input$file$name)),
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
      stringr::str_replace(basename(req(input$file$name)), 
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
              col = RColorBrewer::brewer.pal(
                RColorBrewer::brewer.pal.info[pal,"maxcolors"], 
                pal),
              Rowv=Rowv, Colv=Colv, labRow=labRow, labCol=labCol)
      dev.off()
    }
  )
}
#' @param id session identifier
#' @importFrom shiny tagList fluidRow column fileInput textInput 
#' selectInput checkboxInput uiOutput downloadButton NS
#' @export
#' @rdname corHeatmap
corHeatmapUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::fileInput(ns("file"), "Choose File", 
                accept=c(".csv",".tsv",".xlsx",".xls")),
      shiny::fluidRow(
        shiny::column(6, shiny::textInput(ns("numrow"), "Hi Var Rows", "200")),
        shiny::column(6, shiny::selectInput(ns("height"), "Height", 
                  c("400","500","600","700","800","900","1000"),
                  "600"))),
      shiny::fluidRow(
        shiny::column(6, shiny::checkboxInput(ns("rowname"), "Row Names", TRUE)),
        shiny::column(6, shiny::checkboxInput(ns("colname"), "Col Names", TRUE))),
      shiny::fluidRow(
        shiny::column(6, shiny::checkboxInput(ns("cluster"), "Cluster", TRUE)),
        shiny::column(6, shiny::selectInput(ns("beta"), "(1-cor)^beta", 
                              as.character(c(.1,.2,.3,.5,1,2,3,4,5,6)),
                              "1"))),
      shiny::fluidRow(
        shiny::column(6, shiny::uiOutput(ns("palette"))),
        shiny::column(6, shiny::selectInput(ns("category"), "Palette Type", 
                    c("sequential","divergent","qualitative")))),
      shiny::fluidRow(
        shiny::column(6, shiny::downloadButton(ns("downloadData"), "CSV")),
        shiny::column(6, shiny::downloadButton(ns("downloadPlot"), "Plot"))))
  )
}
#' @export
#' @importFrom shiny tagList fluidRow uiOutput NS
#' @rdname corHeatmap
corHeatmapOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::uiOutput(ns("heatmap"))
    )
  )
}
