library(d3heatmap)
library(shiny)
library(RColorBrewer)
library(readxl)
source("corheatmap.R")
dist_fun <- function(x) {
  as.dist((1 - cor(t(x), use = "pairwise.complete.obs")) / 2)
}

ui <- fluidPage(
  h1("Heatmap using Correlation"),
  checkboxInput("cluster", "Apply clustering"),
  corHeatmapUI("cor"),
  d3heatmapOutput("heatmap")
)

server <- function(input, output, session) {
  cor_par <- callModule(corHeatmap, "cor")
  
  dist_df <- reactive({
    req(cor_par$file())
  })
  output$heatmap <- renderD3heatmap({
    pal <- req(cor_par$palette())
    d <- dist_df()
    d3heatmap(d,
      scale = "column",
      colors = pal,
      distfun = dist_fun,
      dendrogram = if (input$cluster) "both" else "none")
  })
}

shinyApp(ui, server)
