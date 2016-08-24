library(d3heatmap)
library(shiny)
library(RColorBrewer)
library(readxl)
library(stringr)
source("corheatmap.R")
dist_fun <- function(x) {
  as.dist((1 - cor(t(x), use = "pairwise.complete.obs")) / 2)
}

ui <- fluidPage(
  titlePanel("Heatmap using Correlation"),
  sidebarPanel(
    checkboxInput("cluster", "Apply clustering", TRUE),
    corHeatmapUI("cor")
  ),
  mainPanel(
    d3heatmapOutput("heatmap")
  )
)

server <- function(input, output, session) {
  cor_par <- callModule(corHeatmap, "cor",
                        cluster = reactive({input$cluster}))
  
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
      anim_duration = 0,
      brush_color = "#000000",
      dendrogram = if (input$cluster) "both" else "none")
  })
}

shinyApp(ui, server)
