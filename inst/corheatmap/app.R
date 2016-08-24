library(corheatmap)

ui <- fluidPage(
  titlePanel("Heatmap using Correlation"),
  sidebarPanel(
    corHeatmapUI("cor")
  ),
  mainPanel(
    corHeatmapOutput("cor")
  )
)

server <- function(input, output, session) {
  callModule(corHeatmap, "cor")
}

shinyApp(ui, server)
