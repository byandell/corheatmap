library(shiny)
library(corheatmap)

shinyUI(
  fluidPage(
  titlePanel("Heatmap using Correlation"),
  sidebarPanel(
    corHeatmapUI("cor")
  ),
  mainPanel(
    corHeatmapOutput("cor")
  )
))
