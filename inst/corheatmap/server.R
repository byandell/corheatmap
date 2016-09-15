library(shiny)
library(corheatmap)

shinyServer(function(input, output, session) {
  callModule(corHeatmap, "cor")
})
