#' Correlation-based heatmap app
#'
#' Runs Shiny app for correlation based heatmap, built on d3heatmap.
#' 
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
heatmapApp <- function() {
  runApp(system.file("corheatmap/", package = "corheatmap"))
}