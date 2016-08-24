#' Reorder heatmap
#'
#' @param x matrix of values
#' @param distfun distance function for \code{x}
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
heatmap_reorder <- function(x, distfun) {
  ## Pulled from R/stats heatmap()
  Rowv <- rowMeans(x, na.rm = TRUE)
  Colv <- colMeans(x, na.rm = TRUE)
  dd <- as.dendrogram(hclust(distfun(x)))
  dd <- reorder(dd, Rowv)
  if (nrow(x) != length(rowInd <- order.dendrogram(dd))) 
    stop("row dendrogram ordering gave index of wrong length")
  dd <- as.dendrogram(hclust(distfun(t(x))))
  dd <- reorder(dd, Colv)
  if (ncol(x) != length(colInd <- order.dendrogram(dd))) 
    stop("column dendrogram ordering gave index of wrong length")
  x[rev(rowInd), colInd]
}