#' Correlation-based distance
#'
#' @param x matrix to compute correlation
#' @param beta exponent to attenuate correlation
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
dist_cor <- function(x, beta = 1) {
  if(!is.numeric(beta))
    beta <- 1
  if(beta < 0)
    beta <- 1
  if(beta > 6)
    beta <- 6
  as.dist(((1 - cor(t(x), use = "pairwise.complete.obs")) / 2) ^ beta)
}
