#' Correlation-based heatmap
#'
#' @param df data frame with matrix to be plotted
#' @param row_names column number or name with row names, or vector of row names (default column 1 if no rownames on \code{df})
#' @param cluster cluster rows and columns if \code{TRUE} (default)
#' @param beta attenuation parameter for clustering (default \code{1})
#' @param num_rows number of rows to plot
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @examples
#' cormap(mtcars)
#'
#' @export
cormap <- function(df, row_names = is.null(rownames(df)), 
                   cluster = TRUE, beta = 1, num_rows = 500) {
  
  ## Set up row names for matrix
  if(!is.null(row_names)) {
    if(length(row_names) == 1) {
      if(row_names != 0) {
        rownames(df) <- df[,row_names]
        df <- df[,-row_names]
      }
    } else {
      if(length(row_names) == nrow(df) & is.character(row_names))
        rownames(df) <- row_names
      else
        stop("row_names must be column number or vector of row names")
    }
  }
  
  ## Make into matrix; check if numeric
  df <- as.matrix(df)
  if(!is.numeric(df))
    stop("one or more columns of df not numeric")
  
  ## Eliminate rows with half or more missing.
  df <- df[apply(df, 1, function(x) sum(is.na(x))) < ncol(df) / 2,, 
           drop = FALSE ]
  if(!nrow(df))
    stop("all rows have missing data")

  ## Order by decreasing variability.
  df <- df[order(-apply(df, 1, var, na.rm=TRUE)),]
  
  ## Restrict to first numrow entries
  nout <- min(num_rows, nrow(df))
  if(nout < 5)
    stop("must have at least 5 rows with little missing data")
  df <- df[seq_len(nout),]

  if(cluster) 
    df <- heatmap_reorder(df, function(x) dist_cor(x, beta))
  
  attr(df, "beta") <- beta
  attr(df, "cluster") <- cluster
  class(df) <- c("cormap", class(df))
  df
}
#' @param rowname include row names in plot if \code{TRUE}
#' @param colname include column names in plot if \code{TRUE}
#' @param cluster cluster rows and columns if \code{TRUE} (default)
#' @param beta attenuation parameter for clustering (default \code{1})
#' @param category category of \code{RColorBrewer} palettes (see \code{\link[RColorBrewer]{brewer.pal.info}})
#' @param d3map use d3heatmap interactive plot if \code{TRUE}
#' @param palette colorblind friendly palette to plot (see \code{\link[RColorBrewer]{brewer.pal.info}})
#' @param ... other plot parameters (ignored)
#' 
#' @examples
#' plot(cormap(mtcars), d3map=FALSE)
#' 
#' @export
#' @method plot cormap
#' @rdname cormap
plot.cormap <- function(x, 
                        rowname = TRUE, colname = TRUE,
                        cluster = attr(x, "cluster"), 
                        beta = attr(x, "beta"),
                        category = c("div","seq","qual"),
                        d3map = interactive(),
                        palette = palettes, 
                        ...) {
  
  ## Set up palettes using RColorBrewer::brewer.pal.info
  category <- match.arg(category, several.ok=TRUE)
  palettes <- 
    rownames(brewer.pal.info)[brewer.pal.info$colorblind &
                                brewer.pal.info$category %in% category]
  palette <- match.arg(palette[1], palettes)
  
  dist_fun <- function(x) dist_cor(x, beta)
  
  if(d3map) {
    ## Set up row and column names
    if(rowname) {
      labRow <- rownames(x)
      yaxis_width = 120
    } else {
      labRow <- rep("",nrow(x))
      yaxis_width = 1
    }
    if(colname) {
      labCol <- colnames(x)
      xaxis_height = 80
    } else {
      labCol <- rep("",ncol(x))
      xaxis_height = 1
    }
    d3heatmap(x,
              scale = "column",
              colors = palette,
              distfun = dist_fun,
              anim_duration = 0,
              brush_color = "#000000",
              labCol=labCol, labRow=labRow,
              xaxis_height = xaxis_height,
              yaxis_width = yaxis_width,
              dendrogram = if (cluster) "both" else "none")
  } else {
    if(cluster) {
      Rowv <- Colv <- NULL
    } else {
      Rowv <- Colv <- NA
    }
    margins <- c(5,5)
    if(rowname) {
      labRow <- NULL
    } else {
      labRow <- NA
      margins[2] <- 0
    }
    if(colname) {
      labCol <- NULL
    } else {
      labCol <- NA
      margins[1] <- 0
    }
    heatmap(x, scale = "column",
            distfun = dist_fun, 
            na.rm = FALSE,
            margins = margins,
            col = brewer.pal(brewer.pal.info[palette,"maxcolors"], palette),
            Rowv=Rowv, Colv=Colv, labRow=labRow, labCol=labCol)
  }
}
