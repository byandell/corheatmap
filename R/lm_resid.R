#' adjust values based on factors
#'
#' @param form formula
#' @param object data.frame or tbl_df
#' @param center center on mean if \code{TRUE}
#' @param group column to group by (or not if \code{NULL})
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @examples 
#' lm_resid(mpg ~ wt + cyl, mtcars)
#'
#' @importFrom dplyr ungroup do group_by_ mutate
#' @importFrom broom tidy
#' @export
lm_resid <- function(form, object, center=TRUE, group=NULL) {
  form <- formula(form)
  if(is.null(group)) {
    lm_resid_one(object, form, center)
  } else {
    dplyr::ungroup(
      dplyr::do(
        dplyr::group_by_(object, group), 
        lm_resid_one(., form, center)))
  }
}
lm_resid_one <- function(object, form, center) {
  
  if(center) {
    offset <- mean(object[[response]], na.rm = TRUE)
  } else {
    offset <- 0
  }

  # Funky way to get response and column names
  response <- as.character(form[2])
  col_names <- dimnames(attr(terms(form), "factors"))[[1]]
  # Is anything in row used for lm fit missing?
  is_na <- apply(object[,col_names],1, function(x) any(is.na(x)))
  names(is_na) <- row.names(object)
  
  myresid <- function(fit, is_na) {
    # Fills back out to full data set
    out <- rep(NA, length(is_na))
    out[!is_na] <- resid(fit)
    names(out) <- names(is_na)
    out
  }
  
  setNames(
    dplyr::mutate(
      broom::tidy(
        myresid(lm(form, data=object), is_na)
        ),
      x = x + offset),
    c("names", response))
}
