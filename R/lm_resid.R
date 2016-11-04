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
#' @export
lm_resid <- function(form, object, center=TRUE, group=NULL) {
  form <- formula(form)
  if(is.null(group)) {
    lm_resid_one(object, form, center)
  } else {
    object %>%
      ## Do ANOVA by each group level.
      group_by_(group) %>%
      do(lm_resid_one(form, center)) %>%
      ungroup
  }
}
lm_resid_one <- function(object, form, center) {
  response <- as.character(form[2])
  if(center) {
    offset <- mean(object[[response]])
  } else {
    offset <- 0
  }
  tidy(resid(lm(form, data=object))) %>%
    mutate(x = x + offset) %>%
    setNames(c("names", response))
}
