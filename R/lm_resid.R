#' adjust values based on factors
#'
#' @param object data.frame or tbl_df
#' @param form formula
#' @param center center on mean if \code{TRUE}
#' @param group column to group by (or not if \code{NULL})
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
lm_resid <- function(object, form, center=TRUE, group=NULL) {
  if(is.null(group))
    lm_resid_one(object, form)
  
  object %>%
    ## Do ANOVA by each group level.
    group_by_(group) %>%
    lm_resid_one(form, center)
}
lm_resid_one <- function(object, form, center) {
  full_join(data.frame(names=as.character(seq(nrow(object)))), tmp)
  
  do(tidy(resid(lm(form, data=object)))) %>%
    ## Problem here is that resid for missing data are not included.
  ## Need to reduce to non-missing data first.
  ## Append kazu residuals.
  kazu_resid <- kazu %>%
    do(tidy(resid(lm(lesion ~ strain, data=.))))
  kazu$resid <- kazu_resid$x
  
  ## Filter to at least two strains and no missing data.
  rpkg <- rpkg %>%
    enough_data %>%
    filter(!is.na(signal))
  ## Do ANOVA by each EC level.
  rpkg_resid <- rpkg %>%
    group_by(EC) %>%
    do(tidy(resid(lm(signal ~ strain, data=.))))
  rpkg <- rpkg %>%
    arrange(EC) %>%
    mutate(resid = rpkg_resid$x)
  ## Now correlation of residuals.
  cor(rpkg %>%
        select(-signal) %>%
        spread(EC,resid) %>%
        select(-mouse,-strain),
      kazu$resid,
      use = "pairwise.complete.obs")
}
