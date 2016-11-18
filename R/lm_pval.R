#' anova p-values grouped by column
#'
#' @param form formula
#' @param object data.frame or tbl_df
#' @param group column to group by (or not if \code{NULL})
#' @param type type of p-values (one of c("drop1","anova"))
#' @param inter test for interactions if \code{TRUE}
#' @param digits number of significant digits (default \code{3})
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#' 
#' @examples
#' lm_pval(mpg ~ wt + cyl, mtcars)
#'
#' @importFrom dplyr ungroup do group_by_ mutate select filter
#' @importFrom tidyr spread
#' @importFrom broom tidy
#' @export
lm_pval <- function(form, object, group=NULL, type=c("drop1","anova"),
                    inter = TRUE, digits = 3) {
  type <- match.arg(type)
  digits <- as.integer(digits)
  stopifnot(!is.na(digits) & digits > 0 & digits < 7)
  
  ## Set up interaction formula from form
  form <- formula(form)
  if(inter) {
    form2 <- as.character(form)
    form2 <- formula(paste0(form2[2], "~(", form2[3], ")^2"))
  } else {
    form2 <- NULL
  }
  
  fit_fn <- ifelse(type == "drop1", drop1, anova)
  if(is.null(group)) {
    pval_one(object, fit_fn, form, form2, digits)
  } else {
    ## Do ANOVA by each group level.
    dplyr::ungroup(
      dplyr::do(
        dplyr::group_by_(object, group),
        pval_one(., fit_fn, form, form2, digits)))
  }
}
pval_one <- function(object, fit_fn, form, form2, digits) {
  fit <- lm(form, data=object)
  sfit <- summary(fit)

  fstat <- sfit$fstatistic
  pval_overall <- signif(pf(fstat[1L], fstat[2L], fstat[3L], 
                            lower.tail = FALSE),
                         digits)
  
  ## Tidy one-line tbl_df of p-values and SD
  out <- dplyr::mutate(
    tidyr::spread(
      dplyr::mutate(
        dplyr::mutate(
          dplyr::filter(
            dplyr::select(
              broom::tidy(
                dplyr::select(
                  fit_fn(fit, fit, test = "F"), 
                  -AIC)
                ),
              term, p.value),
            !is.na(p.value)),
          p.value = signif(p.value, digits)),
        term = make.names(term)),
      term,p.value),
    overall = pval_overall)

  ## P-value for extra interactions
  if(!is.null(form2)) {
    fit2 <- lm(form2, data=object)
    out$interact <- signif(anova(fit2, fit, test="F")[2,"Pr(>F)"],
                           digits)
  }

  out$SD <- signif(sfit$sigma, digits)
  out
}
