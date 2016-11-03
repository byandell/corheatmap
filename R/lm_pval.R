#' anova p-values grouped by column
#'
#' @param object data.frame or tbl_df
#' @param form formula
#' @param group column to group by (or not if \code{NULL})
#' @param type type of p-values (one of c("drop1","anova"))
#' @param inter test for interactions if \code{TRUE}
#' @param digits number of significant digits (default \code{3})
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
lm_pval <- function(object, form, group=NULL, type=c("drop1","anova"),
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
    object %>%
      ## Do ANOVA by each group level.
      group_by_(group) %>%
      do(pval_one(., fit_fn, form, form2, digits))
  }
}
pval_one <- function(object, fit_fn, form, form2, digits) {
  fit <- lm(form, data=object)
  sfit <- summary(fit)
  sigma <- signif(sfit$sigma, digits)
  fit2 <- lm(form2, data=object)

  fstat <- sfit$fstatistic
  pval_overall <- signif(pf(fstat[1L], fstat[2L], fstat[3L], 
                            lower.tail = FALSE),
                         digits)
  pval_inter <- signif(anova(fit2, fit, test="F")[2,"Pr(>F)"],
                       digits)
  
  ## Tidy one-line tbl_df of p-values and SD
  tidy(fit_fn(fit, fit, test = "F") %>% select(-AIC)) %>%
    select(term, p.value) %>%
    filter(!is.na(p.value)) %>%
    mutate(p.value = signif(p.value, digits)) %>%
    mutate(term = make.names(term)) %>%
    spread(term,p.value) %>%
    mutate(overall = pval_overall,
           interact = pval_inter,
           SD = sigma)
}
