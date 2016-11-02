#' anova p-values grouped by column
#'
#' @param object data.frame or tbl_df
#' @param form formula
#' @param group column to group by (or not if \code{NULL})
#' @param type type of p-values (one of c("drop1","anova"))
#'
#' @author Brian S Yandell, \email{brian.yandell@@wisc.edu}
#' @keywords utilities
#'
#' @export
lm_pval <- function(object, form, group=NULL, type=c("drop1","anova")) {
  type <- match.arg(type)
  switch(type,
         drop1 = {
           if(is.null(group)) {
             drop1_pval_one(object, form)
           } else {
             object %>%
               ## Do ANOVA by each group level.
               group_by_(group) %>%
               do(drop1_pval_one(., form))
           }
         },
         anova = {
           if(is.null(group)) {
             anova_pval_one(object, form)
           } else {
             object %>%
               ## Do ANOVA by each group level.
               group_by_(group) %>%
               do(anova_pval_one(., form))
           }
         })
}
anova_pval_one <- function(object, form) {
  fit <- lm(form, data=object)
  sfit <- summary(fit)
  sigma <- sfit$sigma
  tidy(anova(fit)) %>%
    select(term, p.value) %>%
    filter(!is.na(p.value)) %>%
    mutate(term = make.names(term)) %>%
    spread(term,p.value) %>%
    mutate(overall = pval_overall(sfit),
           SD = sigma)
}
drop1_pval_one <- function(object, form) {
  fit <- lm(form, data=object)
  sfit <- summary(fit)
  sigma <- sfit$sigma
  tidy(drop1(fit, fit, test = "F") %>% select(-AIC)) %>%
    select(term, p.value) %>%
    filter(!is.na(p.value)) %>%
    mutate(term = make.names(term)) %>%
    spread(term,p.value) %>%
    mutate(overall = pval_overall(sfit),
           SD = sigma)
}
pval_overall <- function(sfit) {
  fstat <- sfit$fstatistic
  pf(fstat[1L], fstat[2L], fstat[3L], lower.tail = FALSE)
}
