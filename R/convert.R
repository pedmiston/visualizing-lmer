library(dplyr)
library(reshape2)

#' Converts parameters of a `Reaction ~ (Intercept) + A` model into estimates.
#' Assumes A is a 0-centered, unit-weighted, dichotomous variable.
#'
#' @param tidy_frame A model summary converted to a data.frame, e.g., by using
#'      broom::tidy()
#' @param id_var The name of the grouping factor, if there is one.
convert_parameters_to_estimates <- function(tidy_frame, id_var = ".") {
    tidy_frame %>%
      dcast(as.formula(paste(id_var, "term", sep="~")), value.var = "estimate") %>%
      mutate(`-0.5` = `(Intercept)` - A/2, `0.5` = `(Intercept)` + A/2) %>%
      select(-`(Intercept)`, -A) %>%
      melt(idvars = id_var, measure.vars = c("-0.5", "0.5"),
      variable.name = "A", value.name = "Reaction") %>%
      mutate(A = as.numeric(as.character(A)))
}
