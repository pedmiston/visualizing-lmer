library(MASS)
library(dplyr)

#' @param correlations A list of correlations
#' @param names Names for the resulting matrix
#' @return A correlation matrix
to_cor_matrix <- function(correlations, names) {
  if (length(correlations) == 1) {
    cor_matrix <- diag(2)
  } else {
    cor_matrix <- diag(length(correlations))
  }
  cor_matrix[row(cor_matrix) > col(cor_matrix)] <- unlist(correlations)
  cor_matrix <- cor_matrix + t(cor_matrix)
  diag(cor_matrix) <- 1
  dimnames(cor_matrix) <- list(names, names)
  cor_matrix
}

#' @param cor_matrix A correlation matrix
#' @param stdevs A vector of standard deviations
#' @return A variance-covariance matrix
cor_to_cov <- function(cor_matrix, stdevs) {
  cor_matrix * (stdevs %*% t(stdevs))
}

#' Generates by-subject effects for simulating within-subject data.
#'
#' @param num_subjects The number of subjects in the sample
#' @param grand_means A data.frame with columns for Mean and SD and rows for
#'          of the variables.
#' @param cor_matrix The desired symmetric correlation matrix for all variables.
#' @return A data.frame with columns for subject and each parameter
generate_subject_effects <- function(num_subjects, grand_means, cor_matrix) {
  cov_matrix <- cor_to_cov(cor_matrix, grand_means$SD)
  mvrnorm(num_subjects, mu = grand_means$Mean, cov_matrix, empirical = TRUE) %>%
    as.data.frame(.) %>%
    mutate(Subject = paste0("S", seq(from = 101, to = 101 + num_subjects - 1)))
}

#' Given by-subject effects, generates random data for each subject.
#'
#' @param parameters A list with slots for Subject, Intercept, Slope, and Error
#' @return A data.frame in the form of the sleepstudy package in lme4
generate_subject_data <- function(subj_effects) {
  subj_effects %>% rowwise() %>%
    do({
      data.frame(Subject = .[["Subject"]], Days = 0:9) %>%
        mutate(
          DaysCentered = Days - mean(Days),
          Reaction = rnorm(n(), mean = .[["Intercept"]], sd = .[["Error"]]),
          Reaction = Reaction + .[["Slope"]] * DaysCentered
        )
    }) %>% ungroup()
}