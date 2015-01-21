suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(dplyr))

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
generate_subject_effects <- function(num_subjects, grand_means, cor_matrix, 
                                     seed = as.numeric(format(Sys.time(), "%M%S"))) {
  set.seed(seed)
  cov_matrix <- cor_to_cov(cor_matrix, grand_means$sd)
  subj_effects <- mvrnorm(num_subjects, mu = grand_means$mean, cov_matrix, empirical = TRUE) %>%
    as.data.frame(.) %>%
    mutate(subject = paste0("S", seq(from = 101, to = 101 + num_subjects - 1)))
  if ("error" %in% names(subj_effects)) {
    subj_effects <- subj_effects[,c("subject", "intercept", "slope", "error")]
  } else {
    subj_effects <- subj_effects[,c("subject", "intercept", "slope")]
  }
  subj_effects
}

#' Given by-subject effects, generates random data for each subject.
#'
#' @param parameters A list with slots for Subject, Intercept, Slope, and Error
#' @return A data.frame in the form of the sleepstudy package in lme4
generate_subject_data <- function(subj_effects, residual_sd = NULL,
                                  seed = as.numeric(format(Sys.time(), "%M%S"))) {
  set.seed(seed)
  if (!is.null(residual_sd)) {
    subj_effects$residual_sd <- residual_sd
  }
  subj_effects %>% rowwise() %>%
    do({
      base_reaction = rnorm(n = length(0:9), mean = .[["intercept"]], sd = .[["residual_sd"]])
      base_slope = rep(.[["slope"]], times = length(0:9))
      data.frame(subject = .[["subject"]], days = 0:9) %>%
        mutate(
          days_c = days - median(days),
          reaction = base_reaction,
          reaction = reaction + days_c * base_slope
        )
    }) %>% ungroup()
}

shuffle_until <- function(frame, col, break_func, max_iter = 1000, 
                          seed = as.numeric(format(Sys.time(), "%M%S"))) {
  set.seed(seed)
  i <- 1
  while (break_func(frame) == FALSE && i < max_iter) {
    frame[col] <- sample(frame[[col]])
    i <- i + 1
  }
  frame
}
