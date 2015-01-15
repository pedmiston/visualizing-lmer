library(testthat)

source("sleepstudy.R")

context("Sleep Study")

test_that("Subject effects for the sleepstudy experiment can be simulated", {
  num_subjects <- 18
  grand_means <- data.frame(
    row.names = c("Intercept", "Slope", "Error"),
    Mean = c(400, 20, 200), 
    SD = c(200, 15, 20)
  )
  correlations <- c("intercept_slope" = 0.0, "intercept_error" = 0.0, "slope_error" = 0.0)
  cor_matrix <- to_cor_matrix(correlations, row.names(grand_means)) # no true correlation
  
  subj_effects <- generate_subject_effects(num_subjects, grand_means, cor_matrix)
  expect_equal(nrow(subj_effects), num_subjects)
  expect_equal(mean(subj_effects$Intercept), grand_means["Intercept", "Mean"])
  expect_equal(mean(subj_effects$Slope), grand_means["Slope", "Mean"])
  expect_equal(mean(subj_effects$Error), grand_means["Error", "Mean"])
})
