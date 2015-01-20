library(testthat)

source("simulate.R")

context("Sleep Study")

test_that("Subject effects for the sleepstudy experiment can be simulated", {
  num_subjects <- 18
  grand_means <- data.frame(
    row.names = c("intercept", "slope", "error"),
    mean = c(400, 20, 200), 
    sd = c(200, 15, 20)
  )
  correlations <- c("intercept_slope" = 0.0, "intercept_error" = 0.0, "slope_error" = 0.0)
  cor_matrix <- to_cor_matrix(correlations, row.names(grand_means)) # no true correlation
  
  subj_effects <- generate_subject_effects(num_subjects, grand_means, cor_matrix)
  expect_equal(nrow(subj_effects), num_subjects)
  expect_equal(mean(subj_effects$intercept), grand_means["intercept", "mean"])
  expect_equal(mean(subj_effects$slope), grand_means["slope", "mean"])
  expect_equal(mean(subj_effects$error), grand_means["error", "mean"])
  expect_equal(names(subj_effects), c("subject", "intercept", "slope", "error"))
})

test_that("Simple correlation between slope and intercept can be simulated", {
  num_subjects <- 18
  grand_means <- data.frame(
    row.names = c("intercept", "slope"),
    mean = c(300, 10), 
    sd = c(38, 8)
  )
  correlations <- c("intercept_slope" = 0.9)
  cor_matrix <- to_cor_matrix(correlations, row.names(grand_means))
  
  subj_effects <- generate_subject_effects(num_subjects, grand_means, cor_matrix)
  expect_equal(nrow(subj_effects), num_subjects)
  expect_equal(mean(subj_effects$intercept), grand_means["intercept", "mean"])
  expect_equal(mean(subj_effects$slope), grand_means["slope", "mean"])
  expect_equal(names(subj_effects), c("subject", "intercept", "slope"))
})
