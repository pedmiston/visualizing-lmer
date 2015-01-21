library(testthat)

source("simulate.R")

context("Sleep Study")

test_that("generate subject effects with correlated intercept and slope", {
  num_subjects <- 18
  grand_means <- data.frame(
    term = c("intercept", "slope"),
    mean = c(300, 10), 
    sd = c(38, 8)
  )
  correlations <- c("intercept_slope" = 0.9)
  cor_matrix <- to_cor_matrix(correlations, grand_means$term)
  
  subj_effects <- generate_subject_effects(num_subjects, grand_means, cor_matrix)
  expect_equal(nrow(subj_effects), num_subjects)
  expect_equal(mean(subj_effects$intercept), grand_means[1, "mean"])
  expect_equal(mean(subj_effects$slope), grand_means[2, "mean"])
  expect_equal(names(subj_effects), c("subject", "intercept", "slope"))
})

test_that("generate subject effects with correlated intercept, slope, and error", {
  num_subjects <- 18
  grand_means <- data.frame(
    term = c("intercept", "slope", "error"),
    mean = c(400, 20, 200), 
    sd = c(200, 15, 20)
  )
  correlations <- c("intercept_slope" = 0.0, "intercept_error" = 0.0, "slope_error" = 0.0)
  cor_matrix <- to_cor_matrix(correlations, grand_means$term) # no true correlation
  
  subj_effects <- generate_subject_effects(num_subjects, grand_means, cor_matrix)
  expect_equal(nrow(subj_effects), num_subjects)
  expect_equal(mean(subj_effects$intercept), grand_means[1, "mean"])
  expect_equal(mean(subj_effects$slope), grand_means[2, "mean"])
  expect_equal(mean(subj_effects$error), grand_means[3, "mean"])
  expect_equal(names(subj_effects), c("subject", "intercept", "slope", "error"))
})

test_that("random effects are reproducible", {
  num_subjects <- 18
  grand_means <- data.frame(
    term = c("intercept", "slope"),
    mean = c(300, 10), 
    sd = c(38, 8)
  )
  correlations <- c("intercept_slope" = 0.9)
  cor_matrix <- to_cor_matrix(correlations, grand_means$term)

  same_seed <- 100
  diff_seed <- 101
  subj_effects_1 <- generate_subject_effects(num_subjects, grand_means, cor_matrix, same_seed)
  subj_effects_2 <- generate_subject_effects(num_subjects, grand_means, cor_matrix, same_seed)
  subj_effects_3 <- generate_subject_effects(num_subjects, grand_means, cor_matrix, diff_seed)
  
  expect_true(all(subj_effects_1$intercept == subj_effects_2$intercept))
  expect_false(any(subj_effects_1$intercept == subj_effects_3$intercept))  # fuzzy
})

