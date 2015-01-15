library(testthat)
context("Convert Tests")

library(dplyr)
library(broom)
library(lme4)

source("convert.R")

example <- sleepstudy %>%
  mutate(A = ifelse(Days < 5, -0.5, 0.5)) %>%
  select(Subject, A, Reaction)

intercepts_mod <- lmer(Reaction ~ A + (1|Subject), data = example)
slopes_mod <- lmer(Reaction ~ A + (A|Subject), data = example)

test_that("convert fixed effects of intercepts mod to point estimates", {
  fixed_params <- tidy(intercepts_mod, effects = "fixed")[,c("term", "estimate")]
  fixed_estimates <- convert_parameters_to_estimates(fixed_params)[,c("A", "Reaction")]

  expect_equal(nrow(fixed_estimates), 2)  # should be one estimate for every level of A

  group_means <- example %>% 
    group_by(A) %>% 
    summarize(Reaction = mean(Reaction)) %>% 
    select(Reaction)
  expect_equal(fixed_estimates[["Reaction"]], group_means[["Reaction"]])
})

test_that("convert fixed effects of slopes mod to point estimates", {
  fixed_params <- tidy(slopes_mod, effects = "fixed")[,c("term", "estimate")]
  fixed_estimates <- convert_parameters_to_estimates(fixed_params)[,c("A", "Reaction")]
  
  expect_equal(nrow(fixed_estimates), 2)

  group_means <- example %>% 
    group_by(A) %>% 
    summarize(Reaction = mean(Reaction)) %>% 
    select(Reaction)
  expect_equal(fixed_estimates[["Reaction"]], group_means[["Reaction"]])
})

test_that("convert random effects of intercept mod to point estimates", {
  random_params <- tidy(intercepts_mod, effect = "random")
  random_estimates <- convert_parameters_to_estimates(random_params, id_var = "level")

  expect_equal(nrow(random_estimates), length(unique(example$Subject)) * 2)
})

test_that("convert random effects of slopes mod to point estimates", {
  random_params <- tidy(slopes_mod, effect = "random")
  random_estimates <- convert_parameters_to_estimates(random_params, id_var = "level")
  
  expect_equal(nrow(random_estimates), length(unique(example$Subject)) * 2)
})
