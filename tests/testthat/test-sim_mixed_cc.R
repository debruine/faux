test_that("default", {
  data <- sim_mixed_cc()
  
  expect_equal(nrow(data), 2000)
  expect_equal(ncol(data), 3)
  expect_equal(names(data), c("sub_id", "item_id", "val"))
  
  res <- lme4::lmer(val ~ 1 + (1 | sub_id) + (1 | item_id), data = data) %>%
    broom.mixed::tidy(effects = "ran_pars")
})

test_that("default", {
  data <- sim_mixed_cc(100, 100)
  
  expect_equal(nrow(data), 10000)
  expect_equal(ncol(data), 3)
  expect_equal(names(data), c("sub_id", "item_id", "val"))
  
  res <- lme4::lmer(val ~ 1 + (1 | sub_id) + (1 | item_id), data = data) %>%
    broom.mixed::tidy(effects = "ran_pars")
})

