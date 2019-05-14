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

# seed ----
test_that("seed", {
  df1 <- sim_mixed_cc(20, 20, seed = 1)
  df2 <- sim_mixed_cc(20, 20, seed = 1)
  
  expect_equal(df1, df2)
  
  df3 <- sim_mixed_cc(20, 20, seed = 90210)
  
  expect_true(!identical(df1, df3))
})

