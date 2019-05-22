context("sim_mixed_cc")

test_that("default", {
  data <- sim_mixed_cc()
  
  expect_equal(nrow(data), 2000)
  expect_equal(ncol(data), 7)
  expect_equal(names(data), c("sub_id", "item_id", "y", "grand_i", "sub_i", "item_i", "err"))
  
  res <- lme4::lmer(y ~ 1 + (1 | sub_id) + (1 | item_id), data = data) %>%
    broom.mixed::tidy(effects = "ran_pars")
  
  expect_equal(res$estimate, c(1,1,1), tolerance = 0.4)
})

test_that("n", {
  data <- sim_mixed_cc(100, 100)
  
  expect_equal(nrow(data), 10000)
  expect_equal(ncol(data), 7)
  expect_equal(names(data), c("sub_id", "item_id", "y", "grand_i", "sub_i", "item_i", "err"))
  
  res <- lme4::lmer(y ~ 1 + (1 | sub_id) + (1 | item_id), data = data) %>%
    broom.mixed::tidy(effects = "ran_pars")
  
  expect_equal(res$estimate, c(1,1,1), tolerance = 0.1)
})

# seed ----
test_that("seed", {
  # setting seed returns same DF, but is reset
  set.seed(1)
  rnd0 <- rnorm(1)
  df1 <- sim_mixed_cc(seed = 910210)
  rnd1 <- rnorm(1)
  df2 <- sim_mixed_cc(seed = 910210)
  rnd2 <- rnorm(1)
  set.seed(1)
  rnd0b <- rnorm(1)
  rnd1b <- rnorm(1)
  rnd2b <- rnorm(1)
  df3 <- sim_mixed_cc(seed = 8675309)
  
  expect_equal(df1, df2)
  expect_false(rnd1 == rnd2)
  expect_equal(rnd0, rnd0b)
  expect_equal(rnd1, rnd1b)
  expect_equal(rnd2, rnd2b)
  expect_true(!identical(df1, df3))
  
  # user sets seed externally
  set.seed(1)
  df4 <- sim_mixed_cc()
  set.seed(1)
  df5 <- sim_mixed_cc()
  expect_equal(df4, df5)
})

