context("test-sim_design")

test_that("error messages", {
  factors_err <- "You must specify at least one factor"
  expect_error(sim_design(), factors_err)
  expect_error(sim_design(within = list()), factors_err)
  expect_error(sim_design(between = list()), factors_err)
  
  list_err <- "within and between must be lists"
  expect_error(sim_design("1"), list_err)
  expect_error(sim_design(list(), "1"), list_err)
})

test_that("works", {
  within <- list("condition" = c("cheerful", "sad"))
  between <- list("voice" = c("human", "robot"))
  
  within <- list(
    "Aw" = c("A1", "A2"),
    "Bw" = c("B1", "B2")
  )
  between <- list(
    "Ab" = c("A1", "A2"),
    "Bb" = c("B1", "B2", "B3")
  )
  
  df <- sim_design(within, between, n = 4, cors = .2, mu = c(1, 2, 3, 4), empirical = TRUE)
  check_sim_stats(df)
  
  expect_equal(nrow(df), 24)
  expect_equal(ncol(df), 7)
  expect_equal(names(df), c("sub_id", "Ab", "Bb", "A1_B1", "A1_B2", "A2_B1", "A2_B2"))
})
