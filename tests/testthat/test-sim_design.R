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
  
  between <- list(
    "B" = c("B1", "B2"),
    "A" = c("A2", "A1")
  )
  within <- list(
    "W" = c("W1", "W2"),
    "C" = c("C2", "C1")
  )
  
  mu = list(
    "B1_A2" = c(0, 10, 20, 30),
    "B1_A1" = c(40, 50, 60, 70),
    "B2_A1" = c(100, 110, 120, 130),
    "B2_A2" = c(140, 150, 160, 170)
  )
  sd = list(
    "B1_A2" = c(1, 1, 1, 1),
    "B1_A1" = 2,
    "B2_A1" = c(5, 10, 15, 20),
    "B2_A2" = c(30, 40, 50, 60)
  )
  
  triangle <- c(.1, .2, .3, .4, .5, .6)
  long_cor <- c(1, .1, .2, .3,
               .1,  1, .4, .5,
               .2, .4,  1, .6,
               .3, .5, .6,  1)
  mat <- matrix(long_cor, nrow = 4)
  
  cors = list(
    "B1_A2" = triangle,
    "B1_A1" = long_cor,
    "B2_A1" = mat,
    "B2_A2" = .4
  )
  
  n = 100
  empirical = TRUE

  df <- sim_design(within, between, n, cors, mu, sd, empirical)
  check_sim_stats(df, c("B", "A"))
  
  expect_equal(nrow(df), 400)
  expect_equal(ncol(df), 7)
  expect_equal(names(df), c("sub_id", "B", "A", "W1_C2", "W2_C2", "W1_C1", "W2_C1"))
})
