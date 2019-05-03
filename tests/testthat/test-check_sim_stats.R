context("check_sim_stats")

# error messages ----
test_that("error messages", {
  expect_error(check_sim_stats("A"), ".data must be a data frame or matrix")
  expect_error(check_sim_stats(iris, FALSE), "between must be a numeric or character vector")
})

# defaults ----
test_that("defaults", {
  checkiris <- check_sim_stats(iris)
  irisnames <- c("n", "var", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "mean", "sd")
  
  expect_equal(nrow(checkiris), 4)
  expect_equal(ncol(checkiris), 8)
  expect_equal(names(checkiris), irisnames)
})

# defaults with between ----
test_that("defaults with between", {
  checkiris <- check_sim_stats(iris, "Species")
  irisnames <- c("Species", "n", "var", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "mean", "sd")
  
  expect_equal(nrow(checkiris), 12)
  expect_equal(ncol(checkiris), 9)
  expect_equal(names(checkiris), irisnames)
})

# is_pos_def ----
test_that("is_pos_def", {
  expect_equal(is_pos_def(matrix(c(1, .5, .5, 1), 2)), TRUE)
  
  bad_matrix <- matrix(c(1, .9, .9, 
                        .9, 1, -.2,
                        .9, -.2, 1), 3)
  expect_equal(is_pos_def(bad_matrix), FALSE)
})


# get_design_long ----
test_that("get_design_long", {
  df_long <- sim_design(2, 2, n = 10, mu = 5, sd = 2, r = 0.5, long = TRUE, empirical = TRUE)
  d <- get_design_long(df_long)
  
  n <- data.frame(A1 = c(10, 10), A2 = c(10, 10), row.names = c("B1", "B2"))
  mu <- data.frame(A1 = c(5, 5), A2 = c(5, 5), row.names = c("B1", "B2"))
  sd <- data.frame(A1 = c(2, 2), A2 = c(2, 2), row.names = c("B1", "B2"))
  r <- data.frame(A1 = c(1, .5), A2 = c(.5, 1), row.names = c("A1", "A2"))
  
  expect_equal(d$within, list(A = c("A1", "A2")))
  expect_equal(d$between, list(B = c("B1", "B2")))
  expect_equal(d$within_factors, "A")
  expect_equal(d$between_factors, "B")
  expect_equal(d$cell_n, n)
  expect_equal(d$cell_mu, mu)
  expect_equal(d$cell_sd, sd)
  expect_equal(d$cell_r$B1, r)
  expect_equal(d$cell_r$B2, r)
  
  df_long <- sim_design(c(2, 2, 2), c(2, 2, 2), long = TRUE, empirical = TRUE)
  d <- get_design_long(df_long)
  
  expect_equal(d$within_factors, c("A", "B", "C"))
  expect_equal(d$between_factors, c("D", "E", "F"))
  expect_equal(d$cell_n  %>% sum(), 6400)
  expect_equal(d$cell_mu %>% sum(), 0)
  expect_equal(d$cell_sd %>% sum(), 64)
  expect_equal(d$cell_r[[1]] %>% sum(), 8)
})
