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
