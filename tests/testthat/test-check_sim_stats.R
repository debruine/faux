context("check_sim_stats")

test_that("error messages", {
  expect_error(check_sim_stats("A"), "dat must be a data frame or matrix")
  expect_error(check_sim_stats(iris, FALSE), "grp_by must be a numeric or character vector")
})

test_that("correct defaults", {
  checkiris <- check_sim_stats(iris)
  irisnames <- c("var", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "mean", "sd")
  
  expect_equal(nrow(checkiris), 4)
  expect_equal(ncol(checkiris), 7)
  expect_equal(names(checkiris), irisnames)
})

test_that("correct defaults with group", {
  checkiris <- check_sim_stats(iris, "Species")
  irisnames <- c("Species", "var", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "mean", "sd")
  
  expect_equal(nrow(checkiris), 12)
  expect_equal(ncol(checkiris), 8)
  expect_equal(names(checkiris), irisnames)
})
