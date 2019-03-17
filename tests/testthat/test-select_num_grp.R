context("select_num_grp")

test_that("error messages", {
  expect_error(select_num_grp("A"), "dat must be a data frame or matrix")
  expect_error(select_num_grp(iris, FALSE), "grp_by must be a numeric or character vector")
})

test_that("correct defaults", {
  checkiris <- select_num_grp(iris)
  irisnames <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  iris_manual <- dplyr::select(iris, tidyselect::one_of(irisnames))
  
  expect_equal(nrow(checkiris), nrow(iris))
  expect_equal(ncol(checkiris), 4)
  expect_equal(names(checkiris), irisnames)
  expect_equal(checkiris, iris_manual)
})

test_that("grouping", {
  # grouping by name
  checkiris <- select_num_grp(iris, "Species")
  irisnames <- c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  iris_manual <- dplyr::select(iris, tidyselect::one_of(irisnames))
  
  expect_equal(nrow(checkiris), nrow(iris))
  expect_equal(ncol(checkiris), 5)
  expect_equal(names(checkiris), irisnames)
  expect_equal(checkiris, iris_manual)
  
  # grouping by number
  checkiris <- select_num_grp(iris, 5)
  irisnames <- c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  iris_manual <- dplyr::select(iris, tidyselect::one_of(irisnames))
  
  expect_equal(nrow(checkiris), nrow(iris))
  expect_equal(ncol(checkiris), 5)
  expect_equal(names(checkiris), irisnames)
  expect_equal(checkiris, iris_manual)
})

test_that("columns", {
  # columns by name
  irisnames <- c("Sepal.Length", "Sepal.Width")
  checkiris <- select_num_grp(iris, cols = irisnames)
  iris_manual <- dplyr::select(iris, tidyselect::one_of(irisnames))
  
  expect_equal(nrow(checkiris), nrow(iris))
  expect_equal(ncol(checkiris), 2)
  expect_equal(names(checkiris), irisnames)
  expect_equal(checkiris, iris_manual)
  
  # columns by number
  irisnum <- 1:2
  checkiris <- select_num_grp(iris, cols = irisnum)
  irisnames <- names(iris[irisnum])
  iris_manual <- dplyr::select(iris, tidyselect::one_of(irisnames))
  
  expect_equal(nrow(checkiris), nrow(iris))
  expect_equal(ncol(checkiris), 2)
  expect_equal(names(checkiris), irisnames)
  expect_equal(checkiris, iris_manual)
})

