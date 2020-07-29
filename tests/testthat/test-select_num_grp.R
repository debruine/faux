context("select_num_grp")

# errors ----
test_that("error messages", {
  expect_error(select_num_grp("A"), "data must be a data frame or matrix")
  expect_error(select_num_grp(iris, FALSE), "between must be a numeric or character vector")
})

# defaults ----
test_that("defaults", {
  checkiris <- select_num_grp(iris)
  expect_equal(checkiris, iris[,1:4])
})

# matrix conversion ----
test_that("matrix", {
  checkiris <- as.matrix(iris[,1:4]) %>% 
    select_num_grp()
  
  expect_equal(checkiris, iris[,1:4])
})

# grouping ----
test_that("grouping", {
  # grouping by name
  checkiris <- select_num_grp(iris, "Species")
  irisnames <- c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  iris_manual <- dplyr::group_by(iris[irisnames], Species)
  
  expect_equal(nrow(checkiris), nrow(iris))
  expect_equal(ncol(checkiris), 5)
  expect_equal(names(checkiris), irisnames)
  expect_equal(checkiris, iris_manual)
  
  # grouping by number
  checkiris <- select_num_grp(iris, 5)
  irisnames <- c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  iris_manual <- dplyr::group_by(iris[irisnames], Species)
  
  expect_equal(nrow(checkiris), nrow(iris))
  expect_equal(ncol(checkiris), 5)
  expect_equal(names(checkiris), irisnames)
  expect_equal(checkiris, iris_manual)
})

# columns ----
test_that("columns", {
  # columns by name
  irisnames <- c("Sepal.Length", "Sepal.Width")
  checkiris <- select_num_grp(iris, cols = irisnames)
  iris_manual <- dplyr::select(iris, all_of(irisnames))
  
  expect_equal(nrow(checkiris), nrow(iris))
  expect_equal(ncol(checkiris), 2)
  expect_equal(names(checkiris), irisnames)
  expect_equal(checkiris, iris_manual)
  
  # columns by number
  irisnum <- 1:2
  checkiris <- select_num_grp(iris, cols = irisnum)
  irisnames <- names(iris[irisnum])
  iris_manual <- dplyr::select(iris, all_of(irisnames))
  
  expect_equal(nrow(checkiris), nrow(iris))
  expect_equal(ncol(checkiris), 2)
  expect_equal(names(checkiris), irisnames)
  expect_equal(checkiris, iris_manual)
})

