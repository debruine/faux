context("check_sim_stats")

test_that("error messages", {
  expect_error(check_sim_stats("A"), "dat must be a data frame or matrix")
  expect_error(check_sim_stats(iris, FALSE), "between must be a numeric or character vector")
})

test_that("correct defaults", {
  checkiris <- check_sim_stats(iris)
  irisnames <- c("n", "var", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "mean", "sd")
  
  expect_equal(nrow(checkiris), 4)
  expect_equal(ncol(checkiris), 8)
  expect_equal(names(checkiris), irisnames)
})

test_that("correct defaults with group", {
  checkiris <- check_sim_stats(iris, "Species")
  irisnames <- c("Species", "n", "var", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "mean", "sd")
  
  expect_equal(nrow(checkiris), 12)
  expect_equal(ncol(checkiris), 9)
  expect_equal(names(checkiris), irisnames)
})

test_that("is_pos_def", {
  expect_equal(is_pos_def(matrix(c(1, .5, .5, 1), 2)), TRUE)
  
  bad_matrix <- matrix(c(1, .9, .9, 
                        .9, 1, -.2,
                        .9, -.2, 1), 3)
  expect_equal(is_pos_def(bad_matrix), FALSE)
})


test_that("long", {
  df_long <- sim_design(2,2, frame_long = TRUE)
  df_wide <- long2wide(df_long, "B", "A", "val", "sub_id")
  
  expect_equal(names(df_wide), c("sub_id", "B", "A1", "A2"))
  expect_equal(nrow(df_wide), 200)
  
  iris_long <- iris %>%
    dplyr::mutate(id = make_id(nrow(.), "I")) %>%
    tidyr::gather(var, val, Sepal.Length:Petal.Width) %>%
    tidyr::separate(var, c("Feature", "Measure"))

<<<<<<< HEAD
  iris_wide <- long2wide(iris_long, within = c("Feature", "Measure"), 
                         between = "Species", dv = "val", id = "id")
  inames <- c("Species", "id", "Petal_Length", "Petal_Width", "Sepal_Length", "Sepal_Width")
  testthat::expect_equal(names(iris_wide), inames)
  testthat::expect_equal(nrow(iris_wide), 150)
  
  long <- check_sim_stats(iris_long, within = c("Feature", "Measure"), 
                          between = "Species", dv = "val", id = "id")
  
  wide <- check_sim_stats(iris, between = "Species")
  
  
  testthat::expect_equal(nrow(long), nrow(wide))
=======
  iris_wide <- long2wide(iris_long, between = "Species", 
                         within = c("Feature", "Measure"), 
                         dv = "val", id = "id")
  iris_wide_names <- c("id", "Species", "Petal_Length", "Petal_Width", "Sepal_Length", "Sepal_Width")
  
  expect_equal(names(iris_wide), iris_wide_names)
  expect_equal(nrow(iris_wide), 150)
  
  chk <- check_sim_stats(iris_long, between = "Species", 
                         within = c("Feature", "Measure"), 
                         dv = "val", id = "id")
  
  expect_equal(nrow(chk), 12)
  expect_equal(names(chk), c("Species", "n", "var", "Petal_Length", "Petal_Width", 
                             "Sepal_Length", "Sepal_Width", "mean", "sd"))
>>>>>>> 2e6b53215a76edb725bd2a55068d6181d5594185
})
