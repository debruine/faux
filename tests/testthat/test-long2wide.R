context("long2wide")

# 2w ----
test_that("2w", {
  df_long <- sim_design(within = 2, long = TRUE)
  df_wide <- long2wide(df_long, "A", c(), "y", "id")
  
  expect_equal(nrow(df_wide), 100)
  expect_equal(ncol(df_wide), 3)
  expect_equal(colnames(df_wide), c("id", "A1", "A2"))
})

# 2b ----
test_that("2b", {
  df_long <- sim_design(between = 2, long = TRUE)
  df_wide <- long2wide(df_long, c(), "A", "y", "id")
  
  expect_equal(nrow(df_wide), 200)
  expect_equal(ncol(df_wide), 3)
  expect_equal(colnames(df_wide), c("id", "A", "y"))
  expect_equivalent(df_long, df_wide)
})

# 2w*2b ----
test_that("2w*2b", {
  df_long <- sim_design(2, 2, long = TRUE)
  df_wide <- long2wide(df_long, "A", "B", "y", "id")
  
  expect_equal(names(df_wide), c("id", "B", "A1", "A2"))
  expect_equal(nrow(df_wide), 200)
})

# named arguments ----
test_that("named arguments", {
  df_long <- sim_design(2, 2, long = TRUE)
  df_wide <- long2wide(dv = "y", id = "id", .data = df_long, between = "B", within = "A")
  
  expect_equal(names(df_wide), c("id", "B", "A1", "A2"))
  expect_equal(nrow(df_wide), 200)
})

# 2w*2w*2b*2b ----
test_that("2w*2w*2b*2b", {
  df_long <- sim_design(c(2, 2), c(2, 2), long = TRUE)
  df_wide <- long2wide(df_long, c("A", "B"), c("C","D"), "y", "id")
  
  expect_equal(names(df_wide), c("id", "C", "D", "A1_B1", "A1_B2", "A2_B1", "A2_B2"))
  expect_equal(nrow(df_wide), 400)
})


# iris ----
test_that("iris", {
  iris_long <- iris %>%
    dplyr::mutate(id = make_id(nrow(.), "I")) %>%
    tidyr::gather(var, y, Sepal.Length:Petal.Width) %>%
    tidyr::separate(var, c("Feature", "Measure"))
  
  iris_wide <- long2wide(iris_long, within = c("Feature", "Measure"), 
                         between = "Species", dv = "y", id = "id")
  inames <- c("id", "Species", "Petal_Length", "Petal_Width", "Sepal_Length", "Sepal_Width")
  expect_equal(names(iris_wide), inames)
  expect_equal(nrow(iris_wide), 150)
  
  long <- check_sim_stats(iris_long, within = c("Feature", "Measure"), 
                          between = "Species", dv = "y", id = "id")
  
  wide <- check_sim_stats(iris, between = "Species")
  
  
  expect_equal(nrow(long), nrow(wide))
})

# wide2long ----
test_that("wide2long", {
  long_iris <- wide2long(
    iris,
    within_factors = c("feature", "dimension"),
    within_cols = c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width"),
    dv = "value",
    id = "id"
  )
  
  expect_equal(nrow(long_iris), 600)
  expect_equal(names(long_iris), c("Species", "id", "feature", "dimension", "value"))
})
