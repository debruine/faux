context("test-convert_param")

cells_w <- c("W1_X1", "W2_X1", "W1_X2", "W2_X2")
cells_b <- c("B1_C1", "B2_C1", "B1_C2", "B2_C2")

# single number ---
testthat::test_that("single number", {
  param <- 3
  cp <- faux:::convert_param(param, cells_w, cells_b)
  comp <- matrix(rep(3, 16), 4, dimnames = list(cells_b, cells_w)) %>% as.data.frame()
  
  expect_equal(cp, comp)
})
  
# list of single numbers ----
testthat::test_that("list of single numbers", {
  param <- list(
    "B1_C1" = 1, 
    "B2_C1" = 2, 
    "B1_C2" = 3, 
    "B2_C2" = 4
  )
  cp <- faux:::convert_param(param, cells_w, cells_b)
  comp <- matrix(rep(1:4, times = 4), 4, dimnames = list(cells_b, cells_w)) %>% 
    as.data.frame()
  expect_equal(cp, cp)
})


# unnamed vector of cells ----
testthat::test_that("unnamed vector of cells", {
  param <- rep(1:4, each = 4)
  cp <- faux:::convert_param(param, cells_w, cells_b)
  comp <- matrix(rep(1:4, times = 4), 4, dimnames = list(cells_b, cells_w)) %>% 
    as.data.frame()
  
  expect_equal(cp, comp)
})


# list of unnamed vectors ----
testthat::test_that("list of unnamed vectors", {
  param <- list(
    "B1_C1" = 1:4,
    "B2_C1" = 5:8,
    "B1_C2" = 9:12,
    "B2_C2" = 13:16
  )
  cp <- faux:::convert_param(param, cells_w, cells_b)
  comp <- matrix(1:16, 4, dimnames = list(cells_w, cells_b)) %>% t() %>% as.data.frame()
  expect_equal(cp, comp)
})


# list of named vectors ----
testthat::test_that("list of named vectors", {
  param <- list(
    "B1_C1" = c("W1_X1" = 1, "W2_X1" = 2, "W1_X2" = 3, "W2_X2" = 4),
    "B2_C1" = c("W1_X1" = 5, "W2_X1" = 6, "W1_X2" = 7, "W2_X2" = 8),
    "B1_C2" = c("W1_X1" = 9, "W2_X1" = 10, "W1_X2" = 11, "W2_X2" = 12),
    "B2_C2" = c("W1_X1" = 13, "W2_X1" = 14, "W1_X2" = 15, "W2_X2" = 16)
  )
  cp <- faux:::convert_param(param, cells_w, cells_b)
  comp <- matrix(1:16, 4, dimnames = list(cells_w, cells_b)) %>% t() %>% as.data.frame()
  expect_equal(cp, comp)
})

# list of disordered named vectors ----
testthat::test_that("list of disordered named vectors", {
  param <- list(
    "B1_C2" = c("W1_X2" = 11, "W2_X2" = 12, "W1_X1" = 9, "W2_X1" = 10),
    "B1_C1" = c("W1_X1" = 1, "W2_X1" = 2, "W1_X2" = 3, "W2_X2" = 4),
    "B2_C1" = c("W1_X1" = 5, "W1_X2" = 7, "W2_X1" = 6, "W2_X2" = 8),
    "B2_C2" = c("W1_X1" = 13, "W1_X2" = 15, "W2_X1" = 14, "W2_X2" = 16)
  )
  cp <- faux:::convert_param(param, cells_w, cells_b)
  comp <- matrix(1:16, 4, dimnames = list(cells_w, cells_b)) %>% t() %>% as.data.frame()
  expect_equal(cp, comp)
})

# data frame ----
testthat::test_that("data frame", {
  param <- data.frame(
    c(1, 5, 9, 13),
    c(2, 6, 10, 14),
    c(3, 7, 11, 15),
    c(4, 8, 12, 16),
    row.names = cells_b
  )
  names(param) <- cells_w
  
  cp <- faux:::convert_param(param, cells_w, cells_b)
  comp <- matrix(1:16, 4, dimnames = list(cells_w, cells_b)) %>% t() %>% as.data.frame()
  expect_equal(cp, comp)
})

# backwards data frame ----
testthat::test_that("backwards data frame", {
  param <- data.frame(
    1:4,
    5:8,
    9:12,
    13:16,
    row.names = cells_w
  )
  names(param) <- cells_b
  
  cp <- faux:::convert_param(param, cells_w, cells_b)
  comp <- matrix(1:16, 4, dimnames = list(cells_w, cells_b)) %>% t() %>% as.data.frame()
  expect_equal(cp, comp)
})



