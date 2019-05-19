context("test-convert_param")

cells_w <- c("W1_X1", "W2_X1", "W1_X2", "W2_X2")
cells_b <- c("B1_C1", "B2_C1", "B1_C2", "B2_C2")

# single number ---
testthat::test_that("single number", {
  param <- 3
  cp <- faux:::convert_param(param, cells_w, cells_b)
  expect_equal(names(cp), cells_b)
  expect_equal(names(cp[[1]]), cells_w)
  expect_equal(unlist(cp) %>% unname(), rep(3, 16))
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
  
  expect_equal(names(cp), cells_b)
  expect_equal(names(cp[[1]]), cells_w)
  expect_equal(unlist(cp) %>% unname(), rep(1:4, each = 4))
})


# unnamed vector of cells ----
testthat::test_that("unnamed vector of cells", {
  param <- rep(1:4, each = 4)
  cp <- faux:::convert_param(param, cells_w, cells_b)
  
  expect_equal(names(cp), cells_b)
  expect_equal(names(cp[[1]]), cells_w)
  expect_equal(unlist(cp) %>% unname(), param)
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
  
  expect_equal(names(cp), cells_b)
  expect_equal(names(cp[[1]]), cells_w)
  expect_equal(unlist(cp) %>% unname(), 1:16)
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
  
  expect_equal(names(cp), cells_b)
  expect_equal(names(cp[[1]]), cells_w)
  expect_equal(unlist(cp) %>% unname(), 1:16)
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
  
  expect_equal(names(cp), cells_b)
  expect_equal(names(cp[[1]]), cells_w)
  expect_equal(unlist(cp) %>% unname(), 1:16)
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
  
  expect_equal(names(cp), cells_b)
  expect_equal(names(cp[[1]]), cells_w)
  expect_equal(unlist(cp) %>% unname(), 1:16)
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
  
  expect_equal(names(cp), cells_b)
  expect_equal(names(cp[[1]]), cells_w)
  expect_equal(unlist(cp) %>% unname(), 1:16)
})

# 1-col or 1-row data frames
test_that("1-col/1-row df", {
  # setup 
  cells_b <- c("A1_B1", "A1_B2", "A2_B1", "A2_B2")
  cells_w <- c("C1", "C2")
  rows_b <- data.frame(1:4, row.names = cells_b)
  cols_b <- t(rows_b) %>% as.data.frame()
  
  cb <- convert_param(cols_b, cells_w, cells_b)
  rb <- convert_param(rows_b, cells_w, cells_b)
  expect_equal(cb, rb)
  
  cols_w <- data.frame(C1 = 1, C2 = 2)
  rows_w <- t(cols_w) %>% as.data.frame()
  cw <- convert_param(cols_w, cells_w, cells_b)
  rw <- convert_param(rows_w, cells_w, cells_b)
  expect_equal(cw, rw)
})



