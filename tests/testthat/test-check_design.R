context("test-check_design")

# 2w ----
test_that("2w", {
  within <- list(time = c("night", "day"))
  between <- list()
  design <- check_design(within, between, n = 10, plot = FALSE)
  
  cell_n <- data.frame(night = 10, day = 10, row.names = "val")
  cell_mu <- data.frame(night = 0, day = 0, row.names = "val")
  cell_sd <- data.frame(night = 1, day = 1, row.names = "val")
  
  expect_equal(design$within, list(time = c(night = "night", day = "day")))
  expect_equal(design$between, list())
  
  expect_equal(design$cell_n, cell_n)
  expect_equal(design$cell_mu, cell_mu)
  expect_equal(design$cell_sd, cell_sd)
  
  expect_equal(design$cells_w, c("night", "day"))
  expect_equal(design$cells_b, "val")
})

# 2b ----
test_that("2b", {
  within <- list()
  between <- list(time = c("night", "day"))
  design <- check_design(within, between, n = 10, plot = FALSE)
  
  cell_n <- data.frame(val = c(10, 10), row.names = c("night", "day"))
  cell_mu <- data.frame(val = c(0, 0), row.names = c("night", "day"))
  cell_sd <- data.frame(val = c(1, 1), row.names = c("night", "day"))
  
  expect_equal(design$within, list())
  expect_equal(design$between, list(time = c(night = "night", day = "day")))
  
  expect_equal(design$cell_n, cell_n)
  expect_equal(design$cell_mu, cell_mu)
  expect_equal(design$cell_sd, cell_sd)
  
  expect_equal(design$cells_w, "val")
  expect_equal(design$cells_b, c("night", "day"))
})

# 2w*2b ----
test_that("2w*2b", {
  within <- list(time = c("night", "day"))
  between <- list(pet = c("dog", "cat"))
  design <- check_design(within, between, n = 10, plot = FALSE)

  cell_n <- data.frame(night = c(10,10), day = c(10,10), 
                       row.names = c("dog", "cat"))
  cell_mu <- data.frame(night = c(0,0), day = c(0,0), 
                        row.names = c("dog", "cat"))
  cell_sd <- data.frame(night = c(1,1), day = c(1,1), 
                        row.names = c("dog", "cat"))

  expect_equal(design$within, list(time = c(night = "night", day = "day")))
  expect_equal(design$between, list(pet = c(dog = "dog", cat = "cat")))
  
  expect_equal(design$cell_n, cell_n)
  expect_equal(design$cell_mu, cell_mu)
  expect_equal(design$cell_sd, cell_sd)
  
  expect_equal(design$cells_w, c("night", "day"))
  expect_equal(design$cells_b, c("dog", "cat"))
})

# 2w*2w*2b*2b ----
test_that("2w*2w*2b*2b", {
  within <- list(
    time = c(day = "day time", night = "night time"), 
    condition = c(A = "condition A", B = "condition B")
  )
  between <- list(
    pet = c(dog = "has dogs", cat = "has cats"), 
    age = c(old = "older", young = "younger")
  )
    
  design <- check_design(within, between, plot = FALSE)
  
  cells_w <- c("day_A", "night_A", "day_B", "night_B")
  cells_b <- c("dog_old", "cat_old", "dog_young", "cat_young")
  cell_n <- matrix(rep(100,16), 4, dimnames = list(cells_b, cells_w)) %>% as.data.frame()
  cell_mu <- matrix(rep(0,16), 4, dimnames = list(cells_b, cells_w)) %>% as.data.frame()
  cell_sd <- matrix(rep(1,16), 4, dimnames = list(cells_b, cells_w)) %>% as.data.frame()
  
  expect_equal(design$cells_w, cells_w)
  expect_equal(design$cells_b, cells_b)
  expect_equal(design$cell_n, cell_n)
  expect_equal(design$cell_mu, cell_mu)
  expect_equal(design$cell_sd, cell_sd)
})

# design spec ----
test_that("design spec", {
  between <- list(
    "B" = c("B1", "B2")
  )
  within <- list(
    "W" = c("W1", "W2")
  )
  n <- list(
    "B1" = 60,
    "B2" = 40
  )
  mu <- list(
    "B1" = c(10, 20),
    "B2" = c(10, 30)
  )
  sd <- list(
    "B1" = c(3, 4),
    "B2" = c(5, 6)
  )
  r <- list(
    "B1" = .2,
    "B2" = .5
  )
  
  design <- check_design(within, between, n, mu, sd, r, plot = FALSE)
  
  design_elements <- c("within", "between", "cells_w", "cells_b", 
                       "cell_n", "cell_mu", "cell_sd", "cell_r")
  
  expect_equal(names(design), design_elements)
})

# anon factors ----
test_that("anon factors", {
  design <- check_design(c(2, 4), c(2, 2), plot = FALSE)
  
  w <- list(
    A = c(A1="A1", A2="A2"),
    B = c(B1="B1", B2="B2", B3="B3", B4="B4")
  )
  
  b <- list(
    C = c(C1="C1",C2="C2"),
    D = c(D1="D1", D2="D2")
  )
  
  expect_equal(design$within, w)
  expect_equal(design$between, b)
})

# make_id ----
test_that("make_id", {
  expect_equal(make_id(10), c("S01", "S02", "S03", "S04", "S05", 
                              "S06", "S07", "S08", "S09", "S10"))
  
  expect_equal(make_id(10, "SUB"), c("SUB01", "SUB02", "SUB03", "SUB04", "SUB05", 
                                     "SUB06", "SUB07", "SUB08", "SUB09", "SUB10"))
  
  expect_equal(make_id(100)[[1]], "S001")
  expect_equal(make_id(1000)[[1]], "S0001")
  expect_equal(make_id(1000, "pokemon_")[[1]], "pokemon_0001")
  expect_equal(make_id(100, digits = 4)[[1]], "S0001")
  
  # named arguments
  expect_equal(make_id(n = 100, prefix = "A", digits = 4)[[1]], "A0001")
  expect_equal(make_id(digits = 4, prefix = "A", n = 100)[[1]], "A0001")
  
  # vector
  expect_equal(make_id(2:4), c("S2", "S3", "S4"))
  expect_equal(make_id(100:200)[[1]], "S100")
})
