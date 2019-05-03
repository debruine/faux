context("test-check_design")

# basic ----
test_that("basic", {
  within <- list(time = c("day", "night"))
  between <- list(pet = c("dog", "cat"))
  design <- check_design(within, between, n = 10)

  cell_n <- data.frame(dog = c(10,10), cat = c(10,10)) %>% 
    magrittr::set_rownames(c("day", "night"))
  cell_mu <- data.frame(dog = c(0,0), cat = c(0,0)) %>% 
    magrittr::set_rownames(c("day", "night"))
  cell_sd <- data.frame(dog = c(1,1), cat = c(1,1)) %>% 
    magrittr::set_rownames(c("day", "night"))
    
  
  expect_equal(design$within, within)
  expect_equal(design$between, between)
  expect_equal(design$within_labels, list(time = c(day = "day", night = "night")))
  expect_equal(design$between_labels, list(pet = c(dog = "dog", cat = "cat")))
  
  expect_equal(design$cell_n, cell_n)
  expect_equal(design$cell_mu, cell_mu)
  expect_equal(design$cell_sd, cell_sd)
  
  expect_equal(design$cells_w, c("day", "night"))
  expect_equal(design$cells_b, c("dog", "cat"))
  expect_equal(design$sub_id, c(paste0("S0", 1:9), paste0("S", 10:20)))
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
  
  design <- check_design(within, between, n, mu, sd, r)
  
  design_elements <- c("within", "between", "within_factors", "between_factors", 
                       "within_labels", "between_labels", "cells_w", "cells_b", 
                       "cell_n", "cell_mu", "cell_sd", "cell_r", "sub_id")
  
  expect_equal(names(design), design_elements)
})

# anon factors ----
test_that("anon factors", {
  design <- check_design(c(2, 4), c(2, 2))
  
  w <- list(
    A = c("A1", "A2"),
    B = c("B1", "B2", "B3", "B4")
  )
  
  b <- list(
    C = c("C1","C2"),
    D = c("D1", "D2")
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
