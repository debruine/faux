context("test-check_design")

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
    
  
  testthat::expect_equal(design$within, within)
  testthat::expect_equal(design$between, between)
  testthat::expect_equal(design$within_labels, list(time = c(day = "day", night = "night")))
  testthat::expect_equal(design$between_labels, list(pet = c(dog = "dog", cat = "cat")))
  
  testthat::expect_equal(design$cell_n, cell_n)
  testthat::expect_equal(design$cell_mu, cell_mu)
  testthat::expect_equal(design$cell_sd, cell_sd)
  
  testthat::expect_equal(design$cells_w, c("day", "night"))
  testthat::expect_equal(design$cells_b, c("dog", "cat"))
  testthat::expect_equal(design$sub_id, c(paste0("S0", 1:9), paste0("S", 10:20)))
})

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
  
  design <- check_design(within, between, n, r, mu, sd)
  
  design_elements <- c("within", "between", "within_factors", "between_factors", 
                       "within_labels", "between_labels", "cell_n", "cell_mu", "cell_sd", 
                       "cell_cors", "cells_w", "cells_b", "sub_id")
  
  expect_equal(names(design), design_elements)
})
