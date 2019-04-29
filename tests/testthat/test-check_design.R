context("test-check_design")

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
                       "within_labels",  "cell_n", "cell_mu", "cell_sd", 
                       "cell_cors", "cells_w", "cells_b", "sub_id")
  
  expect_equal(names(design), design_elements)
})
