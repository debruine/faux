context("test-interactive_design")

test_that("test", {
  skip("interactive is too annoying")

  within <- list(A = c("A1", "A2", "A3"))
  between <- list(B = c("B1", "B2"))
  n <- 10
  mu <- list(
    B1 = 1:3,
    B2 = 4:6
  )
  sd <- 1
  r <- list(
    B1 = .5,
    B2 = c(.4, .5, .6)
  )
  des2 <- check_design(within, between, n, mu, sd, r, plot = FALSE)
  
  des <- interactive_design()
  
  expect_equal(des$within,  des2$within)
  expect_equal(des$between, des2$between)
  expect_equal(des$cell_n,  des2$cell_n)
  expect_equal(des$cell_mu, des2$cell_mu)
  expect_equal(des$cell_sd, des2$cell_sd)
  expect_equal(des$cell_r,  des2$cell_r)
})
