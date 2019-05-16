context("test-interactive_design")

test_that("test", {
  des <- interactive_design()
  
  #des <- check_design(d$within, d$between, d$n, d$mu, d$sd, d$r, plot = FALSE)
  des2 <- check_design(2, 2, plot = FALSE)
  
  expect_equal(des$within, des2$within)
  expect_equal(des$between, des2$between)
  expect_equal(des$cell_n, des2$cell_n)
  expect_equal(des$cell_mu, des2$cell_mu)
  expect_equal(des$cell_sd, des2$cell_sd)
  expect_equal(des$cell_r, des2$cell_r)
})
