context("faux_options")

test_that("set", {
  faux_options(sep = ".")
  expect_equal(faux_options("sep"), ".")
  
  faux_options(sep = "_")
  expect_equal(faux_options("sep"), "_")
})
