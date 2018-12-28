context("simdf_mixed")

test_that("default settings", {
  testthat::skip_on_cran()
  s10 <- simdf_mixed(faceratings, 10, 10)
  
  expect_equal(nrow(s10), 100)
  expect_equal(ncol(s10), 5)
})

test_that("specified dv and IDs", {
  testthat::skip_on_cran()
  s10 <- simdf_mixed(faceratings, 10, 10, "rating", "rater_id", "face_id")
  
  expect_equal(nrow(s10), 100)
  expect_equal(ncol(s10), 5)
})
