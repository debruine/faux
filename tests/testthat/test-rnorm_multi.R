context("rnorm_multi")

test_that("error messages", {
  expect_error(rnorm_multi(), "argument \"n\" is missing, with no default")
  expect_error(rnorm_multi(2), "n must be an integer > 2")
  expect_error(rnorm_multi(10.3), "n must be an integer > 2")
  expect_error(rnorm_multi("A"), "n must be an integer > 2")
  
  expect_error(
    rnorm_multi(10, 3, 0, 1:2),
    "the length of mu must be 1 or vars"
  )
  
  expect_error(
    rnorm_multi(10, empirical = NA),
    "empirical must be TRUE or FALSE"
  )
  
  expect_error(
    rnorm_multi(10, 3, 0, 1, 1:2),
    "the length of sd must be 1 or vars"
  )
  expect_error(
    rnorm_multi(10, 3, matrix("A", 3, 3)),
    "cors matrix not numeric" 
  )
  expect_error(
    rnorm_multi(10, 3, matrix(0.5, 4, 2)),
    "cors matrix wrong dimensions" 
  )
  
  m <- matrix(c(1, .5, .5, .5, 1, .5, .5, .75, 1), 3)
  expect_error( 
    rnorm_multi(10, 3, m), 
    "cors matrix not symmetric"
  )
  
  m <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 0), 3)
  expect_error(
    rnorm_multi(10, 3, m),
    "correlation matrix not positive definite"
  )
  
  cors <- c(-0.06826927, -0.89756943, -0.45636273)
  expect_error(
    rnorm_multi(10, 3, cors),
    "correlation matrix not positive definite"
  )
})

test_that("correct default parameters", {
  n <- 1e5
  dat <- rnorm_multi(n)
  cors <- cor(dat)
  means <- dplyr::summarise_all(dat, mean)
  sds <- dplyr::summarise_all(dat, sd)
  
  expect_equal(nrow(dat), n)
  expect_equal(ncol(dat), 3)
  expect_equal(means, data.frame(X1 = 0, X2 = 0, X3 = 0), tolerance = 0.05)
  expect_equal(sds, data.frame(X1 = 1, X2 = 1, X3 = 1), tolerance = 0.05)
})

test_that("correct default parameters with empirical = TRUE", {
  n <- 50
  dat <- rnorm_multi(n, empirical = TRUE)
  cors <- cor(dat)
  means <- dplyr::summarise_all(dat, mean)
  sds <- dplyr::summarise_all(dat, sd)
  
  expect_equal(nrow(dat), n)
  expect_equal(ncol(dat), 3)
  expect_equal(means, data.frame(X1 = 0, X2 = 0, X3 = 0))
  expect_equal(sds, data.frame(X1 = 1, X2 = 1, X3 = 1))
})