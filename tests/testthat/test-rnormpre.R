context("rnormpre")

test_that("error messages", {
  expect_error( rnormpre(matrix(1:9, 3, 3)), "x must be a vector" )
  expect_error( rnormpre(letters), "x must be numeric" )
  expect_error( rnormpre(rnorm(2)), "x must have length > 2" )
})

test_that("correct default parameters", {
  n <- sample(10:100, 1)
  v1 <- rnorm(n)
  v2 <- rnormpre(v1)
  
  rho <- cor(v1, v2)
  ysd <- sd(v2)
  ymean <- mean(v2)
  
  expect_equal(rho, 0)
  expect_equal(ymean, 0)
  expect_equal(ysd, 1)
})

test_that("correct specified parameters", {
  n <- sample(10:100, 1)
  v1 <- rnorm(n)
  rho <- runif(1) * sample(c(-1, 1), 1)
  ymean <- rnorm(1, 0, 100)
  ysd <- runif(1, 0.001, 100)
  v2 <- rnormpre(v1, rho, ymean, ysd)
  
  testrho <- cor(v1, v2)
  testymean <- mean(v2)
  testysd <- sd(v2)
  
  expect_equal(rho, testrho)
  expect_equal(ymean, testymean)
  expect_equal(ysd, testysd)
})