context("rnorm_multi")

test_that("error messages", {
  expect_warning(rnorm_multi(n = 1, empirical = TRUE),
    "When n = 1 and empirical = TRUE, returned data are equal to mu")
  
  expect_error(rnorm_multi(), "argument \"n\" is missing, with no default")
  expect_error(rnorm_multi(-1), "n must be an integer > 0")
  expect_error(rnorm_multi(10.3), "n must be an integer > 0")
  expect_error(rnorm_multi("A"), "n must be an integer > 0")
  
  expect_warning(rnorm_multi(100, 3, cors = .5), "cors is deprecated, please use r")
  
  expect_error(
    rnorm_multi(10, 3, 1:2),
    "the length of mu must be 1 or vars"
  )
  
  expect_error(
    rnorm_multi(10, 3, 0, 1:2),
    "the length of sd must be 1 or vars"
  )
  
  expect_error(
    rnorm_multi(10, empirical = NA),
    "empirical must be TRUE or FALSE"
  )
  
  expect_error(
    rnorm_multi(10, 3, 0, 1, matrix("A", 3, 3)),
    "cors matrix not numeric" 
  )
  expect_error(
    rnorm_multi(10, 3, 0, 1, matrix(0.5, 4, 2)),
    "cors matrix wrong dimensions" 
  )
  
  m <- matrix(c(1, .5, .5, .5, 1, .5, .5, .75, 1), 3)
  expect_error( 
    rnorm_multi(10, 3, 0, 1, m), 
    "cors matrix not symmetric"
  )
  
  m <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 0), 3)
  expect_error(
    rnorm_multi(10, 3, 0, 1, m),
    "correlation matrix not positive definite"
  )
  
  cors <- c(-0.06826927, -0.89756943, -0.45636273)
  expect_error(
    rnorm_multi(10, 3, 0, 1, cors),
    "correlation matrix not positive definite"
  )
})

test_that("correct default parameters", {
  n <- 1e5
  dat <- rnorm_multi(n)
  r <- cor(dat)
  means <- dplyr::summarise_all(dat, mean)
  sds <- dplyr::summarise_all(dat, stats::sd)
  
  expect_equal(nrow(dat), n)
  expect_equal(ncol(dat), 3)
  expect_equal(means, data.frame(X1 = 0, X2 = 0, X3 = 0), tolerance = 0.05)
  expect_equal(sds, data.frame(X1 = 1, X2 = 1, X3 = 1), tolerance = 0.05)
})

test_that("correct default parameters with empirical = TRUE", {
  n <- 50
  dat <- rnorm_multi(n, empirical = TRUE)
  r <- cor(dat)
  means <- dplyr::summarise_all(dat, mean)
  sds <- dplyr::summarise_all(dat, stats::sd)
  
  expect_equal(nrow(dat), n)
  expect_equal(ncol(dat), 3)
  expect_equal(means, data.frame(X1 = 0, X2 = 0, X3 = 0))
  expect_equal(sds, data.frame(X1 = 1, X2 = 1, X3 = 1))
})

# names ----
test_that("names", {
  cmat <- cor(iris[,1:4])
  dat <- rnorm_multi(10, vars = 4, r = cmat)
  
  expect_equal(colnames(dat), colnames(cmat))
})

# matrix ----
test_that("matrix", {
  dat <- mat <- rnorm_multi(10, vars = 4, r = cor(iris[,1:4]))
  mat <- rnorm_multi(10, vars = 4, r = cor(iris[,1:4]), as.matrix = TRUE)
  
  expect_true(class(dat) == "data.frame")
  expect_true(class(mat) == "matrix")
})

# small n ----
test_that("small n", {
  n1 <- rnorm_multi(n = 1, vars = 1, r = 0.5)
  expect_equal(names(n1), "X1")
  expect_equal(nrow(n1), 1)
  
  n1 <- rnorm_multi(n = 1, vars = 2, r = 0.5)
  expect_equal(names(n1), c("X1", "X2"))
  expect_equal(nrow(n1), 1)
  
  n1 <- rnorm_multi(n = 1, vars = 10, r = 0.5)
  expect_equal(names(n1), c("X01", "X02", "X03", "X04", "X05", 
                            "X06", "X07", "X08", "X09", "X10"))
  expect_equal(nrow(n1), 1)
  
  v1 <- rnorm_multi(n = 10, vars = 1, r = 0.5)
  expect_equal(names(v1), "X1")
  expect_equal(nrow(v1), 10)
  
  expect_warning(
    source <- rnorm_multi(n = 1, 5, 1:5, empirical = TRUE),
    "When n = 1 and empirical = TRUE, returned data are equal to mu")
  target <- data.frame(X1 = 1, X2 = 2, X3 = 3, X4 = 4, X5 = 5)
  expect_equal(source,  target)
  
  expect_error(
    rnorm_multi(n = 2, vars = 3, r = 0.5, empirical = TRUE),
    "The correlated variables could not be generated."
  )
})