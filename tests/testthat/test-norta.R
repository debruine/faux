r <- c(-.5, .2, .5)
set.seed(8675309)

# convert_r ----
test_that("convert_r warnings", {
  # warnings and errors
  expect_warning(convert_r(.95, "binom", params1 = list(size = 1, prob = 0.5)))
  expect_error(convert_r(0.3, "nope1"))
  expect_error(convert_r(0.3, "unif", "nope2"))
})

## norm:norm ----
test_that("convert_r norm:norm", {
  x <- sapply(r, convert_r)
  #plot(r, x)
  expect_true(all(abs(r - x) < .001))
  
  # norm with changed SD
  x <- sapply(r, convert_r, 
              params1 = list(mean = 10, sd = 5),
              params2 = list(mean = -10, sd = 2))
  #plot(r, x)
  expect_true(all(abs(r - x) < .001))
})

## norm:binom ----
test_that("convert_r norm:binom", {
  size <- 2
  prob <- .5
  x <- sapply(r, convert_r, dist2 = "binom",
              params2 = list(size = size, prob = prob))
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    check_dist$X2 <- norm2binom(check_dist$X2, 
                                size = size, 
                                prob = prob, 
                                mu = 0, sd = 1)
    cor(check_dist$X1, check_dist$X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})

## binom:binom
test_that("convert_r binom:binom", {
  size1 <- 2
  prob1 <- .5
  size2 <- 3
  prob2 <- 1/3
  x <- sapply(r, convert_r, 
              dist1 = "binom", dist2 = "binom",
              params1 = list(size = size1, prob = prob1),
              params2 = list(size = size2, prob = prob2))
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    X1 <- norm2binom(check_dist$X1, 
                      size = size1, 
                      prob = prob1, 
                      mu = 0, sd = 1)
    X2 <- norm2binom(check_dist$X2, 
                      size = size2, 
                      prob = prob2, 
                      mu = 0, sd = 1)
    cor(X1, X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})

## norm:pois ----
test_that("convert_r norm:pois", {
  lambda <- 1.5
  x <- sapply(r, convert_r, dist2 = "pois",
              params2 = list(lambda = lambda))
  #plot(r, x)
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    X1 <- check_dist$X1
    X2 <- norm2pois(check_dist$X2, 
                    lambda = lambda, 
                    mu = 0, sd = 1)
    cor(X1, X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})

## pois:pois ----
test_that("convert_r pois:pois", {
  lambda1 <- 1
  lambda2 <- 2
  x <- sapply(r, convert_r, 
              dist1 = "pois",
              dist2 = "pois",
              params1 = list(lambda = lambda1),
              params2 = list(lambda = lambda2))
  #plot(r, x)
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    X1 <- norm2pois(check_dist$X1, 
                    lambda = lambda1, 
                    mu = 0, sd = 1)
    X2 <- norm2pois(check_dist$X2, 
                    lambda = lambda2, 
                    mu = 0, sd = 1)
    cor(X1, X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})

## norm:beta ----
test_that("convert_r norm:beta", {
  shape1 <- 1.1
  shape2 <- 1.2
  x <- sapply(r, convert_r, dist2 = "beta",
              params2 = list(shape1 = shape1,
                             shape2 = shape2))
  #plot(r, x)
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    X1 <- check_dist$X1
    X2 <- norm2beta(check_dist$X2, 
                    shape1 = shape1, 
                    shape2 = shape2,
                    mu = 0, sd = 1)
    cor(X1, X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})

## norm:gamma ----
test_that("convert_r norm:gamma", {
  shape <- 1.5
  rate <- 1.2
  x <- sapply(r, convert_r, dist2 = "gamma",
              params2 = list(shape = shape,
                             rate = rate))
  #plot(r, x)
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    X1 <- check_dist$X1
    X2 <- norm2gamma(check_dist$X2, 
                    shape = shape,
                    rate = rate,
                    mu = 0, sd = 1)
    cor(X1, X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})

## norm:cauchy ----
test_that("convert_r norm:cauchy", {
  skip("cauchy max r depends on n")
  location <- 0
  scale <- 1
  x <- sapply(r, convert_r, dist2 = "cauchy",
              params2 = list(location = location,
                             scale = scale))
  #plot(r, x)
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    X1 <- check_dist$X1
    X2 <- pnorm(check_dist$X2, mean = 0, sd = 1) %>%
      qcauchy(location, scale)
    cor(X1, X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})


## norm:likert ----
test_that("convert_r norm:likert", {
  prob <- c(10, 20, 40, 20, 10)
  labels <- LETTERS[1:length(prob)]
  x <- sapply(r, convert_r, dist2 = "likert",
              params2 = list(prob = prob, 
                             labels = labels))
  #plot(r, x)
  
  recovered_r <- sapply(x, function(adj_r) {
    check_dist <- rnorm_multi(1e5, 2, r = adj_r, empirical = TRUE)
    X1 <- check_dist$X1
    X2 <- norm2likert(check_dist$X2, 
                      prob = prob,
                      labels = labels,
                      mu = 0, sd = 1) %>% as.numeric()
    cor(X1, X2)
  })
  diff <- abs(recovered_r - r)
  expect_true(all(diff < .01))
})

# rmulti ----
test_that("rmulti", {
  r <- seq(.1, .6, .1)
  dist <- c(A = "norm", B = "binom", C = "beta", D = "pois")
  params <- list(A = list(mean = 10, sd = 5), 
                 B = list(size = 6, prob = 0.5),
                 C = list(shape1 = 2, shape2 = 2),
                 D = list(lambda = 10))
  
  x <- rmulti(n = 100, 
              dist = dist, 
              params = params, 
              r = r, 
              empirical = TRUE)
  
  recov_r <- cor(x)
  diff <- abs(recov_r[upper.tri(recov_r)] - r)
  expect_true(all(diff < .05))
})
