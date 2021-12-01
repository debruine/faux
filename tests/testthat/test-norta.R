test_that("convert_r warnings", {
  # warnings and errors
  expect_warning(convert_r(.95, "binom", params1 = list(size = 1, prob = 0.5)))
  expect_error(convert_r(0.3, "nope1"))
  expect_error(convert_r(0.3, "unif", "nope2"))
})

test_that("convert_r", {
  # norm
  r <- seq(-.75, .75, .25)
  x <- sapply(r, convert_r)
  #plot(r, x)
  expect_true(all(abs(r - x) < .001))
  
  # norm with changed SD
  r <- seq(-.75, .75, .25)
  x <- sapply(r, convert_r, 
              params1 = list(mean = 10, sd = 5),
              params2 = list(mean = -10, sd = 2))
  #plot(r, x)
  expect_true(all(abs(r - x) < .001))
  
  # norm:binom
  r <- c(-2:-1, 1:2)/5
  x <- sapply(r, convert_r, dist2 = "binom",
              params2 = list(size = 2, prob = 0.25))
  # expect linear relationship, so low-SD of ratio
  expect_true(sd(r/x) < .02)
  
  check_dist <- rnorm_multi(1e5, 2, r = x[1], empirical = TRUE)
  check_dist$X2 <- norm2binom(check_dist$X2, size = 2, prob = 0.5, mu = 0, sd = 1)
  cor(check_dist$X1, check_dist$X2)
})
