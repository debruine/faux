context("test-distributions")

# error ----
test_that("error", {
  missing_x <- "argument \"x\" is missing, with no default"
  expect_error(norm2unif(), missing_x)
  expect_error(norm2pois(), missing_x)
  expect_error(norm2binom(), missing_x)
  expect_error(unif2norm(), missing_x)
  
  expect_error(norm2pois(1), "argument \"lambda\" is missing, with no default")
})

reps <- 1

# unif2norm ----
test_that("unif2norm", {
  for (i in 1:reps) {
    mu <- runif(1, -100, 100)
    sd <- runif(1, 1, 5)
    
    s <- purrr::map_df(1:1000, ~{
      x <- runif(100)
      y <- unif2norm(x, mu, sd)
      y2 <- rnorm(100, mu, sd)
      
      list(m_1 = mean(y), sd_1 = sd(y),
           m_2 = mean(y2), sd_2 = sd(y2))
    }) 
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(mu, sd, mu, sd), tolerance = 0.1)
  }
})

# norm2unif ----
test_that("norm2unif", {
  for (i in 1:reps) {
    min <- runif(1, -100, 100)
    max <- min + runif(1, 1, 100)
    
    s <- purrr::map_df(1:1000, ~{
      x <- rnorm(100)
      y <- norm2unif(x, min, max)
      
      y2 <- runif(100, min, max)
      list(min_1 = min(y), max_1 = max(y),
           min_2 = min(y2), max_2 = max(y2)
      )
    })
    
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(min, max, min, max), tolerance = 0.1)
  }
})

# norm2pois ----
test_that("norm2pois", {
  for (i in 1:reps) {
    lambda <- sample(1:10, 1)
    
    s <- purrr::map_df(1:1000, ~{
      x <- rnorm(100)
      y <- norm2pois(x, lambda)
      
      y2 <- rpois(100, lambda)
      list(m_1 = mean(y), m_2 = mean(y2)
      )
    })
    
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(lambda, lambda), tolerance = 0.1)
  }
})

# norm2binom ----
test_that("norm2binom", {
  for (i in 1:reps) {
    prob <- runif(1)
    size <- sample(1:10, 1)
    
    s <- purrr::map_df(1:1000, ~{
      x <- rnorm(100)
      y <- norm2binom(x, size, prob)
      
      y2 <- rbinom(100, size, prob)
      list(m_1 = mean(y), m_2 = mean(y2)
      )
    })
    
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(prob*size, prob*size), tolerance = 0.1)
  }
})

# norm2trunc ----
test_that("norm2trunc", {
  for (i in 1:reps) {
    min <- runif(1, -100, 100)
    max <- min + runif(1, 1, 100)
    mu <- (max + min)/2
    sd <- runif(1, 1, 5)
    
    s <- purrr::map_df(1:1000, ~{
      x <- rnorm(100, mu, sd)
      y <- norm2trunc(x, min, max, mu, sd)
      
      y2 <- truncnorm::rtruncnorm(100, min, max, mu, sd)
      list(m_1 = mean(y), sd_1 = sd(y),
           m_2 = mean(y2), sd_2 = sd(y2))
    }) 
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(mu, sd, mu, sd), tolerance = 0.1)
  }
})

# trunc2norm ----
test_that("trunc2norm", {
  for (i in 1:reps) {
    min <- runif(1, -100, 100)
    max <- min + runif(1, 1, 100)
    mu <- (max + min)/2
    sd <- runif(1, 1, 5)
    
    s <- purrr::map_df(1:1000, ~{
      x <- truncnorm::rtruncnorm(100, min, max, mu, sd)
      y <- trunc2norm(x, min, max, mu, sd)
      
      y2 <- rnorm(100, mu, sd)
      list(m_1 = mean(y), sd_1 = sd(y),
           m_2 = mean(y2), sd_2 = sd(y2))
    }) 
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(mu, sd, mu, sd), tolerance = 0.1)
  }
})

# norm2likert ----
test_that("norm2likert", {
  x <- rnorm(1e4)
  
  expect_error(norm2likert(), "argument \"prob\" is missing, with no default")
  expect_error(norm2likert(x), "argument \"prob\" is missing, with no default")
  expect_error(norm2likert(x, 2), "argument \"prob\" must add up to 1")
  
  y <- norm2likert(x, c(.25, .5, .25))
  expect_equal(mean(y == 1), .25, tolerance = 0.1)
  expect_equal(mean(y == 2), .50, tolerance = 0.1)
  expect_equal(mean(y == 3), .25, tolerance = 0.1)
  
  y <- norm2likert(x, c(.1, .2, .3, .4))
  expect_equal(mean(y == 1), .1, tolerance = 0.1)
  expect_equal(mean(y == 2), .2, tolerance = 0.1)
  expect_equal(mean(y == 3), .3, tolerance = 0.1)
  expect_equal(mean(y == 4), .4, tolerance = 0.1)
  
  y <- norm2likert(x, c(.4, .3, .2, .1))
  expect_equal(mean(y == 1), .4, tolerance = 0.1)
  expect_equal(mean(y == 2), .3, tolerance = 0.1)
  expect_equal(mean(y == 3), .2, tolerance = 0.1)
  expect_equal(mean(y == 4), .1, tolerance = 0.1)
})
