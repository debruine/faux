context("test-norm2unif")

# error ----
test_that("error", {
  missing_x <- "argument \"x\" is missing, with no default"
  expect_error(norm2unif(), missing_x)
  expect_error(norm2pois(), missing_x)
  expect_error(norm2binom(), missing_x)
  expect_error(unif2norm(), missing_x)
  
  expect_error(norm2pois(1), "argument \"lambda\" is missing, with no default")
})

# unif2norm ----
test_that("unif2norm", {
  for (i in 1:3) {
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
  for (i in 1:3) {
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
  for (i in 1:3) {
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
  for (i in 1:3) {
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
