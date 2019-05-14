context("rnorm_pre")

test_that("error messages", {
  expect_error(rnorm_pre(matrix(1:9, 3, 3)), "x must be a vector")
  expect_error(rnorm_pre(letters), "x must be numeric")
  expect_error(rnorm_pre(rnorm(2)), "x must have length > 2")
})

test_that("correct default parameters", {
  n <- sample(10:100, 1)
  v1 <- rnorm(n)
  v2 <- rnorm_pre(v1, empirical = TRUE)
  
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
  v2 <- rnorm_pre(v1, ymean, ysd, rho, empirical = TRUE)
  
  testrho <- cor(v1, v2)
  testymean <- mean(v2)
  testysd <- sd(v2)
  
  expect_equal(rho, testrho)
  expect_equal(ymean, testymean)
  expect_equal(ysd, testysd)
})

# sample_from_pop ----
test_that("calculations for sample_from_pop", {
  reps <- 1000
  tol <- 0.01
  
  # SD of sample correlations = sqrt(1/n) * (1-r^2)
  sim_r <- function(n, r) {
    r_sd <-  purrr::map_dbl(1:(reps/10), ~{
      x <- rnorm_multi(n, 2, 0, 1, r)
      cor(x$X1, x$X2)
      }) %>% sd()
    data.frame(n = n, r = r, r_sd = r_sd)
  }
  
  ns <- c(50, 100, 200) %>% rep(each = 6)
  rs <- seq(0, .5, by = 0.1) %>% rep(times = 3)
  sim_r_df <- purrr::map2_df(ns, rs, sim_r)
  
  r_diff <- sim_r_df %>%
    dplyr::mutate(pred = sqrt(1/n) * (1-r^2),
                  diff = abs(r_sd - pred)) %>%
    dplyr::pull(diff) %>% 
    mean()
  
  expect_equal(r_diff, 0, tolerance = tol)
  
  
  # SD of sample means = sd / sqrt(n)
  sim_m <- function(n, sd) {
    mu_sd <- purrr::map_dbl(1:reps, ~rnorm(n, 100, sd) %>% mean()) %>% sd()
    data.frame(n = n, sd = sd, mu_sd = mu_sd)
  }
  
  ns <- c(50, 100, 200, 400) %>% rep(each = 3)
  sds <- 1:3 %>% rep(times = 4)
  sim_m_df <- purrr::map2_df(ns, sds, sim_m)
  
  mu_diff <- sim_m_df %>%
    dplyr::mutate(pred = sd / sqrt(n),
                  diff = abs(mu_sd - pred)) %>%
    dplyr::pull(diff) %>% 
    mean()
  
  expect_equal(mu_diff, 0, tolerance = tol)
  
  # SD of sample SDs = sd / sqrt(2*n)
  sim_sd <- function(n, sd) {
    sd_sd <- purrr::map_dbl(1:reps, ~rnorm(n, 1, sd) %>% sd()) %>% sd()
    data.frame(n = n, sd = sd, sd_sd = sd_sd)
  }

  sim_sd_df <- purrr::map2_df(ns, sds, sim_sd)
  
  sd_diff <- sim_sd_df %>%
    dplyr::mutate(pred = sd / sqrt(2*n),
           diff = abs(sd_sd - pred)) %>%
    dplyr::pull(diff) %>% 
    mean()
  
  expect_equal(sd_diff, 0, tolerance = tol)
  
})

test_that("empirical = FALSE", {
  n <- 100
  r <- 0
  mu <- 0
  sd <- 1
  simdat <- purrr::map_df(1:1000, ~{
    v1 <- rnorm(n)
    v2 <- rnorm_pre(v1, mu, sd, r)
    
    data.frame(
      sd = sd(v2),
      mu = mean(v2),
      r = cor(v1, v2)
    )
  })
  
  expect_equal(mean(simdat$mu), mu, tolerance = .01)
  expect_equal(mean(simdat$sd), sd, tolerance = .01)
  expect_equal(mean(simdat$r), r, tolerance = .01)
  
  expect_equal(sd(simdat$mu), sd / sqrt(n), tolerance = .01)
  expect_equal(sd(simdat$sd), sd / sqrt(2*n), tolerance = .01)
  expect_equal(sd(simdat$r), sqrt(1/n) * (1-r^2), tolerance = .01)
})
