context("sample_from_pop")

# sample_from_pop ----
test_that("calculations for sample_from_pop", {
  skip("just proving something to myself")
  
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
  
  expect_equal(r_diff, 0, tolerance = tol , check.environment=FALSE)
  
  
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
  
  expect_equal(mu_diff, 0, tolerance = tol , check.environment=FALSE)
  
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
  
  expect_equal(sd_diff, 0, tolerance = tol, check.environment=FALSE)
  
})