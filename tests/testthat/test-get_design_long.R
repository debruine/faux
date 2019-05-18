context("test-get_design_long")

# 2w ----
test_that("2w", {
  within <- list(time = c("day", "night"))
  between <- list()
  mu <- c(1,2)
  d <- check_design(within, between, mu = mu, plot = FALSE)
  data <- faux:::sim_design_(d, long = TRUE, empirical= TRUE)
  d2 <- get_design_long(data, plot = FALSE)
  
  expect_equal(d$within, d2$within)
  expect_equivalent(d$between, d2$between)
  expect_equal(d$n, d2$n)
  expect_equal(d$mu, d2$mu)
  expect_equal(d$sd, d2$sd)
  expect_equal(d$r, d2$r)
})

# get_design_long ----
test_that("get_design_long", {
  design <- check_design(2, 2, n = 10, mu = 5, sd = 2, r = 0.5)
  df_long <- sim_design(design = design, long = TRUE, empirical = TRUE)
  d <- get_design_long(df_long, plot = FALSE)
  
  n <- data.frame(A1 = c(10, 10), A2 = c(10, 10), row.names = c("B1", "B2"))
  mu <- data.frame(A1 = c(5, 5), A2 = c(5, 5), row.names = c("B1", "B2"))
  sd <- data.frame(A1 = c(2, 2), A2 = c(2, 2), row.names = c("B1", "B2"))
  r <- data.frame(A1 = c(1, .5), A2 = c(.5, 1), row.names = c("A1", "A2")) %>% as.matrix()
  
  expect_equal(d$within, list(A = list(A1="A1", A2="A2")))
  expect_equal(d$between, list(B = list(B1="B1", B2="B2")))
  expect_equal(d$n, n)
  expect_equal(d$mu, mu)
  expect_equal(d$sd, sd)
  expect_equal(d$r$B1, r)
  expect_equal(d$r$B2, r)
  
  df_long <- sim_design(c(2, 2, 2), c(2, 2, 2), long = TRUE, empirical = TRUE)
  d <- get_design_long(df_long, plot = FALSE)
  
  expect_equal(d$mu %>% names(), c("A1_B1_C1", "A2_B1_C1", "A1_B2_C1", "A2_B2_C1", 
                            "A1_B1_C2", "A2_B1_C2", "A1_B2_C2", "A2_B2_C2"))
  expect_equal(d$mu %>% row.names(), c("D1_E1_F1", "D2_E1_F1", "D1_E2_F1", "D2_E2_F1", 
                            "D1_E1_F2", "D2_E1_F2", "D1_E2_F2", "D2_E2_F2"))
  expect_equal(d$n  %>% sum(), 6400)
  expect_equal(d$mu %>% sum(), 0)
  expect_equal(d$sd %>% sum(), 64)
  expect_equal(d$r[[1]] %>% sum(), 8)
})

test_that("2w*2b", {
  within <- list(time = c("night", "day"))
  between <- list(pet = c("dog", "cat"))
  mu <- list(
    dog = c(1,2),
    cat = c(2,3)
  )
  d <- check_design(within, between, mu = mu, plot = FALSE)
  data <- sim_design(within, between, mu = mu, long = TRUE, empirical = TRUE)
  d2 <- get_design_long(data, plot = FALSE)
  expect_equal(d$within, d2$within)
  expect_equivalent(d$between, d2$between)
  expect_equal(d$n, d2$n)
  expect_equal(d$mu, d2$mu)
  expect_equal(d$sd, d2$sd)
  expect_equal(d$r, d2$r)
})
