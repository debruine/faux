context("test-sim_design")

# error messages ----
test_that("error messages", {
  factors_err <- "You must specify at least one factor"
  expect_error(sim_design(), factors_err)
  expect_error(sim_design(within = list()), factors_err)
  expect_error(sim_design(between = list()), factors_err)
  
  list_err <- "within and between must be lists"
  expect_error(sim_design("1"), list_err)
  expect_error(sim_design(list(), "1"), list_err)
  
  factor_name_err <- "You have multiple factors with the same name (A). Please give all factors unique names."
  within <- list("A" = c("A1", "A2"))
  between <- list("A" = c("A1", "A2"))
  expect_error(sim_design(within, between), factor_name_err)
  
  level_err <- "You have duplicate levels for factor(s): A, C, B, D"
  within <- list("A" = c("yes", "yes"), "C" = c("C1", "C1"))
  between <- list("B" = c("B1", "B1"), "D" = c("D1", "D1"))
  expect_error(sim_design(within, between), level_err)
  
  level_err <- "You have duplicate levels for factor(s): A, B"
  within <- list("A" = c("yes", "yes"), "C" = c("C1", "C2"))
  between <- list("B" = c("yes", "yes"), "D" = c("D1", "D2"))
  expect_error(sim_design(within, between), level_err)
})

# 2w ----
test_that("2w", {
  within <- list(
    "W" = c("W1", "W2")
  )
  between <- list()
  
  df <- sim_design(within, between, empirical = TRUE)
  chk <- check_sim_stats(df)
  
  comp <- tibble::tribble(
    ~n, ~var, ~W1, ~W2, ~mean, ~sd,
    100, "W1", 1,   0,    0,     1,
    100, "W2", 0,   1,    0,     1
  )
  
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 3)
  expect_equal(names(df), c("sub_id", "W1", "W2"))
  expect_equal(chk, comp)
})

# 2w*2w ----
test_that("2w*2w", {
  within <- list(
    "W" = c("W1", "W2"),
    "X" = c("X1", "X2")
  )
  between <- list()
  
  df <- sim_design(within, between, empirical = TRUE)
  chk <- check_sim_stats(df)

  comp <- tibble::tribble(
    ~n, ~var, ~W1_X1, ~W2_X1, ~W1_X2, ~W2_X2, ~mean, ~sd,
    100, "W1_X1", 1,     0,     0,     0,     0,     1,
    100, "W2_X1", 0,     1,     0,     0,     0,     1,
    100, "W1_X2", 0,     0,     1,     0,     0,     1,
    100, "W2_X2", 0,     0,     0,     1,     0,     1
  )
  
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 5)
  expect_equal(names(df), c("sub_id", "W1_X1", "W2_X1", "W1_X2", "W2_X2"))
  expect_equal(chk, comp)
})

# 2b ----
test_that("2b", {
  between <- list(
    "B" = c("B1", "B2")
  )
  within <- list()
  
  df <- sim_design(within, between, n = 100, empirical = TRUE)
  chk <- check_sim_stats(df, grp_by = "B")
  
  comp <- tibble::tribble(
    ~B, ~n, ~var, ~val, ~mean, ~sd,
    "B1", 100, "val", 1,   0,     1,
    "B2", 100, "val", 1,   0,     1
  ) %>%
    dplyr::mutate(B = as.factor(B))
  
  expect_equal(nrow(df), 200)
  expect_equal(ncol(df), 3)
  expect_equal(names(df), c("sub_id", "B", "val"))
  expect_equal(as.data.frame(chk), as.data.frame(comp))
})

# 2b*2b ----
test_that("2b*2b", {
  between <- list(
    "A" = c("A1", "A2"),
    "B" = c("B1", "B2")
  )
  within <- list()
  
  df <- sim_design(within, between, n = 100, empirical = TRUE)
  chk <- check_sim_stats(df, grp_by = c("A","B"))
  
  comp <- tibble::tribble(
    ~A, ~B, ~n, ~var, ~val, ~mean, ~sd,
    "A1", "B1", 100, "val", 1,   0,     1,
    "A2", "B1", 100, "val", 1,   0,     1,
    "A1", "B2", 100, "val", 1,   0,     1,
    "A2", "B2", 100, "val", 1,   0,     1
  ) %>%
    dplyr::mutate(A = as.factor(A), B = as.factor(B))
  
  expect_equal(nrow(df), 400)
  expect_equal(ncol(df), 4)
  expect_equal(names(df), c("sub_id", "A", "B", "val"))
  expect_equal(as.data.frame(chk), as.data.frame(comp))
})

# 2w*2b basic ----
# uses ordering for within specification (not labels)
test_that("2w*2b basic", {
  between <- list(
    "B" = c("B1", "B2")
  )
  within <- list(
    "W" = c("W1", "W2")
  )
  n <- list(
    "B1" = 60,
    "B2" = 40
  )
  mu <- list(
    "B1" = c(10, 20),
    "B2" = c(10, 30)
  )
  sd <- list(
    "B1" = c(3, 4),
    "B2" = c(5, 6)
  )
  r <- list(
    "B1" = .2,
    "B2" = .5
  )
  
  df <- sim_design(within, between, n, r, mu, sd, TRUE)
  chk <- check_sim_stats(df, grp_by = "B")
  
  comp <- tibble::tribble(
    ~B, ~n, ~var, ~W1, ~W2, ~mean, ~sd,
    "B1", 60, "W1", 1,     0.2,    10,     3,
    "B1", 60, "W2", 0.2,   1,      20,     4,
    "B2", 40, "W1", 1,     0.5,    10,     5,
    "B2", 40, "W2", 0.5,   1,      30,     6
  ) %>%
    dplyr::mutate(B = as.factor(B))
  
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 4)
  expect_equal(names(df), c("sub_id", "B", "W1", "W2"))
  expect_equal(chk, comp)
})

# 2w*2b alt ----
# uses alternative specification for factors
test_that("2w*2b alt", {
  between <- list(
    "B" = c(B1 = "First between level", B2 = "Second between level")
  )

  within <- list(
    "W" = c(W1 = "First within level", W2 = "Second within level")
  )
  n <- list(
    B1 = 60,
    B2 = 40
  )
  mu <- list(
    B1 = c(10, 20),
    B2 = c(10, 30)
  )
  sd <- list(
    B1 = c(3, 4),
    B2 = c(5, 6)
  )
  r <- list(
    B1 = .2,
    B2 = .5
  )
  
  df <- sim_design(within, between, n, r, mu, sd, TRUE)
  chk <- check_sim_stats(df, grp_by = "B")
  
  comp <- tibble::tribble(
    ~B, ~n, ~var, ~W1, ~W2, ~mean, ~sd,
    "B1", 60, "W1", 1,     0.2,    10,     3,
    "B1", 60, "W2", 0.2,   1,      20,     4,
    "B2", 40, "W1", 1,     0.5,    10,     5,
    "B2", 40, "W2", 0.5,   1,      30,     6
  ) %>%
    dplyr::mutate(B = as.factor(B))
  
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 4)
  expect_equal(names(df), c("sub_id", "B", "W1", "W2"))
  expect_equal(chk, comp)
})


# 2w*2b within order ----
test_that("2w*2b within order", {
  between <- list(
    "B" = c("B1", "B2")
  )
  within <- list(
    "W" = c("W1", "W2")
  )
  
  mu <- list(
    B2 = c(W2 = 30, W1 = 10),
    B1 = c(W2 = 20, W1 = 10)
  )
  #get_mu_sd(mu, between[["B"]], within[["W"]])
  
  sd <- list(
    "B1" = c(W2 = 4, W1 = 3),
    "B2" = c(W2 = 6, W1 = 5)
  )
  
  df <- sim_design(within, between, 50, .5, mu, sd, TRUE)
  check_sim_stats(df, grp_by = "B")
  
  chk <- check_sim_stats(df, grp_by = "B")
  comp <- tibble::tribble(
    ~B, ~n, ~var, ~W1, ~W2, ~mean, ~sd,
    "B1", 50, "W1", 1,     0.5,    10,     3,
    "B1", 50, "W2", 0.5,   1,      20,     4,
    "B2", 50, "W1", 1,     0.5,    10,     5,
    "B2", 50, "W2", 0.5,   1,      30,     6
  ) %>%
    dplyr::mutate(B = as.factor(B))
  
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 4)
  expect_equal(names(df), c("sub_id", "B", "W1", "W2"))
  expect_equal(chk, comp)
})


# 2w*2b order ----
# change order of named list items
test_that("2w*2b order", {
  between <- list(
    "B" = c("B1", "B2")
  )
  within <- list(
    "W" = c("W1", "W2")
  )
  # if you try to specify n for each level of W, it will just use the first level
  # TODO: add a warning for this
  n <- list(
    "B2" = 40,
    "B1" = 60
  )
  mu <- list(
    "B2" = c(W1 = 10, W2 = 30),
    "B1" = c(W1 = 10, W2 = 20)
  )
  sd <- list(
    "B2" = c(W1 = 5, W2 = 6),
    "B1" = c(W1 = 3, W2 = 4)
  )
  r <- list(
    "B2" = .5,
    "B1" = .2
  )
  
  df <- sim_design(within, between, n, r, mu, sd, TRUE)
  check_sim_stats(df, grp_by = "B")
  
  chk <- check_sim_stats(df, grp_by = "B")
  comp <- tibble::tribble(
    ~B, ~n, ~var, ~W1, ~W2, ~mean, ~sd,
    "B1", 60, "W1", 1,     0.2,    10,     3,
    "B1", 60, "W2", 0.2,   1,      20,     4,
    "B2", 40, "W1", 1,     0.5,    10,     5,
    "B2", 40, "W2", 0.5,   1,      30,     6
  ) %>%
    dplyr::mutate(B = as.factor(B))
  
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 4)
  expect_equal(names(df), c("sub_id", "B", "W1", "W2"))
  expect_equal(chk, comp)
})

test_that("2w*2b*2b", {
  between <- list(
    A = c("A1", "A2"),
    B = c("B1", "B2")
  )
  within <- list(
    W = c("W1", "W2")
  )
  n <- list(
    A1_B1 = 50,
    A2_B1 = 50,
    A1_B2 = 50,
    A2_B2 = 50
  )
  mu <- list(
    A1_B1 = c(W1 = 10, W2 = 20),
    A2_B1 = c(W1 = 30, W2 = 40),
    A1_B2 = c(W1 = 50, W2 = 60),
    A2_B2 = c(W1 = 70, W2 = 80)
  )
  sd <- list(
    A1_B1 = c(W1 = 3, W2 = 4),
    A2_B1 = c(W1 = 5, W2 = 6),
    A1_B2 = c(W1 = 7, W2 = 8),
    A2_B2 = c(W1 = 9, W2 = 10)
  )
  r <- list(
    A1_B1 = .1,
    A2_B1 = .2,
    A1_B2 = .3,
    A2_B2 = .4
  )
  
  df <- sim_design(within, between, n, r, mu, sd, TRUE)
  check_sim_stats(df, grp_by = c("A", "B"))
  
  expect_equal(nrow(df), 200)
  expect_equal(ncol(df), 5)
  expect_equal(names(df), c("sub_id", "A", "B", "W1", "W2"))
})

# long format ----
test_that("long", {
  between <- list(
    "B" = c("B1", "B2"),
    "A" = c("A2", "A1")
  )
  within <- list(
    "W" = c("W1", "W2"),
    "C" = c("C2", "C1"),
    "N" = c("N2", "N1")
  )
  
  df <- sim_design(within, between, 100, .5, 0, 1, TRUE, TRUE)
  
  expect_equal(nrow(df), 3200)
  expect_equal(ncol(df), 7)
  expect_equal(names(df), c("sub_id", "B", "A", "W", "C", "N", "val"))
})

# complex names ----
test_that("complex names", {
  between <- list(
    "My first between factor" = c("Factor B1 L1", "Factor B1 L2"),
    "My second between factor" = c("Factor_B2_L1", "Factor_B2_L2")
  )
  within <- list(
    "My first within factor" = c("Factor W1 L1", "Factor W1 L2"),
    "My second within factor" = c("Factor_W2_L1", "Factor_W2_L2")
  )
  
  df_long <- sim_design(within, between, 10, .5, 0, 1, TRUE, TRUE)
  df_wide <- sim_design(within, between, 10, .5, 0, 1, TRUE, FALSE)
  
  long_names <- c("sub_id", "My first between factor", "My second between factor", 
                  "My first within factor", "My second within factor",  "val")
  
  wide_names <- c("sub_id", "My first between factor", "My second between factor", 
                  "Factor.W1.L1_Factor.W2.L1", "Factor.W1.L2_Factor.W2.L1", 
                  "Factor.W1.L1_Factor.W2.L2", "Factor.W1.L2_Factor.W2.L2")
  
  expect_equal(names(df_long), long_names)
  expect_equal(names(df_wide), wide_names)
  
  # same factor level names
  between <- list(
    pets = c("cats", "dogs"),
    pets2 = c("cats", "dogs")
  )
  within <- list(
    time = c("day", "night"),
    time2 = c("day", "night")
  )
  
  df_long <- sim_design(within, between, 10, .5, 0, 1, TRUE, TRUE)
  df_wide <- sim_design(within, between, 10, .5, 0, 1, TRUE, FALSE)
                        
  long_names <- c("sub_id", "pets", "pets2", "time", "time2",  "val")
  wide_names <- c("sub_id", "pets", "pets2", "day_day", "night_day", "day_night", "night_night")
  
  expect_equal(names(df_long), long_names)
  expect_equal(names(df_wide), wide_names)
})

# other stuff ----
test_that("works", {
  
  between <- list(
    "B" = c("B1", "B2"),
    "A" = c("A2", "A1")
  )
  within <- list(
    "W" = c("W1", "W2"),
    "C" = c("C2", "C1")
  )
  
  mu = list(
    "B1_A2" = c(0, 10, 20, 30),
    "B1_A1" = c(40, 50, 60, 70),
    "B2_A1" = c(100, 110, 120, 130),
    "B2_A2" = c(140, 150, 160, 170)
  )
  sd = list(
    "B1_A2" = c(1, 1, 1, 1),
    "B1_A1" = 2,
    "B2_A1" = c(5, 10, 15, 20),
    "B2_A2" = c(30, 40, 50, 60)
  )
  
  triangle <- c(.1, .2, .3, .4, .5, .6)
  long_cor <- c(1, .1, .2, .3,
               .1,  1, .4, .5,
               .2, .4,  1, .6,
               .3, .5, .6,  1)
  mat <- matrix(long_cor, nrow = 4)
  
  cors = list(
    "B1_A2" = triangle,
    "B1_A1" = long_cor,
    "B2_A1" = mat,
    "B2_A2" = .4
  )
  
  n = 100
  empirical = TRUE

  df <- sim_design(within, between, n, cors, mu, sd, empirical)
  check_sim_stats(df, c("B", "A"))
  
  expect_equal(nrow(df), 400)
  expect_equal(ncol(df), 7)
  expect_equal(names(df), c("sub_id", "B", "A", "W1_C2", "W2_C2", "W1_C1", "W2_C1"))
})

# speed ----
test_that("speed tests", {
  library(tidyverse)
  within <- list(
    "A" = c("A1", "A2")
  )
  
  between <- list(
    "B" = c("B1", "B2")
  )
  
  mu <- list(
    "B1" = c(10, 10),
    "B2" = c(10, 10)
  )
  
  func <- function(i) {
    utils::setTxtProgressBar(pb, i)
    df <- sim_design(within, between, n = 20, mu = mu, frame_long = TRUE)
    suppressMessages(
      anova <- afex::aov_4(val~B*(A|sub_id), data = df, check_contrasts = TRUE)
    )
    anova$anova_table %>%
      tibble::as_tibble(rownames = "factor")
  }
  
  reps <- 2
  pb <- txtProgressBar(max = reps)
  timestamp()
  sims <- purrr::map_df(1:reps, func)
  timestamp()
  close(pb)
  
  alpha <- 0.05
  
  power <- sims %>%
    dplyr::group_by(factor) %>%
    dplyr::summarise(power = mean(`Pr(>F)` < alpha))
  
  sims %>%
    select(factor, p = `Pr(>F)`) %>%
    ggplot(aes(p, fill = factor)) +
    facet_grid(~factor) +
    geom_histogram(binwidth = alpha, color = "black", boundary = 0)
  
  
    
})

##------ Sun Apr 28 20:43:15 2019 ------##
#   df <- purrr::map(1:1e4, ~sim_design(within, between, n = 20))
##------ Sun Apr 28 20:44:48 2019 ------##

