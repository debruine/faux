context("test-faux2anova_design")

# errors ----
test_that("errors", {
  design <- check_design(plot = FALSE)
  expect_error(faux2ANOVA_design(design), 
               "You need at least one factor to use Superpower")
  
  design <- check_design(c(2,2,2,2), plot = FALSE)
  expect_error(faux2ANOVA_design(design), 
               "You can't use Superpower with more than 3 factors")
})

test_that("?",  {
  mu <- c(1.1, 1.2, 1.3, 1.4)
  r <- c(0.91, 0.92, 0.93, 
               0.94, 0.95, 
                     0.96)
  within <- list(COLOR = c("red", "blue"),
                 SPEED = c("fast","slow"))
  design <- check_design(within, 
                         n = 80, mu = mu, sd = 1, r = r)
  design_av <- faux2ANOVA_design(design)
  
  design_sp <- Superpower::ANOVA_design(
    design = "2w*2w",
    n = 80,
    mu = c(1.1, 1.2, 1.3, 1.4),
    sd = 1,
    r = c(0.91, 0.92, 0.93, 0.94, 0.95, 0.96),
    labelnames = c("COLOR", "red", "blue",
                   "SPEED", "fast", "slow"),
    plot = TRUE
  )
  
  names(design_sp)
  design$within
})


# 2w ----
test_that("2w", {
  design <- check_design(2, list(), plot = FALSE)
  ap <- faux2ANOVA_design(design, plot = FALSE)
  
  expect_equal(ap$design, "2w", check.environment=FALSE)
  #expect_equal(ap$design_list, c("A1", "A2"), check.environment=FALSE)
  #expect_equal(ap$factors, 1, check.environment=FALSE)
  expect_equal(ap$mu, c(0,0), check.environment=FALSE)
  expect_equal(ap$sd, c(1,1), check.environment=FALSE)
  expect_equal(ap$r, 0, check.environment=FALSE)
  expect_equal(ap$n, 100, check.environment=FALSE)
  #expect_equal(ap$design_factors, 1, check.environment=FALSE)
  expect_equal(ap$labelnames, c("A", "A1", "A2"), check.environment=FALSE)
  expect_equal(ap$plot, FALSE, check.environment=FALSE)
})

# 2b ----
test_that("2b", {
  design <- check_design(list(), 2, plot = FALSE, check.environment=FALSE)
  ap <- faux2ANOVA_design(design, plot = FALSE, check.environment=FALSE)
  
  expect_equal(ap$design, "2b", check.environment=FALSE)
  #expect_equal(ap$design_list, c("A1", "A2"), check.environment=FALSE)
  #expect_equal(ap$factors, 1, check.environment=FALSE)
  expect_equal(ap$mu, c(0,0), check.environment=FALSE)
  expect_equal(ap$sd, c(1,1), check.environment=FALSE)
  expect_equal(ap$r, 0, check.environment=FALSE)
  expect_equal(ap$n, 100, check.environment=FALSE)
  #expect_equal(ap$design_factors, 0, check.environment=FALSE)
  expect_equal(ap$labelnames, c("A", "A1", "A2"), check.environment=FALSE)
  expect_equal(ap$plot, FALSE, check.environment=FALSE)
})

# 2w*2w*2b ----
test_that("2w*2w*2b", {
  within <- list(
    time = c(night = "night time", day = "day time"), 
    condition = c(A = "condition A", B = "condition B")
  )
  between <- list(
    pet = c(dog = "has dogs", cat = "has cats")
  )
  n <- 100
  mu <- 1:8
  sd <- 1:8
  r <- list(
    dog = seq(0, .25, .05),
    cat = seq(.3, .55, .05)
  )
  des <- check_design(within, between, n, mu, sd, r, plot = FALSE)
  ap <- faux2ANOVA_design(des, plot = FALSE)
  
  expect_equal(ap$design, "2b*2w*2w")
  # expect_equal(ap$design_list, c(
  #   "dog_night_A", "dog_night_B", "dog_day_A", "dog_day_B", 
  #   "cat_night_A", "cat_night_B", "cat_day_A", "cat_day_B"),
  #    check.environment=FALSE)
  # expect_equal(ap$factors, 3, check.environment=FALSE)
  expect_equal(ap$mu, 1:8, check.environment=FALSE)
  expect_equal(ap$sd, 1:8, check.environment=FALSE)
  expect_equal(ap$r, c(0, 0.05, 0.15, 0.10, 0.20, 0.25, 
                       0, 0, 0, 0, 0, 0, 0, 0, 
                       0.30, 0, 0, 0, 0, 0.35, 0.45, 
                       0, 0, 0, 0, 0.40, 0.50, 0.55), 
               check.environment=FALSE)
  expect_equal(ap$n, 100, check.environment=FALSE)
  #expect_equal(ap$design_factors, c(0,1,1), check.environment=FALSE)
  expect_equal(ap$labelnames, c("pet", "dog", "cat",
                                "time", "night", "day", 
                                "condition", "A", "B"), 
               check.environment=FALSE)
  expect_equal(ap$plot, FALSE, check.environment=FALSE)
})
