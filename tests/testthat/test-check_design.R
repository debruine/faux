context("test-check_design")

# errors ----
test_that("errors", {
  expect_error(check_design(n = -1, plot = 0), "All n must be >= 0")
  expect_warning(check_design(n = 0, plot = 0), "Some cell Ns are 0. Make sure this is intentional.")
  expect_warning(check_design(n = 10.3, plot = 0), "Some cell Ns are not integers. They have been rounded up to the nearest integer.")
  
  # numeric n
  expect_silent(check_design(between = 2, n = list("A1" = 10, "A2" = 20), plot = 0))
  expect_silent(check_design(between = 2, n = list("A1" = 10, "A2" = "20"), plot = 0))
  expect_error(
    check_design(between = 2, n = list("A1" = 10, "A2" = "B"), plot = 0),
    "All n must be numbers"
  )
  
  # numeric mu
  expect_silent(check_design(between = 2, mu = list("A1" = 10, "A2" = 20), plot = 0))
  expect_silent(check_design(between = 2, mu = list("A1" = 10, "A2" = "20"), plot = 0))
  expect_error(
    check_design(between = 2, mu = list("A1" = 10, "A2" = "B"), plot = 0),
    "All mu must be numbers"
  )
  
  # numeric sd
  expect_silent(check_design(between = 2, sd = list("A1" = 10, "A2" = 20), plot = 0))
  expect_silent(check_design(between = 2, sd = list("A1" = 10, "A2" = "20"), plot = 0))
  expect_error(
    check_design(between = 2, sd = list("A1" = 10, "A2" = "B"), plot = 0),
    "All sd must be numbers"
  )
  
  expect_error(check_design(sd = -1, plot = 0), "All sd must be >= 0")
})

# no factors
test_that("no factors", {
  design <- check_design(plot = FALSE)
  expect_equal(design$within, list())
  expect_equal(design$between, list())
  expect_equal(design$dv, list(y = "Score"))
})

# 2w ----
test_that("2w", {
  within <- list(time = c("night", "day"))
  between <- list()
  design <- check_design(within, between, n = 10, plot = FALSE)
  
  cell_n <- list(y = 10)
  cell_mu <- list(y = list(night = 0, day = 0))
  cell_sd <- list(y = list(night = 1, day = 1))
  
  expect_equal(design$within, list(time = list(night = "night", day = "day")))
  expect_equal(design$between, list())
  
  expect_equal(design$n, cell_n)
  expect_equal(design$mu, cell_mu)
  expect_equal(design$sd, cell_sd)
  expect_equal(design$dv, list(y = "Score"))
  expect_equal(design$id, list(id = "Subject ID"))
  
  expect_true("design" %in% class(design))
})

# 2b ----
test_that("2b", {
  within <- list()
  between <- list(time = c("night", "day"))
  design <- check_design(within, between, n = 10, plot = FALSE)
  
  cell_n <- list(night = 10, day = 10)
  cell_mu <- list(night = list(y=0), day = list(y=0))
  cell_sd <- list(night = list(y=1), day = list(y=1))
  
  expect_equal(design$within, list())
  expect_equal(design$between, list(time = list(night = "night", day = "day")))
  
  expect_equal(design$n, cell_n)
  expect_equal(design$mu, cell_mu)
  expect_equal(design$sd, cell_sd)
  expect_equal(design$dv, list(y = "Score"))
  expect_equal(design$id, list(id = "Subject ID"))
})

# 2w*2b ----
test_that("2w*2b", {
  within  <- list(time = c("night", "day"))
  between <- list(pet = c("dog", "cat"))
  design  <- check_design(within, between, n = 10, plot = FALSE)

  cell_n  <- list(dog = 10, cat = 10)
  cell_mu <- list(dog = list(night = 0, day = 0),
                  cat = list(night = 0, day = 0))
  cell_sd <- list(dog = list(night = 1, day = 1),
                  cat = list(night = 1, day = 1))
  
  expect_equal(design$within, list(time = list(night = "night", day = "day")))
  expect_equal(design$between, list(pet = list(dog = "dog", cat = "cat")))
  
  expect_equal(design$n, cell_n)
  expect_equal(design$mu, cell_mu)
  expect_equal(design$sd, cell_sd)
  expect_equal(design$dv, list(y = "Score"))
  expect_equal(design$id, list(id = "Subject ID"))
})

# 2w*2w*2b*2b ----
test_that("2w*2w*2b*2b", {
  within <- list(
    time = c(night = "night time", day = "day time"), 
    condition = c(A = "condition A", B = "condition B")
  )
  between <- list(
    pet = c(dog = "has dogs", cat = "has cats"), 
    age = c(old = "older", young = "younger")
  )
    
  design <- check_design(within, between, plot = FALSE)
  
  cells_w <- c("night_A", "night_B", "day_A", "day_B")
  cells_b <- c("dog_old", "dog_young", "cat_old", "cat_young")
  cell_n <- list(dog_old = 100, dog_young = 100, cat_old = 100, cat_young = 100)
  mu_list <- list(night_A = 0, night_B = 0, day_A = 0, day_B = 0)
  cell_mu <- list(
    dog_old = mu_list,
    dog_young = mu_list,
    cat_old = mu_list,
    cat_young = mu_list
  )
  sd_list <- list(night_A = 1, night_B = 1, day_A = 1, day_B = 1)
  cell_sd <- list(
    dog_old = sd_list,
    dog_young = sd_list,
    cat_old = sd_list,
    cat_young = sd_list
  )
  
  expect_equal(design$n, cell_n)
  expect_equal(design$mu, cell_mu)
  expect_equal(design$sd, cell_sd)
  expect_equal(design$dv, list(y = "Score"))
  expect_equal(design$id, list(id = "Subject ID"))
})

# design spec ----
test_that("design spec", {
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
  dv <- list(dv = "DV")
  id <- list(sub_id = "subject ID")
  
  design <- check_design(within, between, n, mu, sd, r, dv, id, plot = FALSE)
  
  design_elements <- c("within", "between", "dv", "id", "n", "mu", "sd", "r")
  
  expect_equal(names(design), design_elements)
  expect_equal(design$dv, dv)
  expect_equal(design$id, id)
})

# anon factors ----
test_that("anon factors", {
  design <- check_design(c(2, 4), c(2, 2), plot = FALSE)
  
  w <- list(
    A = list(A1="A1", A2="A2"),
    B = list(B1="B1", B2="B2", B3="B3", B4="B4")
  )
  
  b <- list(
    C = list(C1="C1",C2="C2"),
    D = list(D1="D1", D2="D2")
  )
  
  expect_equal(design$within, w)
  expect_equal(design$between, b)
})

# make_id ----
test_that("make_id", {
  expect_equal(make_id(10), c("S01", "S02", "S03", "S04", "S05", 
                              "S06", "S07", "S08", "S09", "S10"))
  
  expect_equal(make_id(10, "SUB"), c("SUB01", "SUB02", "SUB03", "SUB04", "SUB05", 
                                     "SUB06", "SUB07", "SUB08", "SUB09", "SUB10"))
  
  expect_equal(make_id(100)[[1]], "S001")
  expect_equal(make_id(1000)[[1]], "S0001")
  expect_equal(make_id(1000, "pokemon_")[[1]], "pokemon_0001")
  expect_equal(make_id(100, digits = 4)[[1]], "S0001")
  
  # named arguments
  expect_equal(make_id(n = 100, prefix = "A", digits = 4)[[1]], "A0001")
  expect_equal(make_id(digits = 4, prefix = "A", n = 100)[[1]], "A0001")
  
  # vector
  expect_equal(make_id(2:4), c("S2", "S3", "S4"))
  expect_equal(make_id(100:200)[[1]], "S100")
})
