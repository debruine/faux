faux_options(plot = FALSE)

# errors ----
test_that("errors", {
  expect_error(check_design(n = -1), "All n must be >= 0")
  expect_warning(check_design(n = 0), "Some cell Ns are 0. Make sure this is intentional.")
  expect_warning(check_design(n = 10.3), "Some cell Ns are not integers. They have been rounded up to the nearest integer.")
})

# n as vector ----
test_that("n as vector", {
  # unnamed vector with 2b2w design
  expect_silent(design <- check_design(within = 2, between = 2, n = c(10, 20)))
  expect_equal(design$n, list(B1a = 10, B1b = 20))
  
  # unnamed vector with 2b2b2w design
  n <- list(B1a_B2a = 10, B1a_B2b = 20, B1b_B2a = 30, B1b_B2b = 40)
  design <- check_design(within = 2, between = c(2, 2), n = c(10, 20, 30, 40))
  expect_equal(design$n, n)
  
  # unnamed vector with 2b2b2w2w design
  design <- check_design(within = c(2, 2), between = c(2, 2), n = c(10, 20, 30, 40))
  expect_equal(design$n, n)
  
  # named vector with 2b2w design
  design <- check_design(within = 2, between = 2, n = c(B1b = 10, B1a = 20))
  expect_equal(design$n, list(B1a = 20, B1b = 10))
})


# params ----
test_that("params", {
  # numeric n
  expect_silent(check_design(between = 2, n = list("B1a" = 10, "B1b" = 20)))
  expect_silent(check_design(between = 2, n = list("B1a" = 10, "B1b" = "20")))
  expect_error(
    check_design(between = 2, n = list("B1a" = 10, "B1b" = "B")),
    "All n must be numbers"
  )
  
  # numeric mu
  expect_silent(check_design(between = 2, mu = list("B1a" = 10, "B1b" = 20)))
  expect_silent(check_design(between = 2, mu = list("B1a" = 10, "B1b" = "20")))
  expect_error(
    check_design(between = 2, mu = list("B1a" = 10, "B1b" = "B")),
    "All mu must be numbers"
  )
  
  # numeric sd
  expect_silent(check_design(between = 2, sd = list("B1a" = 10, "B1b" = 20)))
  expect_silent(check_design(between = 2, sd = list("B1a" = 10, "B1b" = "20")))
  expect_error(
    check_design(between = 2, sd = list("B1a" = 10, "B1b" = "B")),
    "All sd must be numbers", fixed = TRUE
  )
  
  expect_error(check_design(sd = -1), "All sd must be >= 0", fixed = TRUE)
  
  err <- "You have duplicate levels for factor(s): A"
  expect_error(check_design(list(A = c("A1", "A1"))), err, fixed = TRUE)
  
  err <- "You have duplicate levels for factor(s): A, B"
  expect_error(check_design(list(A = c("A1", "A1"), B = c("B1", "B1"))), err, fixed = TRUE)
  
  err <- "You have multiple factors with the same name (A). Please give all factors unique names."
  expect_error(check_design(list(A = c("A1", "A2"), A = c("B1", "B2"))), err, fixed = TRUE)
  expect_error(check_design(list(A = c("A1", "A2")), 
                            list(A = c("B1", "B2"))), 
               err, fixed = TRUE)
})

# no factors ----
test_that("no factors", {
  design <- check_design()
  expect_equal(design$within, list())
  expect_equal(design$between, list())
  expect_equal(design$dv, list(y = "value"))
})

# 2w ----
test_that("2w", {
  within <- list(time = c("night", "day"))
  between <- list()
  design <- check_design(within, between, n = 10)
  
  cell_n <- list(y = 10)
  cell_mu <- list(y = list(night = 0, day = 0))
  cell_sd <- list(y = list(night = 1, day = 1))
  
  expect_equal(design$within, list(time = list(night = "night", day = "day")))
  expect_equal(design$between, list())
  
  expect_equal(design$n, cell_n)
  expect_equal(design$mu, cell_mu)
  expect_equal(design$sd, cell_sd)
  expect_equal(design$dv, list(y = "value"))
  expect_equal(design$id, list(id = "id"))
  
  expect_true("design" %in% class(design))
})

# 2b ----
test_that("2b", {
  within <- list()
  between <- list(time = c("night", "day"))
  design <- check_design(within, between, n = 10)
  
  cell_n <- list(night = 10, day = 10)
  cell_mu <- list(night = list(y=0), day = list(y=0))
  cell_sd <- list(night = list(y=1), day = list(y=1))
  
  expect_equal(design$within, list())
  expect_equal(design$between, list(time = list(night = "night", day = "day")))
  
  expect_equal(design$n, cell_n)
  expect_equal(design$mu, cell_mu)
  expect_equal(design$sd, cell_sd)
  expect_equal(design$dv, list(y = "value"))
  expect_equal(design$id, list(id = "id"))
})

# 2w*2b ----
test_that("2w*2b", {
  within  <- list(time = c("night", "day"))
  between <- list(pet = c("dog", "cat"))
  design  <- check_design(within, between, n = 10)

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
  expect_equal(design$dv, list(y = "value"))
  expect_equal(design$id, list(id = "id"))
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
    
  design <- check_design(within, between)
  
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
  expect_equal(design$dv, list(y = "value"))
  expect_equal(design$id, list(id = "id"))
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
  id <- list(sub_id = "id")
  
  design <- check_design(within, between, n, mu, sd, r, dv, id)
  
  design_elements <- c("within", "between", "dv", "id", "vardesc", "n", "mu", "sd", "r", "sep", "params")
  
  expect_equal(names(design), design_elements)
  expect_equal(design$dv, dv)
  expect_equal(design$id, id)
})

# interactions ----
test_that("interactions", {
  faux_options(sep = "_")
  n <- list(
    B1a_B2a = 10, 
    B1a_B2b = 20, 
    B1b_B2a = 30, 
    B1b_B2b = 40
  )

  design <- check_design(2, c(2,2), n = n, plot = FALSE)
  
  expect_equal(design$n, n)
})

# anon factors ----
test_that("anon factors", {
  design <- check_design(c(2, 4), c(2, 2))
  
  w <- list(
    W1 = list(W1a="W1a", W1b="W1b"),
    W2 = list(W2a="W2a", W2b="W2b", W2c="W2c", W2d="W2d")
  )
  
  b <- list(
    B1 = list(B1a="B1a",B1b="B1b"),
    B2 = list(B2a="B2a", B2b="B2b")
  )
  
  expect_equal(design$within, w)
  expect_equal(design$between, b)
})

# wierd factor names ----
test_that("wierd factor names", {
  # only replaces underscores
  within <- list("A" = c("A_1", "A 2"),
                 "B" = c("B~1", "B'2"))
  expect_error(check_design(within))
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

# params table ----
test_that("params table", {
  des <- check_design()
  params <- data.frame(y = "value", n = 100, mu = 0, sd = 1)
  expect_equal(des$params, params)
  
  within <- list(
    time = c("morning" = "am", "night" = "pm"),
    condition = c("A" = "cond 1", "B" = "cond 2", "C" = "cond 3")
  )
  between <- list(
    pet = c("dog" = "Dogs", "cat" = "Cats"),
    x = c("X1" = "First", "X2" = "Second"))
  
  n <- list(
    dog_X1 = 100,
    dog_X2 = 200,
    cat_X1 = 300,
    cat_X2 = 400
  )
  
  r <- list(
    dog_X1 = seq(.1, by = .025, length.out = 15),
    dog_X2 = seq(.2, by = .025, length.out = 15),
    cat_X1 = seq(.3, by = .025, length.out = 15),
    cat_X2 = seq(.4, by = .025, length.out = 15)
  )
  
  des <- check_design(within, between, n = n, mu = 1:24, 
               sd = 1:24, r = r, id = c(id = "ID"))
  
  nm <- c("pet", "x", "time", "condition", "morning_A", 
          "morning_B", "morning_C", "night_A", "night_B",
          "night_C", "n", "mu", "sd")
  
  expect_true(des$params %>% nrow() == 24)
  expect_true(all(des$params %>% names() == nm))
  
  expected <- c(
    "* [DV] y: value  ",
    "* [ID] id: ID  ",
    "* Within-subject variables:",
    "    * time: ",
    "        * morning: am",
    "        * night: pm",
    "    * condition: ",
    "        * A: cond 1",
    "        * B: cond 2",
    "        * C: cond 3",
    "* Between-subject variables:",
    "    * pet: ",
    "        * dog: Dogs",
    "        * cat: Cats",
    "    * x: ",
    "        * X1: First",
    "        * X2: Second"
  )
  
  op <- capture.output(des)
  expect_equal(op[1:length(expected)], expected)
})

# sep ----
test_that("sep", {
  faux_options(sep = ".")
  design <- check_design(
    within = list(
      A = c("A_1", "A_2"),
      B = c("B_1", "B_2")
    ),
    n = 5,
    plot = FALSE
  )
  
  wide <- sim_data(design = design)
  expect_equal(names(wide), c("id", "A_1.B_1", "A_1.B_2", "A_2.B_1", "A_2.B_2"))
  
  long <- sim_data(design = design, long = TRUE)
  expect_equal(unique(long$A), factor(c("A_1", "A_2")))
  expect_equal(unique(long$B), factor(c("B_1", "B_2")))
  faux_options(sep = "_")
})

# vardesc ----
test_that("vardesc", {
  between <- list(
    B = c(B1 = "Level 1B", B2 = "Level 2B")
  )
  within <- list(
    W = c(W1 = "Level 1W", W2 = "Level 2W")
  )
  
  # includes vardesc
  vardesc <- list(B = "Between-Subject Factor",
                  W = "Within-Subject Factor")
  expect_silent(design <- check_design(within, between, vardesc = vardesc))
  expect_mapequal(design$vardesc, vardesc)
  
  op <- capture.output(design)
  expect_equal(op[4], "    * W: Within-Subject Factor: ")
  expect_equal(op[8], "    * B: Between-Subject Factor: ")
  
  # no vardesc
  design <- check_design(within, between)
  expect_mapequal(design$vardesc, list(W = "W", B = "B"))
  # no repeats on identical factor name and label
  op <- capture.output(design)
  expect_equal(op[4], "    * W: ")
  expect_equal(op[8], "    * B: ")
  
  # warns on missing value and replaces with unlabelled
  vardesc_missing <- list(B = "Between-Subject Factor")
  expect_warning(design <- check_design(within, between, vardesc = vardesc_missing))
  expect_equal(design$vardesc$W, "W")
  
  # converts vectors to list
  vardesc_vec <- c(B = "Between-Subject Factor",
                   W = "Within-Subject Factor")
  expect_silent(design <- check_design(within, between, vardesc = vardesc_vec))
  expect_mapequal(design$vardesc, vardesc)
})

# get_design ----
test_that("get_design", {
  data <- sim_design(2, 2)
  design <- get_design(data)
  expect_equal(design, attributes(data)$design)
  expect_equal(design$id, list(id = "id"))
})

# set_design ----
test_that("set_design", {
  design <- check_design()
  data <- data.frame(id = 1:100, y = rnorm(100))
  data_design <- set_design(data, design)
  
  expect_equal(design, get_design(data_design))
  expect_equal(class(data_design), c("faux", "data.frame"))
})

faux_options(plot = TRUE)
faux_options(sep = "_")