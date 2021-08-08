test_that("x", {
  data <- expand.grid(
    rater = 1:3,
    stimulus = 1:2,
    obs = 1:4
  )
  
  datar <- data %>%
    add_ranef("rater", r_i = 1) %>%
    add_ranef("stimulus", s_i = 10) %>%
    add_ranef(c("rater", "stimulus"), rs_i = 100)
  
  r <- datar$r_i
  s <- datar$s_i
  rs <- datar$rs_i
  
  expect_equal(rep(r[1:3], 8), r)
  expect_equal(rep(s[c(1, 4)], each = 3, times = 4), s)
  expect_equal(rep(rs[1:6], 4), rs)
})

test_that("x and y", {
  set.seed(1)
  
  nrater <- 5000
  x_sd <- sample(1:10, 1)
  y_sd <- sample(1:10, 1)
  r_xy <- 0.5
  
  data <- expand.grid(
    rater = 1:nrater,
    stimulus = 1:2
  )
  
  datar <- add_ranef(data, "rater", x = x_sd, y = y_sd, .cors = r_xy)
  x <- datar$x[1:nrater]
  y <- datar$y[1:nrater]
  
  expect_equal(x, datar$x[(nrater+1):(2*nrater)])
  
  expect_true(mean(x) %>% abs() < .1)
  expect_true(mean(y) %>% abs() < .1)
  expect_equal(sd(x), x_sd, tol = 0.05)
  expect_equal(sd(y), y_sd, tol = 0.05)
  expect_equal(cor(x, y), r_xy, tol = .05)
})


test_that("add_random", {
  # start a data frame
  data1 <- add_random(school = 3)
  expect_equal(data1$school, paste0("s", 1:3))
  # nest classes in schools (2 classes per school)
  data2 <- add_random(data1, class = 2, .nested_in = "school")
  expect_equal(data2$class, paste0("c", 1:6))
  expect_equal(data2$school, rep(data1$school, each = 2))
  # nest students in each class (different n per class)
  n <- c(20, 24, 23, 21, 25, 24)
  data3 <- add_random(data2, student = n, .nested_in = "class")
  expect_equal(nrow(data3), sum(n))
  # cross each student with 10 questions
  data4 <- add_random(data3, question = 10)
  expect_equal(nrow(data4), sum(n)*10)
  expect_equal(data4$student, rep(data3$student, each = 10))
  
  # add crossed random factor to existing dataset
  data5 <- sim_design(within = 2, n = 3, long = TRUE, plot = FALSE)
  data6 <- add_random(data5, Q = 2)
  expect_equal(data6$Q, rep(c("Q1", "Q2"), 6))
  
  # compare nesting in 2 different factors
  data <- add_random(A = 2., B = 2)
  nested_in_A <- add_random(data, C = 2, .nested_in = "A")
  nested_in_B <- add_random(data, C = 2, .nested_in = "B")
  expect_false(all(nested_in_A$C == nested_in_B$C))
})
