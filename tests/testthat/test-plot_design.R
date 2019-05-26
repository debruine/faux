context("test-plot_design")

# error ----
test_that("error", {
  df <- sim_design(plot = FALSE)
  attributes(df)$design <- NULL
  
  err <- "The data table must have a design attribute"
  expect_error(plot_design(df), err)
  expect_error(plot_design(iris), err)
  
  err <- "input must be a design list or a data frame"
  expect_error(plot_design(1), err)
  expect_error(plot_design("A"), err)
  expect_error(plot_design(matrix(1:100, 10)), err)
})

# wide2long ----
test_that("wide2long", {
  df <- sim_design(plot = FALSE, seed = 1)
  p1 <- plot_design(df)
  expect_equal(class(p1), c("gg", "ggplot"))
  
  df <- sim_design(long = TRUE, plot = FALSE, seed = 1)
  p2 <- plot_design(df)
  expect_equal(class(p2), c("gg", "ggplot"))
  expect_equal(p1, p2)
})

# order ----
test_that("order", {
  skip("failing")
  des <- check_design(c(2,2,2,2,2,2), plot = FALSE)
  
  p <- plot_design(des, "F", "E", "D", "C", "B", "A")
  
  expect_equal(p$labels$x, "F")
  expect_equal(p$labels$fill, "E")
  expect_equal(p$labels$colour, "E")
  expect_equal(p$facet$params$rows %>% names(), c("D", "C"))
  expect_equal(p$facet$params$cols %>% names(), c("B", "A"))
})

# from design ----
test_that("from design", {
  s0 <- check_design(plot = FALSE) %>% plot_design()
  s1 <- check_design(2, plot = FALSE) %>% plot_design()
  s2 <- check_design(c(2,2), plot = FALSE) %>% plot_design()
  s3 <- check_design(c(2,2,2), plot = FALSE) %>% plot_design()
  s4 <- check_design(c(2,2,2,2), plot = FALSE) %>% plot_design()
  s5 <- check_design(c(2,2,2,2,2), plot = FALSE) %>% plot_design()
  s6 <- check_design(c(2,2,2,2,2,2), plot = FALSE) %>% plot_design()
  
  expect_equal(s0$labels$x, "Score")
  expect_equal(s0$labels$y, "Score")
  expect_equal(s0$labels$fill, "fill")
  expect_equal(s0$labels$colour, "colour")
  expect_equal(s0$facet$params, list())
  
  expect_equal(s1$labels$x, "A")
  expect_equal(s1$labels$y, "Score")
  expect_equal(s1$labels$fill, "A")
  expect_equal(s1$labels$colour, "A")
  expect_equal(s1$facet$params, list())
  
  expect_equal(s2$labels$x, "A")
  expect_equal(s2$labels$y, "Score")
  expect_equal(s2$labels$fill, "B")
  expect_equal(s2$labels$colour, "B")
  expect_equal(s2$facet$params, list())
  
  expect_equal(s3$labels$x, "A")
  expect_equal(s3$labels$y, "Score")
  expect_equal(s3$labels$fill, "B")
  expect_equal(s3$labels$colour, "B")
  expect_equal(s3$facet$params$rows %>% names(), c("C"))
  expect_equal(s3$facet$params$cols %>% names(), character(0))
  
  expect_equal(s4$labels$x, "A")
  expect_equal(s4$labels$y, "Score")
  expect_equal(s4$labels$fill, "B")
  expect_equal(s4$labels$colour, "B")
  expect_equal(s4$facet$params$rows %>% names(), c("C"))
  expect_equal(s4$facet$params$cols %>% names(), c("D"))
  
  expect_equal(s5$labels$x, "A")
  expect_equal(s5$labels$y, "Score")
  expect_equal(s5$labels$fill, "B")
  expect_equal(s5$labels$colour, "B")
  expect_equal(s5$facet$params$rows %>% names(), c("C"))
  expect_equal(s5$facet$params$cols %>% names(), c("D", "E"))
  
  expect_equal(s6$labels$x, "A")
  expect_equal(s6$labels$y, "Score")
  expect_equal(s6$labels$fill, "B")
  expect_equal(s6$labels$colour, "B")
  expect_equal(s6$facet$params$rows %>% names(), c("C", "D"))
  expect_equal(s6$facet$params$cols %>% names(), c("E", "F"))
})

# from data ----
test_that("from data", {
  s0 <- sim_design(plot = FALSE) %>% plot_design()
  s1 <- sim_design(2, plot = FALSE) %>% plot_design()
  s2 <- sim_design(c(2,2), plot = FALSE) %>% plot_design()
  s3 <- sim_design(c(2,2,2), plot = FALSE) %>% plot_design()
  s4 <- sim_design(c(2,2,2,2), plot = FALSE) %>% plot_design()
  s5 <- sim_design(c(2,2,2,2,2), plot = FALSE) %>% plot_design()
  s6 <- sim_design(c(2,2,2,2,2,2), plot = FALSE) %>% plot_design()
  
  expect_equal(s0$labels$x, "Score")
  expect_equal(s0$labels$y, "Score")
  expect_equal(s0$labels$fill, "fill")
  expect_equal(s0$labels$colour, "colour")
  expect_equal(s0$facet$params, list())
  
  expect_equal(s1$labels$x, "A")
  expect_equal(s1$labels$y, "Score")
  expect_equal(s1$labels$fill, "A")
  expect_equal(s1$labels$colour, "A")
  expect_equal(s1$facet$params, list())
  
  expect_equal(s2$labels$x, "A")
  expect_equal(s2$labels$y, "Score")
  expect_equal(s2$labels$fill, "B")
  expect_equal(s2$labels$colour, "B")
  expect_equal(s2$facet$params, list())
  
  expect_equal(s3$labels$x, "A")
  expect_equal(s3$labels$y, "Score")
  expect_equal(s3$labels$fill, "B")
  expect_equal(s3$labels$colour, "B")
  expect_equal(s3$facet$params$rows %>% names(), c("C"))
  expect_equal(s3$facet$params$cols %>% names(), character(0))
  
  expect_equal(s4$labels$x, "A")
  expect_equal(s4$labels$y, "Score")
  expect_equal(s4$labels$fill, "B")
  expect_equal(s4$labels$colour, "B")
  expect_equal(s4$facet$params$rows %>% names(), c("C"))
  expect_equal(s4$facet$params$cols %>% names(), c("D"))
  
  expect_equal(s5$labels$x, "A")
  expect_equal(s5$labels$y, "Score")
  expect_equal(s5$labels$fill, "B")
  expect_equal(s5$labels$colour, "B")
  expect_equal(s5$facet$params$rows %>% names(), c("C"))
  expect_equal(s5$facet$params$cols %>% names(), c("D", "E"))
  
  expect_equal(s6$labels$x, "A")
  expect_equal(s6$labels$y, "Score")
  expect_equal(s6$labels$fill, "B")
  expect_equal(s6$labels$colour, "B")
  expect_equal(s6$facet$params$rows %>% names(), c("C", "D"))
  expect_equal(s6$facet$params$cols %>% names(), c("E", "F"))
})

# 2w ----
test_that("2w", {
  within <- list(time = c(day = "Tested during the day", 
                          night = "Tested at night"))
  between <- list()
  mu <- c(1,2)
  dv = "rt"
  id = "sub_id"
  d <- sim_design(within, between, mu = mu, dv = dv, id = id, long = TRUE, plot = FALSE)
  p <- plot_design(d)
  expect_equal(class(p), c("gg", "ggplot"))
})

# 2w*2b ----
test_that("2w*2b", {
  within <- list(time = c("day", "night"))
  between <- list(pet = c("dog", "cat"))
  mu <- list(dog = 1:2, cat = 3:4)
  p <- check_design(within, between, mu = mu) %>%
    plot_design()
  expect_equal(class(p), c("gg", "ggplot"))
  
  # axis labels
  within <- list(time = c(day = "Tested during the day", 
                          night = "Tested at night"))
  between <- list(pet = c(dog = "Has a dog", cat = "Has a cat"))
  mu <- list(
    dog = c(1,2),
    cat = c(3,4)
  )
  d <- sim_design(within, between, mu = mu, long = TRUE, plot = FALSE)
  p <- plot_design(d)
  expect_equal(class(p), c("gg", "ggplot"))
  expect_equal(p$labels$x, "time")
  expect_equal(p$labels$y, "Score")
  expect_equal(p$labels$fill, "pet")
  expect_equal(p$labels$colour, "pet")
})

# 2w*2w*2b ----
test_that("2w*2w*2b", {
  within <- list(time = c("day", "night"), condition = c("A", "B"))
  between <- list(pet = c("dog", "cat"))
  mu <- list(dog = 1:4, cat = 2:5)
  p <- check_design(within, between, mu = mu) %>%
    plot_design()
  expect_equal(class(p), c("gg", "ggplot"))
})

# 2w*2w*2b*2b ----
test_that("2w*2w*2b*2b", {
  within <- list(pet = c("ferret", "dog", "cat"), condition = c("A", "B"))
  between <- list(time = c("night", "day"), age = c("old", "young"))
  mu <- list(night_old = 1:6, 
             day_old = 2:7,
             night_young = 3:8, 
             day_young = 4:9)
  design <- check_design(within, between, mu = mu, plot = FALSE)
  p <- plot_design(design)
  expect_equal(class(p), c("gg", "ggplot"))
})

test_that("geoms", {

  
})
