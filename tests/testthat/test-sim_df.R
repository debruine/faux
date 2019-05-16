context("sim_df")

# error messages ----
test_that("error messages", {
  expect_error( sim_df("A"), ".data must be a data frame or matrix" )
  expect_error( sim_df(iris, "A"), "n must be an integer > 2" )
  expect_error( sim_df(iris, 2), "n must be an integer > 2" )
  expect_error( sim_df(iris, 10, FALSE), "between must be a numeric or character vector" )
})

# default parameters ----
test_that("default parameters", {
  newdf <- sim_df(iris)

  expect_equal(nrow(newdf), 100)
  expect_equal(ncol(newdf), 5)
  expect_equal(names(newdf)[2:5], names(iris)[1:4])
})

# specified parameters ----
test_that("specified parameters", {
  n <- 100
  dat <- tibble::as_tibble(iris) %>%
    dplyr::select_if(is.numeric)
  cors <- cor(dat)
  means <- dplyr::summarise_all(dat, mean) %>%
    as.data.frame()
  sds <- dplyr::summarise_all(dat, sd) %>%
    as.data.frame()
  
  # unnamed arguments in order
  newdf <- sim_df(iris, n, c(), TRUE)
  newdat <- dplyr::select_if(newdf, is.numeric)
  newcors <- cor(newdat)
  newmeans <- dplyr::summarise_all(newdat, mean) %>%
    as.data.frame()
  newsds <- dplyr::summarise_all(newdat, sd) %>%
    as.data.frame()
  
  expect_equal(nrow(newdf), n)
  expect_equal(ncol(newdf), 5)
  expect_equal(names(newdf)[2:5], names(iris)[1:4])
  
  expect_equal(cors, newcors)
  expect_equal(means, newmeans)
  expect_equal(sds, newsds)
  
  # named arguments out of order
  newdf <- sim_df(between = c(), empirical = TRUE, .data = iris, n = n)
  newdat <- dplyr::select_if(newdf, is.numeric)
  newcors <- cor(newdat)
  newmeans <- dplyr::summarise_all(newdat, mean) %>%
    as.data.frame()
  newsds <- dplyr::summarise_all(newdat, sd) %>%
    as.data.frame()
  
  expect_equal(nrow(newdf), n)
  expect_equal(ncol(newdf), 5)
  expect_equal(names(newdf)[2:5], names(iris)[1:4])
  
  expect_equal(cors, newcors)
  expect_equal(means, newmeans)
  expect_equal(sds, newsds)
  
})

# grouping by name ----
test_that("grouping by name", {
  newdf <- sim_df(iris, 20, "Species")
  
  expect_equal(nrow(newdf), 60)
  expect_equal(ncol(newdf), 6)
  expect_equal(names(newdf)[2:6] %>% sort(), names(iris) %>% sort())
})

# grouping by col number ----
test_that("grouping by col number", {
  newdf <- sim_df(iris, 20, 5)
  
  expect_equal(nrow(newdf), 60)
  expect_equal(ncol(newdf), 6)
  expect_equal(names(newdf)[2:6] %>% sort(), names(iris) %>% sort())
})

# test_that("mean stats are close over 1000 runs", {
#   skip_on_cran()
#   
#   simiris <- purrr::map_df(1:1000, function(i) {
#     iris %>%
#       sim_df(100) %>%
#       check_sim_stats(digits = 10)
#   })
#   
#   orig_stats <- iris %>%
#     check_sim_stats(digits = 10) %>%
#     dplyr::arrange(var) %>%
#     as.data.frame()
#   
#   sim_stats <- simiris %>% 
#     dplyr::group_by(var) %>%
#     dplyr::summarise_all(mean) %>%
#     dplyr::arrange(var) %>%
#     as.data.frame()
#   
#   expect_equal(orig_stats, sim_stats, tolerance = 0.02)
# })