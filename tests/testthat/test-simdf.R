context("simdf")

test_that("error messages", {
  expect_error( simdf("A"), "dat must be a data frame or matrix" )
  expect_error( simdf(iris, "A"), "n must be an integer > 2" )
  expect_error( simdf(iris, 2), "n must be an integer > 2" )
  expect_error( simdf(iris, 10, FALSE), "grp_by must be a numeric or character vector" )
})

test_that("correct default parameters", {
  newdf <- simdf(iris)

  expect_equal(nrow(newdf), 100)
  expect_equal(ncol(newdf), 4)
  expect_equal(names(newdf), names(iris)[1:4])
})

test_that("correct specified parameters", {
  n <- 100
  dat <- tibble::as_tibble(iris) %>%
    dplyr::select_if(is.numeric)
  cors <- cor(dat)
  means <- dplyr::summarise_all(dat, mean)
  sds <- dplyr::summarise_all(dat, sd) %>%
    as.data.frame()
  
  newdf <- simdf(iris, n, NULL, TRUE)
  newdat <- dplyr::select_if(newdf, is.numeric)
  newcors <- cor(newdat)
  newmeans <- dplyr::summarise_all(newdat, mean)
  newsds <- dplyr::summarise_all(newdat, sd) %>%
    as.data.frame()
  
  expect_equal(nrow(newdf), n)
  expect_equal(ncol(newdf), 4)
  expect_equal(names(newdf), names(iris)[1:4])
  
  expect_equal(cors, newcors)
  expect_equal(means, newmeans)
  expect_equal(sds, newsds)
})

test_that("grouping by col name", {
  newdf <- simdf(iris, 20, "Species")
  
  expect_equal(nrow(newdf), 60)
  expect_equal(ncol(newdf), 5)
  expect_equal(names(newdf) %>% sort(), names(iris) %>% sort())
})

test_that("grouping by col number", {
  newdf <- simdf(iris, 20, 5)
  
  expect_equal(nrow(newdf), 60)
  expect_equal(ncol(newdf), 5)
  expect_equal(names(newdf) %>% sort(), names(iris) %>% sort())
})