context("test-interactive_design")

# set up canonial design
within <- list(A = c("A1", "A2", "A3"))
between <- list(B = c("B1", "B2"))
n <- 10
mu <- list(
  B1 = 1:3,
  B2 = 4:6
)
sd <- 1
r <- list(
  B1 = .5,
  B2 = c(.4, .5, .6)
)
des2 <- check_design(within, between, n, mu, sd, r, plot = TRUE)

test_that("default", {
  # set up interactive answers
  f <- file()
  options(faux.connection = f)
  lines <- c(
    "1", "A", "3", "A1", "A2", "A3", "1", "B", "2", "B1", "B2", 
    "y", "id", "10", "1,2,3", "4,5,6", "1", "1", ".5", ".4, .5, .6"
  )
  ans <- paste(lines, collapse = "\n")
  write(ans, f)
  
  capture_output_lines({des <- interactive_design()})
  
  options(faux.connection = stdin()) # reset connection
  close(f) # close the file
  
  # check match
  expect_equal(des$within,  des2$within)
  expect_equal(des$between, des2$between)
  expect_equal(des$id, des2$id)
  expect_equal(des$dv, des2$dv)
  expect_equal(des$n,  des2$n)
  expect_equal(des$mu, des2$mu)
  expect_equal(des$sd, des2$sd)
  expect_equal(des$r,  des2$r)
})

test_that("get it wrong", {
  # set up interactive answers
  f <- file()
  options(faux.connection = f)
  lines <- c(
    "X", "1", "", "A", "X", "3", "", "A1", "", "A2", "", "A3", "X", "1", 
    "", "B", "X", "2", "", "B1", "", "B2", 
    "", "y", "", "id", "X", "10", "X", "1,2,3", "X", "4,5,6", 
    "X", "1", "X", "1", "X", ".5", "X", ".4, .5, .6"
  )
  ans <- paste(lines, collapse = "\n")
  write(ans, f)
  
  capture_output_lines({des <- interactive_design(plot = FALSE)})
  
  options(faux.connection = stdin()) # reset connection
  close(f) # close the file
  
  # check match
  expect_equal(des$within,  des2$within)
  expect_equal(des$between, des2$between)
  expect_equal(des$id, des2$id)
  expect_equal(des$dv, des2$dv)
  expect_equal(des$n,  des2$n)
  expect_equal(des$mu, des2$mu)
  expect_equal(des$sd, des2$sd)
  expect_equal(des$r,  des2$r)
})
