context("test-readline_check")

test_that("interactive", {
  skip("interactive is too annoying")

  n <- readline_check("Type a number: ", "numeric")
  expect_true(is.numeric(n))
  n0 <- readline_check("Type a number less than or equal to 0: ", "numeric", max = 0)
  expect_true(is.numeric(n0))
  expect_true(n0 <= 0)
  n10 <- readline_check("Type a number from 10 to 20: ", "numeric", min = 10, max = 20)
  expect_true(is.numeric(n10))
  expect_true(n10 <= 20)
  expect_true(n10 >= 10)
  c2 <- readline_check("Type two characters: ", "length", min=2, max = 2)
  expect_true(nchar(c2)==2)
  c3 <- readline_check("Type at least 3 characters: ", "length", min =3)
  expect_true(nchar(c3) >= 3)
  c4 <- readline_check("Type no more than 4 characters: ", "length", max = 4)
  expect_true(nchar(c4) <= 4)
  ln <- readline_check("Type a letter and a number: ", "grep", 
                       warning = "\033[31mIncorrect.\033[39m Type only a letter and a number: ", 
                       pattern = "^[a-zA-Z]\\d$")
  expect_equal(grep("^[a-zA-Z]\\d$", ln), 1)
})
