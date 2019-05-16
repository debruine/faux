context("test-readline_check")

test_that("readline", {
  n <- readline_check("Type a number: ", "numeric")
  c2 <- readline_check("Type two characters: ", "length", 2)
  c3 <- readline_check("Type at least 3 characters: ", "minlength", 3)
  c4 <- readline_check("Type no more than 4 characters: ", "maxlength", 4)
  ln <- readline_check("Type a letter and a number: ", "grep", pattern = "^[a-zA-Z]\\d$")
  yes <- readline_check("Type 'yes': ", "exact", "yes")
})
