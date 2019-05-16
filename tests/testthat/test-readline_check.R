context("test-readline_check")

# test_that("readline", {
#   skip_on_cran()
#   skip_on_travis()
#   
#   n <- readline_check("Type a number: ", "numeric")
#   expect_true(is.numeric(n))
#   c2 <- readline_check("Type two characters: ", "length", 2)
#   expect_true(nchar(c2)==2)
#   c3 <- readline_check("Type at least 3 characters: ", "minlength", 3)
#   expect_true(nchar(c3) >= 3)
#   c4 <- readline_check("Type no more than 4 characters: ", "maxlength", 4)
#   expect_true(nchar(c4) <= 4)
#   ln <- readline_check("Type a letter and a number: ", "grep", pattern = "^[a-zA-Z]\\d$")
#   expect_equal(grep("^[a-zA-Z]\\d$", ln), 1)
#   yes <- readline_check("Type 'yes': ", "exact", "yes")
#   expect_equal(yes, "yes")
# })
