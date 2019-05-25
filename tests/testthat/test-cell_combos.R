context("test-cell_combos")

test_that("0 factors", {
  expect_equal(cell_combos(list()), "y")
  expect_equal(cell_combos(list(), "DV"), "DV")
})

test_that("1 factor", {
  fac <- list(c(A = "A", B = "B", C = "C"))
  expect_equal(cell_combos(fac), LETTERS[1:3])
  
  fac <- list(pet = c(cat = "cat", dog = "dog"))
  expect_equal(cell_combos(fac), c("cat", "dog"))
})


test_that("2 factors", {
  factors <- list(pet = c(cat = "cat", dog = "dog"),
                  time = c(day = "day", night = "night"))
  cells <- cell_combos(factors)
  expect_equal(cells, c("cat_day", "dog_day", "cat_night", "dog_night"))
})

test_that("3 factors", {
  factors <- list(pet = c(cat = "cat", dog = "dog"),
                  time = c(day = "day", night = "night"),
                  condition = c(A = "A", B = "B"))
  cells <- cell_combos(factors)
  expect_equal(cells, c("cat_day_A", "dog_day_A", 
                        "cat_night_A", "dog_night_A", 
                        "cat_day_B", "dog_day_B",
                        "cat_night_B", "dog_night_B"))
})
