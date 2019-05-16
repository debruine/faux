context("test-unique_pairs")

test_that("number", {
  expect_equal(unique_pairs(2), "A_B")
  expect_equal(unique_pairs(3), c("A_B", "A_C", "B_C"))
  expect_equal(unique_pairs(4), c("A_B", "A_C", "A_D", "B_C", "B_D", "C_D"))
  expect_error(unique_pairs(1), "There must be at least 2 levels")
})

test_that("levels", {
  expect_equal(unique_pairs(c("dog", "cat")), "dog_cat")
  expect_equal(unique_pairs(c("Z", "A", "P")), c("Z_A", "Z_P", "A_P"))
  expect_equal(unique_pairs(c("a.name", "another.name")), "a.name_another.name")
  expect_error(unique_pairs(c("same", "same")), "You have duplicate levels")
  expect_error(unique_pairs(c("one")), "There must be at least 2 levels")
})
