context("test-messages")

test_that("check", {
  expect_message(message("green"), "\033[32mgreen\033[39m", fixed = 1)
  expect_message(message("pipes?", "no!"), "\033[32mpipes?no!\033[39m", fixed = 1)
})
