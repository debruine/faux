context("test-check_mixed_design")

test_that("faceratings", {
  des <- check_mixed_design(fr4, "rating", "rater_id", "face_id")
  
  expect_equal(des$grand_i, 2.927083, tolerance = 1e-06)
  expect_equal(des$sub_sd, 0.9361247, tolerance = 1e-06)
  expect_equal(des$item_sd, 0.6612174, tolerance = 1e-06)
  expect_equal(des$error_sd, 1.072141, tolerance = 1e-06)
  expect_equal(des$random_effects$rater_id$`(Intercept)` %>% length(), 24)
  expect_equal(des$random_effects$face_id$`(Intercept)` %>% length(), 32)
})


test_that("formula", {
  fm <- "rating ~ face_sex + (1 | face_id) + (1 | rater_id)"
  des <- check_mixed_design(fr4, "rating", "rater_id", "face_id", fm)
  
  expect_equal(des$grand_i, 3.226562, tolerance = 1e-06)
  expect_equal(des$sub_sd, 0.9361247, tolerance = 1e-06)
  expect_equal(des$item_sd, 0.5980894, tolerance = 1e-06)
  expect_equal(des$error_sd, 1.072141, tolerance = 1e-06)
  expect_equal(des$random_effects$rater_id %>% names(), "(Intercept)")
  expect_equal(des$random_effects$face_id %>% names(), "(Intercept)")
})
