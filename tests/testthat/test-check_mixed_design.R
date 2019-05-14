context("test-check_mixed_design")

test_that("faceratings", {
  des <- check_mixed_design(fr10, "rating", "rater_id", "face_id")
  
  expect_equal(des$grand_i, 3.32, tolerance = 1e-06)
  expect_equal(des$sub_sd, 1.126342, tolerance = 1e-06)
  expect_equal(des$item_sd, 0.7032301, tolerance = 1e-06)
  expect_equal(des$error_sd, 1.193817, tolerance = 1e-06)
  expect_equal(des$random_effects$rater_id$`(Intercept)` %>% length(), 10)
  expect_equal(des$random_effects$face_id$`(Intercept)` %>% length(), 10)
})


test_that("formula", {
  fm <- "rating ~ face_sex + (1 | face_id) + (1 | rater_id)"
  des <- check_mixed_design(fr10, "rating", "rater_id", "face_id", fm)
  
  expect_equal(des$grand_i, 3.7, tolerance = 1e-06)
  expect_equal(des$sub_sd, 1.12612, tolerance = 1e-06)
  expect_equal(des$item_sd, 0.3915544, tolerance = 1e-06)
  expect_equal(des$error_sd, 1.193811, tolerance = 1e-06)
  expect_equal(des$random_effects$rater_id %>% names(), "(Intercept)")
  expect_equal(des$random_effects$face_id %>% names(), "(Intercept)")
})
