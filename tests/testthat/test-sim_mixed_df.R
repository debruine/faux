context("sim_mixed_df")

test_that("default settings", {
  s10 <- sim_mixed_df(fr10, 10, 10)
  default_names <- c("sub_id", "item_id", "val", "grand_i", "sub_i", "item_i", "err")
  
  expect_equal(nrow(s10), 100)
  expect_equal(ncol(s10), 7)
  expect_equal(names(s10), default_names)
})

test_that("specified dv and IDs", {
  s10 <- sim_mixed_df(fr10, 10, 10, "rating", "rater_id", "face_id")
  default_names <- c("sub_id", "item_id", "val", "grand_i", "sub_i", "item_i", "err")
  
  expect_equal(nrow(s10), 100)
  expect_equal(ncol(s10), 7)
  expect_equal(names(s10), default_names)
})

# seed ----
test_that("seed", {
  df1 <- sim_mixed_df(fr10, 10, 10, seed = 1)
  df2 <- sim_mixed_df(fr10, 10, 10, seed = 1)
  
  expect_equal(df1, df2)
  
  df3 <- sim_mixed_df(fr10, 10, 10, seed = 90210)
  
  expect_true(!identical(df1, df3))
})

# exact items ----
test_that("exact items", {
  check <- check_mixed_design(fr10)
  
  # 20 new subjects with same 10 items
  df_items <- sim_mixed_df(fr10, sub_n = 20, item_n = NULL)
  items <- dplyr::count(df_items, item_id, item_i)
  check_items <- check$random_effects$face_id
  
  expect_equal(items$item_i, check_items[,1])
  
  # 20 new items with same 10 subjects
  df_subs <- sim_mixed_df(fr10, sub_n = NULL, item_n = 20)
  subs <- dplyr::count(df_subs, sub_id, sub_i)
  check_subs <- check$random_effects$rater_id
  
  expect_equal(subs$sub_i, check_subs[,1])
 })

