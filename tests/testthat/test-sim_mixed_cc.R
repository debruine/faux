test_that("default", {
  data <- sim_mixed_cc()
  
  expect_equal(nrow(data), 2000)
  expect_equal(ncol(data), 3)
  expect_equal(names(data), c("sub_id", "item_id", "val"))
  
  res <- lme4::lmer(val ~ 1 + (1 | sub_id) + (1 | item_id), data = data) %>%
    broom.mixed::tidy(effects = "ran_pars")
})

test_that("default", {
  data <- sim_mixed_cc(100, 100)
  
  expect_equal(nrow(data), 10000)
  expect_equal(ncol(data), 3)
  expect_equal(names(data), c("sub_id", "item_id", "val"))
  
  res <- lme4::lmer(val ~ 1 + (1 | sub_id) + (1 | item_id), data = data) %>%
    broom.mixed::tidy(effects = "ran_pars")
})

library(ggplot2)

sim_cc <- function() {
  data <- sim_mixed_cc(100, 100, 0, 1, 1, 2)
  
  lme4::lmer(val ~ 1 + (1 | sub_id) + (1 | item_id), data = data) %>%
    broom.mixed::tidy(effects = "ran_pars") %>%
    ti
}

sims <- purrr::map_df(1:100, ~sim_cc())

ggplot(sims, aes(estimate, color = group)) +
  geom_density()
