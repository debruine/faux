library(tidyverse)
library(purrr)

sim_cc <- function(sub_n, item_n, grand_i, sub_sd, item_sd, error_sd, ...) {
  data <- sim_mixed_cc(sub_n, item_n, grand_i, sub_sd, item_sd, error_sd)
  
  lme4::lmer(val ~ 1 + (1 | sub_id) + (1 | item_id), data = data) %>%
    broom.mixed::tidy(effects = "ran_pars") %>%
    mutate(sub_n = sub_n, 
           item_n = item_n, 
           grand_i = grand_i, 
           sub_sd = sub_sd, 
           item_sd = item_sd, 
           error_sd = error_sd)
}

# sims <- expand.grid(
#   sub_n = c(10, 20, 40), 
#   item_n = c(10, 20, 40), 
#   grand_i = 0, 
#   sub_sd = 1, 
#   item_sd = 1, 
#   error_sd = c(0.5, 1, 2),
#   rep = 1:1000
# ) %>%
#   purrr::transpose() %>%
#   purrr::map_df(~purrr::pmap_df(., sim_cc))
# 
# saveRDS(sims, file = "tests/mess/sims.rds")

sims <- readRDS(file = "tests/mess/sims.rds")

params <- sims %>%
  group_by(group, sub_n, item_n, error_sd) %>%
  summarise(m = mean(estimate), sd = sd(estimate))

ggplot(params, aes(error_sd, sd, color = factor(sub_n))) +
  geom_point() +
  facet_grid(group~item_n)
