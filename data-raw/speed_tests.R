library(faux)
library(tidyverse)


# speed test ----


within <- list(
  "W" = c("W1", "W2"),
  "X" = c("X1", "X2")
)

between <- list()

mu <- c(W1_X1 = 10, W1_X2 = 12, W2_X1 = 10, W2_X2 = 10)


anova_func <- function(i, v = "afex") {
  #utils::setTxtProgressBar(pb, i)
  df <- sim_design(within, between, n = 20, mu = mu, sd = 4, frame_long = TRUE)
  
  if (v == "afex") {
    afex::aov_4(val~(X*W|sub_id), data = df, return = "aov") %>%
      broom::tidy()
  } else if (v == "aov") {
    aov(val~(X*W)+Error(sub_id/(X*W)), data = df, contrasts = NULL) %>%
      broom::tidy()
  }
}

anova_func2 <- function(i) {
  df <- sim_design(within, between, n = 20, mu = mu, sd = 4, frame_long = TRUE)
  
  aov(val~(X*W)+Error(sub_id/(X*W)), data = df, contrasts = NULL) %>%
      broom::tidy()
}

reps <- 10000
#pb <- utils::txtProgressBar(max = reps)
system.time(
  sims_afex <- purrr::map_df(1:reps, anova_func, v = "afex")
)
system.time(
  sims_aov <- purrr::map_df(1:reps, anova_func, v = "aov")
)
#close(pb)

sims_afex %>%
  filter(term != "Residuals") %>%
  group_by(term) %>%
  summarise(power = mean(p.value < .05))

sims_aov %>%
  filter(term != "Residuals") %>%
  group_by(term) %>%
  summarise(power = mean(p.value < .05))


alpha <- 0.05

power <- sims %>%
  dplyr::group_by(factor) %>%
  dplyr::summarise(power = mean(`Pr(>F)` < alpha))

sims %>%
  select(factor, p = `Pr(>F)`) %>%
  ggplot(aes(p, fill = factor)) +
  facet_grid(~factor) +
  geom_histogram(binwidth = alpha, color = "black", boundary = 0)


##------ Sun Apr 28 20:43:15 2019 ------##
#   df <- purrr::map(1:1e4, ~sim_design(within, between, n = 20))
##------ Sun Apr 28 20:44:48 2019 ------##

##------ Mon Apr 29 16:59:53 2019 ------##
# > sims <- purrr::map_df(1:1e4, anova_func)
##------ Mon Apr 29 17:02:51 2019 ------##

