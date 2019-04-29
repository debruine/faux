# speed test ----

library(tidyverse)
within <- list(
  "A" = c("A1", "A2")
)

between <- list(
  "B" = c("B1", "B2")
)

mu <- list(
  "B1" = c(10, 10),
  "B2" = c(10, 10)
)

func <- function(i) {
  utils::setTxtProgressBar(pb, i)
  df <- sim_design(within, between, n = 20, mu = mu, frame_long = TRUE)
  suppressMessages(
    anova <- afex::aov_4(val~B*(A|sub_id), data = df, check_contrasts = TRUE)
  )
  anova$anova_table %>%
    tibble::as_tibble(rownames = "factor")
}

reps <- 2
pb <- txtProgressBar(max = reps)
timestamp()
sims <- purrr::map_df(1:reps, func)
timestamp()
close(pb)

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

