library(faux)
library(tidyverse)


# speed test ----


within <- list(
  "W" = c("W1", "W2")
)

between <- list(
  "B" = c("B1", "B2")
)

mu <- list(
  "B1" = c(10, 10),
  "B2" = c(10, 10)
)

anova_func <- function(i) {
  utils::setTxtProgressBar(pb, i)
  df <- sim_design(within, between, n = 20, mu = mu, frame_long = TRUE)
  aov <- aov(val~B*W+Error(sub_id/W), data = df, contrasts = NULL) %>% 
    summary() %>% 
    unclass()
  
  b <- aov[[1]][[1]] %>% tibble::as_tibble(rownames = "term")
  w <- aov[[2]][[1]] %>% tibble::as_tibble(rownames = "term")
  b$`den Df` <- b$Df[length(b$Df)]
  w$`den Df` <- w$Df[length(w$Df)]
  
  dplyr::bind_rows(b, w) %>%
    dplyr::filter(term != "Residuals") %>%
    dplyr::mutate(term = trimws(term)) %>%
    dplyr::select(`term`, `num Df` = `Df`, `den Df`, 
                  `Sum Sq`, `Mean Sq`, `F` = `F value`, 
                  `p` = `Pr(>F)`)
}

reps <- 10000
pb <- txtProgressBar(max = reps)
timestamp()
sims <- purrr::map_df(1:reps, anova_func)
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

##------ Mon Apr 29 16:59:53 2019 ------##
# > sims <- purrr::map_df(1:1e4, anova_func)
##------ Mon Apr 29 17:02:51 2019 ------##

