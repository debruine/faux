library(tidyverse)
library(faux)

des <- check_design(list(time = c("morning", "noon", "night"), 
                         pet = c("dog", "cat", "ferret"), 
                         condition = c("A", "B")), 
                    mu = rep(1:6, 3), 
                    sd = 1)


within <- list(
  time = c("morning", "night"),
  condition = c("A", "B", "C")
)
between <- list(
  pet = c("dog", "cat"),
  x = c("X1", "X2"))

r <- list(
  dog_X1 = seq(.1, by = .025, length.out = 15),
  dog_X2 = seq(.2, by = .025, length.out = 15),
  cat_X1 = seq(.3, by = .025, length.out = 15),
  cat_X2 = seq(.4, by = .025, length.out = 15)
)

w <- tidyr::crossing(time, condition) %>%
  unite(w, 1:ncol(.)) %>%
  pull(w)

design <- tidyr::crossing(pet, time, condition)
for (wc in w) {
  design[wc] <- 0
}
design$mu <- 1:8
design$sd <- 1



jsonlite::toJSON(design, pretty = TRUE)
