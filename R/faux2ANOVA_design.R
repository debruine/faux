#' Convert faux deisgn to ANOVApower design
#'
#' @param design faux design list (e.g., from check_design)
#' @param plot whether to show the plot from ANOVApower::ANOVA_design
#'
#' @return a design list for ANOVApower
#' @export
#'
#' @examples
#' within <- list(pet = c(dog = "Dog Owners", cat = "Cat Owners"))
#' between <- list(time = c("day" = "Before 7pm", "night" = "After 7pm"))
#' faux_des <- check_design(within, between, n = 50, mu = 1:4, sd = 2, r = 0.5)
#' apower_des <- faux2ANOVA_design(faux_des)
#' 
faux2ANOVA_design <- function(design, plot = TRUE) {
  if (!requireNamespace("ANOVApower", quietly = TRUE)) {
    stop("Package \"ANOVApower\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  factors <- c(design$between, design$within)
  if (length(factors) < 1) {
    stop("You need at least one factor to use ANOVApower")
  } else if (length(factors) > 3) {
    stop("You can't use ANOVApower with more than 3 factors")
  }
  
  b <- design$between %>% lapply(length) %>% lapply(paste0, "b")
  w <- design$within %>% lapply(length) %>% lapply(paste0, "w")
  string <- c(b, w) %>% paste(collapse = "*")
  
  # warn about n
  n <- design$n %>%  unlist() %>% unique()
  if (length(n) > 1) {
    n <- design$n %>%  unlist() %>% mean() %>% round()
    warning("Your design has different n for the between-subject factors. ANOVApower does not support this, so will use the mean n of ", n)
  }
  
  # labelnames
  ln <- c()
  for (i in 1:length(factors)) {
    fctr <- names(factors)[i]
    lvls <- names(factors[[i]])
    ln <- c(ln, fctr, lvls)
  }
  
  # correlation matrix
  n_cells <- lapply(factors, length) %>% unlist() %>% prod()
  big_r <- matrix(rep(0, n_cells^2), nrow = n_cells)
  
  if (length(design$r)) { # skip if no within-subject factors
    for (i in 1:length(design$r)) {
      mat <- design$r[[i]]
      offset <- (i-1) * nrow(mat)
      for (r in 1:nrow(mat)) {
        for (c in 1:ncol(mat)) {
          big_r[(r+offset), (c+offset)] <- mat[r, c]
        }
      }
    }
  }
  
  
  ap <- list(
    design = string,
    n = n[[1]], # update when they vary n
    mu = unlist(design$mu) %>% unname(),
    sd = unlist(design$sd) %>% unname(),
    r = big_r[upper.tri(big_r, diag = FALSE)],
    labelnames = ln,
    plot = plot
  )
  
  do.call(ANOVApower::ANOVA_design, ap)
}
