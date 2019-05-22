#' Plot design
#'
#' \code{plot_design()} plots the specified within and between design
#'
#' @param design A list of design parameters created by check_design() or a data tbl (in long format)
#' 
#' @return plot
#' 
#' @examples 
#' 
#' within <- list(time = c("day", "night"))
#' between <- list(pet = c("dog", "cat"))
#' des <- check_design(within, between, plot = FALSE)
#' plot_design(des)
#' 
#' @export
#' 
plot_design <- function(design) {
  if (!is.data.frame(design) && is.list(design)) {
    data <- sim_design_(design = design, empirical = TRUE, long = TRUE)
  } else if (is.data.frame(design)) {
    data <- design
    if ("design" %in% names(attributes(data))) {
      design <- attributes(data)$design
    }
  } else {
    stop("design must be a design list or a data frame")
  }
  
  factors <- c(design$within, design$between)
  factor_n <- length(factors)
  f <- dplyr::syms(names(factors)) # make it possible to use strings to specify columns
  dv <- dplyr::sym(names(design$dv))
  
  # use long names for factors
  for (col in names(factors)) {
    lvl <- factors[[col]] %>% names()
    lbl <- factors[[col]]
    data[[col]] <- factor(data[[col]], levels = lvl, labels = lbl)
  }
  
  if (factor_n == 0) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = 0, y = !!dv)) +
      ggplot2::xlab(design$dv[[1]]) + ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x.bottom = ggplot2::element_blank(),
                     axis.ticks.x.bottom = ggplot2::element_blank())
  } else if (factor_n == 1) {
    p <- ggplot2::ggplot(data, ggplot2::aes(!!f[[1]], !!dv)) + ggplot2::theme_bw()
  } else if (factor_n == 2) {
    p <- ggplot2::ggplot(data, ggplot2::aes(!!f[[1]], !!dv, color = !!f[[2]])) + ggplot2::theme_bw()
  } else  {
    expr <- switch(factor_n,
      NULL,
      NULL,
      rlang::expr(!!f[[3]] ~ .),
      rlang::expr(!!f[[3]] ~ !!f[[4]]),
      rlang::expr(!!f[[3]] ~ !!f[[4]]*!!f[[5]]),
      rlang::expr(!!f[[3]]*!!f[[4]] ~ !!f[[5]]*!!f[[6]])
    )
    p <- ggplot2::ggplot(data, ggplot2::aes(!!f[[1]], !!dv, color = !!f[[2]])) +
      ggplot2::facet_grid(eval(expr), labeller = "label_both") + 
      ggplot2::theme_bw()
  }
  
  minsd <- function(x) { mean(x) - sd(x) }
  maxsd <- function(x) { mean(x) + sd(x) }
  
  p <- p + ggplot2::ylab(design$dv[[1]]) +
    ggplot2::stat_summary(fun.y = mean, 
                                 fun.ymin = minsd,
                                 fun.ymax = maxsd,
                                 geom='pointrange', 
                                 shape = 10,
                                 size = 1,
                                 position = ggplot2::position_dodge(width = 0.9))
  
  p
}
