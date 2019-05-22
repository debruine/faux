#' Plot design
#'
#' \code{plot_design()} plots the specified within and between design
#'
#' @param input A list of design parameters created by check_design() or a data tbl (in long format)
#' @param ... A list of factor names to determine visualisation (see vignette)
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
#' data <- sim_design(within, between, plot = FALSE)
#' plot_design(data)
#' 
#' @export
#' 
plot_design <- function(input, ...) {
  if (!is.data.frame(input) && is.list(input)) {
    plot_data <- FALSE
    design <- input
    data <- sim_design_(design = design, empirical = TRUE, long = TRUE)
  } else if (is.data.frame(input)) {
    plot_data <- TRUE
    data <- input
    if ("design" %in% names(attributes(data))) {
      design <- attributes(data)$design
    }
    if (!(names(design$dv) %in% names(data))) {
      # get data into long format
      data <- wide2long(data)
    }
  } else {
    stop("input must be a design list or a data frame")
  }
  
  factors <- c(design$within, design$between)
  factor_n <- length(factors)
  f <- dplyr::syms(names(factors)) # make it possible to use strings to specify columns
  dv <- dplyr::sym(names(design$dv))
  
  if (c(...) %>% length()) {
    f <- dplyr::syms(c(...))
  }
  
  # use long names for factors
  for (col in names(factors)) {
    lvl <- factors[[col]] %>% names()
    lbl <- factors[[col]]
    data[[col]] <- factor(data[[col]], levels = lvl, labels = lbl)
  }
  
  if (factor_n == 0) {
    p <- ggplot(data, aes(x = 0, y = !!dv, fill = "red", color = "red")) +
      xlab(design$dv[[1]]) + theme_bw() +
      theme(axis.text.x.bottom = element_blank(),
            axis.ticks.x.bottom = element_blank(),
            legend.position = "none")
  } else if (factor_n == 1) {
    p <- ggplot(data, aes(!!f[[1]], !!dv,
                          fill = !!f[[1]],
                          color = !!f[[1]])) + 
      theme_bw() +
      theme(legend.position = "none")
  } else {
    p <- ggplot(data, aes(!!f[[1]], !!dv,
                          fill = !!f[[2]],
                          color = !!f[[2]])) + 
      theme_bw()
  }
  
  if (factor_n > 2) {
    expr <- switch(factor_n,
      NULL,
      NULL,
      rlang::expr(!!f[[3]] ~ .),
      rlang::expr(!!f[[3]] ~ !!f[[4]]),
      rlang::expr(!!f[[3]] ~ !!f[[4]]*!!f[[5]]),
      rlang::expr(!!f[[3]]*!!f[[4]] ~ !!f[[5]]*!!f[[6]])
    )
    p <- p + facet_grid(eval(expr), labeller = "label_both")
  }
  
  # add text y-label to all plots
  p <- p + ylab(design$dv[[1]])
  
  if (plot_data) {
    p <- p + geom_violin(color = "black", alpha = 0.5,
                         position = position_dodge(width = 0.9)) +
      geom_boxplot(width = 0.25, color = "black",
                   position = position_dodge(width = 0.9),
                   show.legend = FALSE)
  } else {
    minsd <- function(x) { mean(x) - sd(x) }
    maxsd <- function(x) { mean(x) + sd(x) }
    
    p <- p + stat_summary(
      fun.y = mean, 
      fun.ymin = minsd,
      fun.ymax = maxsd,
      geom='pointrange', 
      shape = 10,
      size = 1,
      position = position_dodge(width = 0.9))
  }
  
  p
}
