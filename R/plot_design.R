#' Plot design
#'
#' \code{plot_design()} plots the specified within and between design
#'
#' @param design A list of design parameters created by check_design() or a data tbl (in long format)
#' @param id the column name(s) that identify a unit of analysis
#' @param dv the column name that identifies the DV
#' 
#' @return plot
#' 
#' @examples 
#' 
#' within <- list(time = c("day", "night"))
#' between <- list(pet = c("dog", "cat"))
#' check_design(within, between) %>% plot_design()
#' 
#' @export
#' 
plot_design <- function(design, id = "id", dv = "y") {
  if (!is.data.frame(design) && is.list(design)) {
    data <- sim_design_(design, empirical = TRUE, long = TRUE)
  } else if (is.data.frame(design)) {
    data <- design
    design <- get_design_long(data, id = id, dv = dv)
  } else {
    stop("design must be a design list or a data frame")
  }
  
  factors <- c(names(design$within), names(design$between))
  factor_n <- length(factors)
  f <- dplyr::syms(factors) # make it possible to use strings to specify columns
  dv <- dplyr::sym(dv)
  
  if (factor_n == 1) {
    p <- ggplot2::ggplot(data, ggplot2::aes(!!f[[1]], !!dv))
  } else if (factor_n == 2) {
    p <- ggplot2::ggplot(data, ggplot2::aes(!!f[[1]], !!dv, color = !!f[[2]]))
  } else if (factor_n == 3) {
    p <- ggplot2::ggplot(data, ggplot2::aes(!!f[[1]], !!dv, color = !!f[[2]])) +
      ggplot2::facet_grid(
        eval(rlang::expr(!!f[[3]] ~ .)), 
        labeller = "label_both"
      )
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(!!f[[1]], !!dv, color = !!f[[2]])) +
      ggplot2::facet_grid(
        eval(rlang::expr(!!f[[3]] ~ !!f[[4]])), 
        labeller = "label_both"
      )
    
    if (factor_n > 4) message("Can't plot more than 4 factors")
  }
  
  minsd <- function(x) { mean(x) - sd(x) }
  maxsd <- function(x) { mean(x) + sd(x) }
  
  p <- p + ggplot2::stat_summary(fun.y = mean, 
                                 fun.ymin = minsd,
                                 fun.ymax = maxsd,
                                 geom='pointrange', 
                                 shape = 10,
                                 size = 1,
                                 position = ggplot2::position_dodge(width = 0.9))
  p <- p + ggplot2::theme_bw()
  
  p
}
