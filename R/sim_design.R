#' Simulate Data from Design
#'
#' \code{sim_design()} generates a data table with a specified within and between design.
#'
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param mu a vector giving the means of the variables (numeric vector of length 1 or vars)
#' @param sd the standard deviations of the variables (numeric vector of length 1 or vars)
#' @param r the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' @param long Whether the returned tbl is in wide (default = FALSE) or long (TRUE) format
#' @param plot whether to show a plot of the design
#' @param seed a single value, interpreted as an integer, or NULL (see set.seed)
#' 
#' @return a tbl
#' 
#' @export
#' 
sim_design <- function(within = list(), between = list(), 
                       n = 100, mu = 0, sd = 1, r = 0, 
                       empirical = FALSE, long = FALSE, 
                       plot = FALSE, seed = NULL) {
  # check the design is specified correctly
  design <- check_design(within = within, between = between, 
                         n = n, mu = mu, sd = sd, r = r, plot = plot)
  
  # simulate the data
  sim_design_(design, empirical = empirical, long = long, seed = seed)
}

#' Fix name labels
#' 
#' Fixes if a factor list does not have named levels or has special characters in the names
#' 
#' @param x the list to fix
#' 
#' @return the fixed list
#' @keywords internal
#' 
fix_name_labels <- function(x) {
  if (is.null(names(x))) { names(x) <- x }
  nm <- names(x)
  # get rid of non-word characters and underscores because they mess up separate
  names(x) <- gsub("(\\W|_)", ".", nm) 
  x
}

#' Simulate Data from Design
#'
#' \code{sim_design_} generates a data table with a specified design
#'
#' @param design A list of design parameters created by check_design()
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' @param long Whether the returned tbl is in wide (default = FALSE) or long (TRUE) format
#' @param seed a single value, interpreted as an integer, or NULL (see set.seed)
#' 
#' @return a tbl
#' @keywords internal
#' 
sim_design_ <- function(design, empirical = FALSE, long = FALSE, seed = NULL) {
  set.seed(seed)
  list2env(design, envir = environment())
  
  # get factor names
  within_factors <- names(within)
  between_factors <- names(between)
  
  # handle no w/in or btwn
  if (length(between_factors) == 0) between_factors <- ".tmpvar."
  if (length(within_factors) == 0)  within_factors  <- ".tmpvar." 
  
  # figure out number of subjects and their IDs
  sub_n <- sum(cell_n[,1])
  sub_id <- make_id(sub_n)
  
  # simulate data for each between-cell
  for (cell in cells_b) {
    if (length(within)) {
      cell_vars <- rnorm_multi(
        n = cell_n[cell,1], vars = length(cells_w), 
        mu = cell_mu[cell,], sd = cell_sd[cell,], r = cell_r[[cell]], 
        varnames = cells_w, empirical = empirical
      ) %>%
        dplyr::mutate("btwn" = cell)
    } else {
      # fully between design
      sd2 <- cell_sd[cell,] %>% as.matrix() %>% as.vector()
      val <- MASS::mvrnorm(n = cell_n[cell,1], 
                           mu = cell_mu[cell,] %>% as.matrix() %>% as.vector(), 
                           Sigma = sd2 %*% t(sd2), 
                           empirical = empirical)
      cell_vars <- data.frame("val" = val)  %>%
        dplyr::mutate("btwn" = cell)
    }
    
    # add cell values to df
    if (cell == cells_b[1]) { 
      df <- cell_vars # first cell sets up the df
    } else {
      df <- dplyr::bind_rows(df, cell_vars)
    }
  }
  
  # set column order
  col_order <- c("sub_id", between_factors, cells_w) %>%
    setdiff(".tmpvar.")
  
  # create wide dataframe
  df_wide <- df %>%
    tidyr::separate("btwn", between_factors, sep = "_") %>%
    dplyr::mutate("sub_id" = sub_id) %>%
    dplyr::mutate_at(c(between_factors), ~as.factor(.)) %>%
    dplyr::select(tidyselect::one_of(col_order))
  
  # put factors in order
  factors_to_order <- setdiff(between_factors, ".tmpvar.")
  for (f in factors_to_order) {
    df_wide[[f]] <- factor(df_wide[[f]], levels = names(between[[f]]))
  }
  
  if (long == TRUE && length(within)) {
    # not necessary for fully between designs
    col_order <- c("sub_id", between_factors, within_factors, "val") %>%
      setdiff(".tmpvar.")
    
    df_long <- df_wide %>%
      tidyr::gather("w_in", "val", tidyselect::one_of(cells_w)) %>%
      tidyr::separate("w_in", within_factors, sep = "_") %>%
      dplyr::select(tidyselect::one_of(col_order)) %>%
      dplyr::mutate_at(within_factors, ~as.factor(.))
    
    # put factors in order
    factors_to_order <- setdiff(within_factors, ".tmpvar.")
    for (f in factors_to_order) {
      df_long[[f]] <- factor(df_long[[f]], levels = names(within[[f]]))
    }
    
    return(df_long)
  }
  
  df_wide
}
