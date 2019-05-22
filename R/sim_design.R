#' Simulate data from design
#'
#' \code{sim_design()} generates a data table with a specified within and between design.
#'
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param mu the means of the variables
#' @param sd the standard deviations of the variables
#' @param r the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' @param long Whether the returned tbl is in wide (default = FALSE) or long (TRUE) format
#' @param dv the name of the dv for long plots (defaults to y)
#' @param id the name of the id column (defaults to id)
#' @param plot whether to show a plot of the design
#' @param seed a single value, interpreted as an integer, or NULL (see set.seed)
#' @param interactive whether to run the function interactively
#' @param design a design list including within, between, n, mu, sd, r, dv, id
#' 
#' @return a tbl
#' 
#' @export
#' @importFrom rlang := 
#' 
sim_design <- function(within = list(), between = list(), 
                       n = 100, mu = 0, sd = 1, r = 0, 
                       empirical = FALSE, long = FALSE, 
                       dv = list(y = "Score"), 
                       id = list(id = "Subject ID"),
                       plot = TRUE, seed = NULL, 
                       interactive = FALSE, design = NULL) {
  # check the design is specified correctly
  if (interactive) {
    design <- interactive_design(plot = plot)
  } else if (!is.null(design)) {
    # double-check the entered design
    design <- check_design(design = design, plot = plot)
  } else if ("design" %in% class(within)) {
    # design given as first argument: not ideal but handle it
    design <- check_design(design = within, plot = plot)
  } else {
    design <- check_design(within = within, between = between, 
                         n = n, mu = mu, sd = sd, r = r, 
                         dv = dv, id = id, plot = plot)
  }
  
  # simulate the data
  sim_design_(design, empirical = empirical, long = long, seed = seed)
}

#' Simulate data from design (internal)
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
  if (!is.null(seed)) {
    # reinstate system seed after simulation
    sysSeed <- .GlobalEnv$.Random.seed
    on.exit({
      if (!is.null(sysSeed)) {
        .GlobalEnv$.Random.seed <- sysSeed 
      } else {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    })
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  }
  
  list2env(design, envir = environment())
  
  # only use DV and ID names here
  dv <- names(dv)
  id <- names(id)
  
  # define columns
  cells_w <- cell_combos(within, dv)
  cells_b <- cell_combos(between, dv) 
  
  # get factor names
  within_factors <- names(within)
  between_factors <- names(between)
  
  # handle no w/in or btwn
  if (length(between_factors) == 0) between_factors <- ".tmpvar."
  if (length(within_factors) == 0)  within_factors  <- ".tmpvar." 
  
  # figure out number of subjects and their IDs
  sub_n <- unlist(n) %>% sum()
  
  # simulate data for each between-cell
  for (cell in cells_b) {
    cell_vars <- rnorm_multi(
      n = n[[cell]], 
      vars = length(cells_w), 
      mu = mu[[cell]] %>% unlist(), 
      sd = sd[[cell]] %>% unlist(), 
      r = r[[cell]], 
      varnames = cells_w, 
      empirical = empirical
    ) %>%
      dplyr::mutate("btwn" = cell)
    
    # add cell values to df
    if (cell == cells_b[1]) { 
      df <- cell_vars # first cell sets up the df
    } else {
      df <- dplyr::bind_rows(df, cell_vars)
    }
  }
  
  # set column order
  col_order <- c(id, between_factors, cells_w) %>%
    setdiff(".tmpvar.")
  
  # create wide dataframe
  df_wide <- df %>%
    tidyr::separate("btwn", between_factors, sep = "_") %>%
    dplyr::mutate(!!id := make_id(sub_n)) %>%
    dplyr::mutate_at(c(between_factors), ~as.factor(.)) %>%
    dplyr::select(tidyselect::one_of(col_order))
  
  # put factors in order
  factors_to_order <- setdiff(between_factors, ".tmpvar.")
  for (f in factors_to_order) {
    df_wide[[f]] <- factor(df_wide[[f]], levels = names(between[[f]]))
  }
  
  if (long == TRUE && length(within)) {
    # not necessary for fully between designs
    col_order <- c(id, between_factors, within_factors, dv) %>%
      setdiff(".tmpvar.")
    
    df_long <- df_wide %>%
      tidyr::gather("w_in", !!dv, tidyselect::one_of(cells_w)) %>%
      tidyr::separate("w_in", within_factors, sep = "_") %>%
      dplyr::select(tidyselect::one_of(col_order)) %>%
      dplyr::mutate_at(within_factors, ~as.factor(.))
    
    # put factors in order
    factors_to_order <- setdiff(within_factors, ".tmpvar.")
    for (f in factors_to_order) {
      df_long[[f]] <- factor(df_long[[f]], levels = names(within[[f]]))
    }
    
    attr(df_long, "design") <- design
    
    return(df_long)
  }
  
  attr(df_wide, "design") <- design
  
  df_wide
}
