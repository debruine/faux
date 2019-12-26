#' Simulate data from design
#'
#' \code{sim_design()} generates a data table with a specified within and between design.
#'
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param mu the means of the variables
#' @param sd the standard deviations of the variables
#' @param r the correlations among the variables (can be a single number, full correlation matrix as a matric or vector, or a vector of the upper right triangle of the correlation matrix
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' @param long Whether the returned tbl is in wide (default = FALSE) or long (TRUE) format
#' @param dv the name of the dv for long plots (defaults to y)
#' @param id the name of the id column (defaults to id)
#' @param plot whether to show a plot of the design
#' @param seed a single value, interpreted as an integer, or NULL (see set.seed)
#' @param interactive whether to run the function interactively
#' @param design a design list including within, between, n, mu, sd, r, dv, id
#' @param rep the number of data frames to return (default 1); if greater than 1, the returned data frame is nested by rep
#' 
#' @return a tbl
#' 
#' @export
#' @importFrom rlang := 
#' 
sim_design <- function(within = list(), between = list(), 
                       n = 100, mu = 0, sd = 1, r = 0, 
                       empirical = FALSE, long = FALSE, 
                       dv = list(y = "value"), 
                       id = list(id = "id"),
                       plot = faux_options("plot"), 
                       seed = NULL, interactive = FALSE, 
                       design = NULL, rep = 1) {
  # check the design is specified correctly
  if (interactive) {
    design <- interactive_design(plot = plot)
  } else if (!is.null(design)) { 
    #& !("design" %in% class(design))) {
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
  data <- sim_data(design, empirical = empirical, long = long, seed = seed, rep = rep)
  
  attr(data, "design") <- design
  
  return(data)
}

#' Simulate data from design (internal)
#'
#' @param design A list of design parameters created by check_design()
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' @param long Whether the returned tbl is in wide (default = FALSE) or long (TRUE) format
#' @param rep the number of data frames to return (default 1); if greater than 1, the returned data frame is nested by rep
#' @param seed a single value, interpreted as an integer, or NULL (see set.seed)
#' @param sep separator for within-columns, defaults to _ (see tidyr::separate)
#' 
#' @return a tbl
#' @export
#' 
sim_data <- function(design, empirical = FALSE, long = FALSE, 
                     rep = 1, seed = NULL, sep = faux_options("sep")) {
  if (!is.numeric(rep)) {
    stop("rep must be a number")
  } else if (rep < 1) {
    stop("rep must be >= 1")
  } else if (rep%%1 != 0) {
    warning("rep should be an integer")
  }
  
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
  cells_w <- faux:::cell_combos(within, dv)
  cells_b <- faux:::cell_combos(between, dv) 
  
  # get factor names
  within_factors <- names(within)
  between_factors <- names(between)
  
  # handle no w/in or btwn
  if (length(between_factors) == 0) between_factors <- ".tmpvar."
  if (length(within_factors) == 0)  within_factors  <- ".tmpvar."
  
  # simulate data for each between-cell
  for (rep_n in 1:rep) {
    for (cell in cells_b) {
      cell_vars <- rnorm_multi(
        n = n[[cell]], 
        vars = length(cells_w), 
        mu = mu[[cell]] %>% unlist(), 
        sd = sd[[cell]] %>% unlist(), 
        r = r[[cell]], 
        varnames = cells_w, 
        empirical = empirical
      )
      cell_vars$btwn <- cell
      
      # add cell values to df
      if (cell == cells_b[1]) { 
        sub_df <- cell_vars # first cell sets up the df
      } else {
        sub_df <- dplyr::bind_rows(sub_df, cell_vars)
      }
    }
    sub_df$`.rep.` <- rep_n
    if (rep_n == 1) {
      df <- sub_df
    } else {
      df <- dplyr::bind_rows(df, sub_df)
    }
  }
  
  # set column order
  col_order <- c(".rep.", id, between_factors, cells_w) %>%
    setdiff(c(".tmpvar."))
  
  # figure out total number of subjects
  sub_n <- unlist(n) %>% sum()
  
  # create wide dataframe
  df_wide <- df %>%
    tidyr::separate("btwn", between_factors, sep = sep) %>%
    dplyr::group_by(`.rep.`) %>%
    dplyr::mutate(!!id := make_id(sub_n)) %>%
    dplyr::ungroup(`.rep.`) %>%
    dplyr::mutate_at(c(between_factors), ~as.factor(.)) %>%
    dplyr::select(tidyselect::one_of(col_order))
  
  # put factors in order
  factors_to_order <- setdiff(between_factors, ".tmpvar.")
  for (f in factors_to_order) {
    df_wide[[f]] <- factor(df_wide[[f]], levels = names(between[[f]]))
  }
  
  if (long == TRUE && length(within)) {
    # not necessary for fully between designs
    col_order <- c(".rep.", id, between_factors, within_factors, dv) %>%
      setdiff(".tmpvar.")
    
    df_long <- df_wide %>%
      tidyr::gather("w_in", !!dv, tidyselect::one_of(cells_w)) %>%
      tidyr::separate("w_in", within_factors, sep = sep) %>%
      dplyr::select(tidyselect::one_of(col_order)) %>%
      dplyr::mutate_at(within_factors, ~as.factor(.))
    
    # put factors in order
    factors_to_order <- setdiff(within_factors, ".tmpvar.")
    for (f in factors_to_order) {
      df_long[[f]] <- factor(df_long[[f]], levels = names(within[[f]]))
    }

    df_return <- df_long
  } else {
    df_return <- df_wide
  }
  
  class(df_return) <- c("faux", "data.frame")
  
  if (rep == 1) {
    df_return$`.rep.` <- NULL
  } else {
    df_return <- df_return %>%
      dplyr::group_by(`.rep.`) %>%
      tidyr::nest() %>%
      dplyr::rename("rep" = `.rep.`)
  }
  return(df_return)
}
