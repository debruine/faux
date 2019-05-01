#' Simulate Data from Design
#'
#' \code{sim_design} generates a dataframe with a specified within and between design
#'
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param cors the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param mu a vector giving the means of the variables (numeric vector of length 1 or vars)
#' @param sd the standard deviations of the variables (numeric vector of length 1 or vars)
#' @param empirical logical. If true, mu, sd and cors specify the empirical not population mean, sd and covariance 
#' @param frame_long Whether the returned dataframe is in wide (default = FALSE) or long (TRUE) format
#' 
#' @return dataframe
#' 
#' @export
#' 
sim_design <- function(within = list(), between = list(), 
                       n = 100, cors = 0, mu = 0, sd = 1, 
                       empirical = FALSE, frame_long = FALSE) {
  # check the design is specified correctly
  design <- check_design(within, between, n, cors, mu, sd)
  
  # simulate the data
  sim_design_(design, empirical = empirical, frame_long = frame_long)
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


#' Convert parameter
#' 
#' Converts parameter specification from vector or list format
#' 
#' @param param the parameter (mu, sd, or n)
#' @param cells_b a list of between-subject cell combinations
#' @param cells_w a list of within-subject cells combinations
#' @param type the name of the parameter (for error messages)
#' 
#' @return a data frame 
#' 
convert_param <- function (param, cells_b, cells_w, type = "this parameter") {
  w_n <- length(cells_w)
  b_n <- length(cells_b)
  all_n <- b_n*w_n
  
  if (is.data.frame(param)) { # convert to list first
    # check for row/column confusion
    cols_are_b <- setdiff(names(param), cells_b) %>% length() == 0
    rows_are_w <- setdiff(rownames(param), cells_w) %>% length() == 0
    cols_are_w <- setdiff(names(param), cells_w) %>% length() == 0
    rows_are_b <- setdiff(rownames(param), cells_b) %>% length() == 0
    if (cols_are_b && rows_are_w) {
      # check this first in case rows and cols are the same labels
      param <- as.list(param) %>%  lapply(magrittr::set_names, rownames(param))
    } else if (cols_are_w && rows_are_b) {
      param <- t(param) %>% as.data.frame()
      param <- as.list(param) %>%  lapply(magrittr::set_names, rownames(param))
    } else {
      stop("The ", type, " dataframe is misspecified.")
    }
  }
  
  if (is.list(param)) {
    param2 <- c()
    # add param in right order
    for (f in cells_b) {
      if (length(param[[f]]) == 1) { 
        new_param <- rep(param[[f]], w_n)
      } else if (length(param[[f]]) != w_n) {
        stop("The number of ", type, " for cell ", f, 
             " is not correct. Please specify either 1 or a vector of ", 
             w_n, " per cell")
      } else if (setdiff(cells_w, names(param[[f]])) %>% length() == 0) {
        new_param <- param[[f]][cells_w] # add named parameters in the right order
      } else {
        new_param <- param[[f]] # parameters are not or incorrectly named, add in this order
      }
      param2 <- c(param2, new_param)
    }
    
    if (length(cells_b) == 0) { # no between-subject factors
      message("no between-subject factors")
      if (length(param) == 1) { 
        param2 <- rep(param, w_n)
      } else if (length(param) != w_n) {
        stop("The number of ", type, 
             " is not correct. Please specify either 1 or a vector of ", 
             w_n, " per cell")
      } else if (setdiff(cells_w, names(param)) %>% length() == 0) {
        param2 <- param[cells_w] # add named parameters in the right order
      } else {
        param2 <- param # parameters are not or incorrectly named, add in this order
      }
    }
  } else if (is.numeric(param)) {
    if (length(param) == 1) { 
      param2 <- rep(param, all_n) 
    } else if (length(param) == all_n) {
      param2 <- param
    } else {
      stop("The number of ", type, " is not correct. Please specify 1, a vector of ", 
           all_n , ", or use the list format")
    }
  }
  
  dd <- matrix(param2, ncol = b_n) %>% as.data.frame()
  names(dd) <- cells_b
  rownames(dd) <- cells_w
  
  dd
}

#' Validate design
#'
#' \code{check_design} validates the specified within and between design
#'
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param cors the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param mu a vector giving the means of the variables (numeric vector of length 1 or vars)
#' @param sd the standard deviations of the variables (numeric vector of length 1 or vars)
#' 
#' @return list
#' 
#' @examples 
#' 
#' within <- list(time = c("day", "night"))
#' between <- list(pet = c("dog", "cat"))
#' design <- check_design(within, between)
#' 
#' @export
#' 
check_design <- function(within = list(), between = list(), 
                         n = 100, cors = 0, mu = 0, sd = 1) {
  # name anonymous factors
  if (is.numeric(within) && within %in% 2:10 %>% mean() == 1) { # vector of level numbers
    within_names <- LETTERS[1:length(within)]
    within <- purrr::map2(within_names, within, ~paste0(.x, 1:.y))
    names(within) <- within_names
  }
  if (is.numeric(between) && between %in% 2:10 %>% mean() == 1) { # vector of level numbers
    between_names <- LETTERS[(length(within)+1):(length(within)+length(between))]
    between <- purrr::map2(between_names, between, ~paste0(.x, 1:.y))
    names(between) <- between_names
  }
  
  # check factor specification
  if (!is.list(within) || !is.list(between)) {
    stop("within and between must be lists")
  } else if (length(within) == 0 && length(between) == 0) {
    stop("You must specify at least one factor")
  }
  
  # if within or between factors are named vectors, 
  # use their names as column names and values as labels for plots
  between_labels <- purrr::map(between, fix_name_labels)
  between <- lapply(between_labels, names)
  within_labels <- purrr::map(within, fix_name_labels)
  within <- lapply(within_labels, names)
  
  within_factors <- names(within)
  between_factors <- names(between)
  
  # handle no w/in or btwn
  if (length(between_factors) == 0) between_factors <- ".tmpvar."
  if (length(within_factors) == 0) within_factors <- ".tmpvar." 
  
  # check for duplicate factor names
  factor_overlap <- intersect(within_factors, between_factors)
  if (length(factor_overlap)) {
    stop("You have multiple factors with the same name (", 
         paste(factor_overlap, collapse = ", "),
         "). Please give all factors unique names.")
  }
  
  # check for duplicate level names within any factor
  dupes <- c(within, between) %>%
    lapply(duplicated) %>%
    lapply(sum) %>%
    lapply(as.logical) %>%
    unlist()
  
  if (sum(dupes)) {
    dupelevels <- c(within, between) %>% 
      names() %>% 
      magrittr::extract(dupes) %>% 
      paste(collapse = ", ")
    stop("You have duplicate levels for factor(s): ", dupelevels)
  }
  
  # define columns
  if (length(within) == 0) {
    cells_w = "val"
  } else {
    cells_w <- do.call(expand.grid, within) %>%
      tidyr::unite("b", 1:ncol(.)) %>% dplyr::pull("b")
  }
  if (length(between) == 0) {
    cells_b = ".tmpvar."
  } else {
    cells_b <- do.call(expand.grid, between) %>%
      tidyr::unite("b", 1:ncol(.)) %>% dplyr::pull("b")
  }
  
  # convert n, mu and sd from vector/list formats
  cell_n <-  convert_param(n, cells_b, cells_w, "Ns")
  cell_mu <- convert_param(mu, cells_b, cells_w, "means")
  cell_sd <- convert_param(sd, cells_b, cells_w, "SDs")
  
  # figure out number of subjects and their IDs
  sub_n <- sum(cell_n[1,])
  sub_id <- make_id(sub_n)
  
  # set up cell correlations from cors (number, vector, matrix or list styles)
  cell_cors <- list()
  if (length(within)) {
    for (cell in cells_b) {
      cell_cor <- if(is.list(cors)) cors[[cell]] else cors
      cell_cors[[cell]] <- cormat(cell_cor, length(cells_w)) 
    }
  }
  
  list(
    within = within,
    between = between,
    within_factors = within_factors,
    between_factors = between_factors,
    within_labels = within_labels,
    between_labels = between_labels,
    cell_n = cell_n,
    cell_mu = cell_mu,
    cell_sd = cell_sd,
    cell_cors = cell_cors,
    cells_w = cells_w,
    cells_b = cells_b,
    sub_id = sub_id
  )
}

#' Simulate Data from Design
#'
#' \code{sim_from_design} generates a dataframe with a specified design
#'
#' @param design A list of design parameters created by check_design()
#' @param empirical logical. If true, mu, sd and cors specify the empirical not population mean, sd and covariance 
#' @param frame_long Whether the returned dataframe is in wide (default = FALSE) or long (TRUE) format
#' 
#' @return dataframe
#' @keywords internal
#' 
sim_design_ <- function(design, empirical = FALSE, frame_long = FALSE) {
  list2env(design, envir = environment())
  
  # simulate data for each between-cell
  for (cell in cells_b) {
    if (length(within)) {
      cell_vars <- rnorm_multi(
        cell_n[1,cell], length(cells_w), cell_cors[[cell]], 
        cell_mu[[cell]], cell_sd[[cell]], cells_w, empirical
      ) %>%
        dplyr::mutate("btwn" = cell)
    } else {
      # fully between design
      val <- MASS::mvrnorm(n = cell_n[1,cell], 
                           mu = cell_mu[[cell]], 
                           Sigma = cell_sd[[cell]] %*% t(cell_sd[[cell]]), 
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
  
  # FIX: put factors in order
  #for (f in between_factors) {
  #  df_wide[[f]] <- factor(df_wide[[f]], levels = between[[f]])
  #}
  
  if (frame_long == TRUE && length(within)) {
    # not necessary for fully between designs
    col_order <- c("sub_id", between_factors, within_factors, "val") %>%
      setdiff(".tmpvar.")
    
    df_long <- df_wide %>%
      tidyr::gather("w_in", "val", tidyselect::one_of(cells_w)) %>%
      tidyr::separate("w_in", within_factors, sep = "_") %>%
      dplyr::select(tidyselect::one_of(col_order)) %>%
      dplyr::mutate_at(within_factors, ~as.factor(.))
    
    #FIX: put factors in order
    #for (f in within_factors) {
    #  df_long[[f]] <- factor(df_long[[f]], levels = within[[f]])
    #}
    
    return(df_long)
  }
  
  df_wide
}
