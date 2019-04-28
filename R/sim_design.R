#' Generate Data from Design (NOT DONE!!!)
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
  # error checking
  if (!is.list(within) || !is.list(between)) {
    stop("within and between must be lists")
  } else if (length(within) == 0 && length(between) == 0) {
    stop("You must specify at least one factor")
  }
  
  # if within or between factors are named vectors, 
  # use their names as column names and values as labels for plots
  fix_name_labels <- function(x) {
    if (is.null(names(x))) { names(x) <- x }
    nm <- names(x)
    # get rid of non-word characters and underscores because they mess up separate
    names(x) <- gsub("(\\W|_)", ".", nm) 
    x
  }
  between_labels <- purrr::map(between, fix_name_labels)
  between <- lapply(between_labels, names)
  within_labels <- purrr::map(within, fix_name_labels)
  within <- lapply(within_labels, names)
  
  
  # TODO: make sure all levels are unique within factors
  
  # define columns and factors
  within_factors <- names(within)
  between_factors <- names(between)
  cells_w <- do.call(expand.grid, within) %>%
    tidyr::unite("b", 1:ncol(.)) %>% dplyr::pull("b")
  cells_b <- do.call(expand.grid, between) %>%
    tidyr::unite("b", 1:ncol(.)) %>% dplyr::pull("b")
  cells_all <- do.call(expand.grid, c(within, between)) %>%
    tidyr::unite("b", 1:ncol(.)) %>% dplyr::pull("b")
  
  # convert n, mu and sd from vector/list formats
  cell_n <-  get_mu_sd(n, cells_b, cells_w, "Ns")
  cell_mu <- get_mu_sd(mu, cells_b, cells_w, "means")
  cell_sd <- get_mu_sd(sd, cells_b, cells_w, "SDs")
  
  between_combos <- length(cells_b)
  within_combos <- length(cells_w)
  
  # figure out number of subjects and their IDs
  sub_n <- sum(cell_n[1,])
  max_digits <- floor(log10(sub_n))+1
  sub_id <- paste0("S",formatC(1:sub_n, width = max_digits, flag = "0"))
  
  # set up cell correlations from cors (number, vector, matrix or list styles)
  cell_cors <- list()
  for (cell in cells_b) {
    cell_cor <- if(is.list(cors)) cors[[cell]] else cors
    cell_cors[[cell]] <- cormat(cell_cor, within_combos) 
  }
  
  # simulate data for each between-cell
  for (cell in cells_b) {
    cell_vars <- rnorm_multi(cell_n[1,cell], within_combos, cell_cors[[cell]], 
                             cell_mu[[cell]], cell_sd[[cell]], 
                             cells_w, empirical) %>%
      dplyr::mutate("btwn" = cell)
    
    # add cell values to df
    if (cell == cells_b[1]) { 
      df <- cell_vars # first cell sets up the df
    } else {
      df <- dplyr::bind_rows(df, cell_vars)
    }
  }
  
  # set column order
  col_order <- c("sub_id", between_factors, cells_w)
  
  # create wide dataframe
  df_wide <- df %>%
    tidyr::separate("btwn", between_factors, sep = "_") %>%
    dplyr::mutate("sub_id" = sub_id) %>%
    dplyr::select(tidyselect::one_of(col_order))
  
  if (frame_long == TRUE) {
    col_order <- c("sub_id", between_factors, within_factors, "val")
    
    df_long <- df_wide %>%
      tidyr::gather("w_in", "val", tidyselect::one_of(cells_w)) %>%
      tidyr::separate("w_in", within_factors, sep = "_") %>%
      dplyr::select(tidyselect::one_of(col_order))
    
    return(df_long)
  }
  
  df_wide
}


#' Convert mu or sd
#' 
#' Checks mu and sd specification from vector or list format
#' 
#' @param param the mu or sd
#' @param cells_b a list of between-subject cell combinations
#' @param cells_w a list of within-subject cells combinations
#' @param type the name of the parameter (for error messages)
#' 
#' @return a data frame 
#' 
get_mu_sd <- function (param, cells_b, cells_w, type = "this parameter") {
  w_n <- length(cells_w)
  b_n <- length(cells_b)
  all_n <- b_n*w_n
  
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
