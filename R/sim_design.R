#' Generate Data from Design (NOT DONE!!!)
#'
#' \code{sim_design} generates a dataframe with a sepcified within and between design
#'
#' @param n the number of samples required
#' @param cors the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param mu a vector giving the means of the variables (numeric vector of length 1 or vars)
#' @param sd the standard deviations of the variables (numeric vector of length 1 or vars)
#' @param empirical logical. If true, mu, sd and cors specify the empirical not population mean, sd and covariance 
#' @param interactive 
#' 
#' @return dataframe
#' 
sim_design <- function(within = list(), between = list(), 
                       n = 100, cors = 0, mu = 0, sd = 1, 
                       empirical = FALSE, interactive = FALSE) {
  # error checking
  if (!is.list(within) || !is.list(between)) {
    stop("within and between must be lists")
  } else if (length(within) == 0 && length(between) == 0) {
    stop("You must specify at least one factor")
  }
  
  # TODO: make sure all levels are unique within factors
  
  # define columns and factors
  cells_w <- do.call(expand.grid, within) %>%
    tidyr::unite("b", 1:ncol(.)) %>% dplyr::pull("b")
  cells_b <- do.call(expand.grid, between) %>%
    tidyr::unite("b", 1:ncol(.)) %>% dplyr::pull("b")
  cells_all <- do.call(expand.grid, c(within, between)) %>%
    tidyr::unite("b", 1:ncol(.)) %>% dplyr::pull("b")
  
  # create empty data frame with correct structure
  between_combos <- length(cells_b)
  within_combos <- length(cells_w)
  
  sub_n <- n * between_combos
  max_digits <- floor(log10(sub_n))+1
  sub_id <- paste0("S",formatC(1:sub_n, width = max_digits, flag = "0"))
  
  # convert mu and sd from vector/list formats
  mu2 <- get_mu_sd(mu, cells_b, cells_w, "means")
  sd2 <- get_mu_sd(sd, cells_b, cells_w, "SDs")
  cors2 <- list()
  for (cell in cells_b) {
    cell_cor <- if(is.list(cors)) cors[[cell]] else cors
    cors2[[cell]] <- cormat(cell_cor, within_combos) 
  }
  
  for (cell in cells_b) {
    cell_vars <- rnorm_multi(n, within_combos, cors2[[cell]], 
                          mu2[[cell]], sd2[[cell]], 
                          cells_w, empirical) %>%
      dplyr::mutate("btwn" = cell)
    
    if (cell == cells_b[1]) { 
      df1 <- cell_vars 
    } else {
      df1 <- dplyr::bind_rows(df1, cell_vars)
    }
  }
  
  col_order <- c("sub_id", names(between), cells_w)
  
  df <- df1 %>%
    tidyr::separate("btwn", names(between)) %>%
    dplyr::mutate("sub_id" = sub_id) %>%
    dplyr::select(tidyselect::one_of(col_order))
  
  df
}


#' Convert mu or sd
#' 
#' Checks mu and sd specification from vector or list format
#' 
#' @param param the mu or sd
#' @param cells_b a list of between-subject cell combinations
#' @param cells_w a list of within-subject cells combinations
#' 
#' @return a data frame 
#' 
get_mu_sd <- function (param, cells_b, cells_w, type = "mean") {
  w_n <- length(cells_w)
  b_n <- length(cells_b)
  all_n <- b_n*w_n
  
  if (is.list(param)) {
    param2 <- c()
    # add param in right order
    for (f in cells_b) {
      if (length(param[[f]]) == 1) { 
        param[[f]] <- rep(param[[f]], w_n)
      } else if (length(param[[f]]) != w_n) {
        stop("The number of ", type, " for cell ", f, 
             " is not correct. Please specify either 1 or a vector of ", 
             w_n, " per cell")
      }
      param2 <- c(param2, param[[f]])
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
