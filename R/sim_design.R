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
  
  # create empty data frame with correct structure
  between_combos <- lapply(between, length) %>% unlist() %>% prod()
  within_combos <- lapply(within, length) %>% unlist() %>% prod()
  
  sub_n <- n * between_combos
  max_digits <- floor(log10(sub_n))+1
  sub_ids <- paste0("S",formatC(1:sub_n, width = max_digits, flag = "0"))
  
  df <- do.call(expand.grid, c(within, list(sub_id = 1:n), between)) %>%
    tidyr::unite("w", 1:(length(within))) %>%
    dplyr::mutate(var = 0) %>%
    tidyr::spread(w, var, convert = TRUE) %>%
    dplyr::mutate(sub_id = sub_ids)
  
  # populate with correlated vars
  w_vars <- rnorm_multi(sub_n, within_combos, cors, mu, sd, NULL, empirical)
  
  start <- ncol(df) - within_combos + 1
  end <- ncol(df)
  df[, start:end] <- w_vars
  
  df
}
