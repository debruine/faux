#' Simulate an existing dataframe
#'
#' \code{sim_df} Produces a dataframe with the same distributions and correlations as an existing dataframe. Only returns numeric columns and simulates all numeric variables from a continuous normal distribution (for now).
#'
#' @param dat the existing dataframe (must be in wide format)
#' @param n the number of samples to return per group
#' @param between a list of the between-subject columns
#' @param empirical logical. Passed on to rnorm_multi
#' @param grp_by (deprecated; use between)
#' 
#' @return tibble
#' @examples
#' iris100 <- sim_df(iris, 100)
#' iris_species <- sim_df(iris, 100, between = "Species")
#' @export

sim_df <- function (dat, n = 100, between = c(), empirical = FALSE, grp_by = NULL) {
  # error checking
  if ( !is.numeric(n) || n %% 1 > 0 || n < 3 ) {
    stop("n must be an integer > 2")
  }
  
  if (!is.null(grp_by)) {
    warning("grp_by is deprecated, please use between")
    if (between == c()) between = grp_by # set between to grp_by if between is not set
  }
  
  grpdat <- select_num_grp(dat, between)
  
  simdat <- grpdat %>%
    tidyr::nest() %>%
    dplyr::mutate(newsim = purrr::map(data, function(data) {
      rnorm_multi(
        n = n, 
        vars = ncol(data), 
        mu = t(dplyr::summarise_all(data, mean)), 
        sd = t(dplyr::summarise_all(data, stats::sd)), 
        r = stats::cor(data),
        varnames = names(data),
        empirical = empirical
      )
    })) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(newsim) %>%
    dplyr::ungroup()
  
  return(simdat)
}
