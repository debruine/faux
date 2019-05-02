#' Simulate an existing dataframe
#'
#' \code{sim_df} Produces a dataframe with the same distributions and correlations as an existing dataframe. Only returns numeric columns and simulates all numeric variables from a continuous normal distribution (for now).
#'
#' @param dat the existing dataframe
#' @param n the number of samples to return per group
#' @param between a list of the between-subject factors
#' @param within a list of the within-subject factors
#' @param empirical logical. Passed on to rnorm_multi
#' 
#' @return tibble
#' @examples
#' iris100 <- sim_df(iris, 100)
#' iris_species <- sim_df(iris, 100, "Species")
#' @export

sim_df <- function (dat, n = 100, between = c(), within = c(), empirical = FALSE) {
  # error checking
  if ( !is.numeric(n) || n %% 1 > 0 || n < 3 ) {
    stop("n must be an integer > 2")
  }
  
  grpdat <- select_num_grp(dat, between)
  
  simdat <- grpdat %>%
    tidyr::nest() %>%
    dplyr::mutate(newsim = purrr::map(data, function(data) {
      rnorm_multi(
        n = n, 
        vars = ncol(data), 
        cor = stats::cor(data),
        mu = t(dplyr::summarise_all(data, mean)), 
        sd = t(dplyr::summarise_all(data, stats::sd)), 
        varnames = names(data),
        empirical = empirical
      )
    })) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(newsim) %>%
    dplyr::ungroup()
  
  return(simdat)
}
