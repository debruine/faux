#' Simulate an existing dataframe
#'
#' \code{simdf} Produces a dataframe with the same distributions and correlations as an existing dataframe. Only returns numeric columns and simulates all numeric variables from a continuous normal distribution (for now).
#'
#' @param dat the existing dataframe
#' @param n the number of samples to return per group
#' @param grp_by an optional list of column names to group by
#' @param empirical logical. Passed on to multirnorm
#' 
#' @return tibble
#' @examples
#' iris100 <- simdf(iris, 100)
#' iris_species <- simdf(iris, 100, "Species")
#' @export

simdf <- function (dat, n=100, grp_by=NULL, empirical = FALSE) {
  # error checking
  if ( !is.numeric(n) || n %% 1 > 0 || n < 3 ) {
    stop("n must be an integer > 2")
  }
  
  grpdat <- select_num_grp(dat, grp_by)
  
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
