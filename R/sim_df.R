#' Simulate an existing dataframe
#'
#' \code{sim_df} Produces a data table with the same distributions and correlations as an existing data table Only returns numeric columns and simulates all numeric variables from a continuous normal distribution (for now).
#'
#' @param .data the existing tbl (must be in wide format)
#' @param n the number of samples to return per group
#' @param between a list of the between-subject columns
#' @param empirical logical. Passed on to rnorm_multi
#' @param seed a single value, interpreted as an integer, or NULL (see set.seed)
#' @param grp_by (deprecated; use between)
#' 
#' @return a tbl
#' @examples
#' iris100 <- sim_df(iris, 100)
#' iris_species <- sim_df(iris, 100, between = "Species")
#' @export

sim_df <- function (.data, n = 100, between = c(), 
                    empirical = FALSE, seed = NULL, grp_by = NULL) {
  set.seed(seed)
  
  # error checking
  if ( !is.numeric(n) || n %% 1 > 0 || n < 3 ) {
    stop("n must be an integer > 2")
  }
  
  if (!is.null(grp_by)) {
    warning("grp_by is deprecated, please use between")
    if (between == c()) between = grp_by # set between to grp_by if between is not set
  }
  
  grpdat <- select_num_grp(.data, between)
  
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
