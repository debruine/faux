#' Multiple Normally Distributed Vectors
#'
#' \code{rnorm_multi()} makes multiple normally distributed vectors with specified relationships.
#'
#' @param n the number of samples required
#' @param vars the number of variables to return
#' @param mu a vector giving the means of the variables (numeric vector of length 1 or vars)
#' @param sd the standard deviations of the variables (numeric vector of length 1 or vars)
#' @param r the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param varnames optional names for the variables (string vector of length vars) defaults if r is a matrix with column names
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' @param as.matrix logical. If true, returns a matrix
#' @param cors (deprecated; use r)
#' 
#' @return a tbl of vars vectors
#' 
#' @examples
#' rnorm_multi(100, 3, 0, 1, c(0.2, 0.4, 0.5), varnames=c("A", "B", "C"))
#' rnorm_multi(100, 3, 0, 1, c(1, 0.2, -0.5, 0.2, 1, 0.5, -0.5, 0.5, 1), varnames=c("A", "B", "C"))
#' 
#' @export

rnorm_multi <- function(n, vars = 3, mu = 0, sd = 1, r = 0,
                       varnames = NULL, 
                       empirical = FALSE, 
                       as.matrix = FALSE, 
                       cors = NULL) {
  if (!is.null(cors)) {
    warning("cors is deprecated, please use r")
    if (r == 0) r = cors # set r to cors if r is not set
  }
  
  # error handling
  if ( !is.numeric(n) || n %% 1 > 0 || n < 3 ) {
    stop("n must be an integer > 2")
  }
  
  if (!(empirical  %in% c(TRUE, FALSE))) {
    stop("empirical must be TRUE or FALSE")
  }
  
  if (length(mu) == 1) {
    mu <- rep(mu, vars)
  } else if (length(mu) != vars) {
    stop("the length of mu must be 1 or vars");
  } else {
    # get rid of names
    mu <- as.matrix(mu) %>% as.vector()
  }
  
  if (length(sd) == 1) {
    sd <- rep(sd, vars)
  } else if (length(sd) != vars) {
    stop("the length of sd must be 1 or vars");
  } else {
    # get rid of names
    sd <- as.matrix(sd) %>% as.vector()
  }
  
  cor_mat <- cormat(r, vars)
  
  sigma <- (sd %*% t(sd)) * cor_mat
  bvn <- MASS::mvrnorm(n, mu, sigma, empirical = empirical)
  df <- data.frame(bvn)
  
  if (length(varnames) == vars) {
    colnames(bvn) <- varnames
  } else if (!is.null(colnames(cor_mat))) {
    # if r was a matrix with names, use that
    colnames(bvn) <- colnames(cor_mat)
  }
  
  if (as.matrix) bvn else data.frame(bvn)
}

