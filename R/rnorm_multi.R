#' Multiple Normally Distributed Vectors
#'
#' \code{rnorm_multi} makes multiple normally distributed vectors with specified relationships
#'
#' @param n the number of samples required
#' @param vars the number of variables to return
#' @param cors the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param mu a vector giving the means of the variables (numeric vector of length 1 or vars)
#' @param sd the standard deviations of the variables (numeric vector of length 1 or vars)
#' @param varnames optional names for the variables (string vector of length vars) defaults if cors is a matrix with column names
#' @param empirical logical. If true, mu, sd and cors specify the empirical not population mean, sd and covariance 
#' @param as.matrix logical. If true, returns a matrix
#' 
#' @return dataframe of vars vectors
#' @examples
#' rnorm_multi(100, 3, c(0.2, 0.4, 0.5), varnames=c("A", "B", "C"))
#' rnorm_multi(100, 3, c(1, 0.2, -0.5, 0.2, 1, 0.5, -0.5, 0.5, 1), varnames=c("A", "B", "C"))
#' @export

rnorm_multi <- function(n, vars = 3, cors = 0, mu = 0, sd = 1, 
                       varnames = NULL, 
                       empirical = FALSE, 
                       as.matrix = FALSE) {
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
  }
  
  if (length(sd) == 1) {
    sd <- rep(sd, vars)
  } else if (length(sd) != vars) {
    stop("the length of sd must be 1 or vars");
  }
  
  cor_mat <- cormat(cors, vars)
  
  sigma <- (sd %*% t(sd)) * cor_mat
  bvn <- MASS::mvrnorm(n, mu, sigma, empirical = empirical)
  df <- data.frame(bvn)
  
  if (length(varnames) == vars) {
    colnames(bvn) <- varnames
  } else if (!is.null(colnames(cor_mat))) {
    # if cors was a matrix with names, use that
    colnames(bvn) <- colnames(cor_mat)
  }
  
  if (as.matrix) bvn else data.frame(bvn)
}

