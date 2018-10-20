#' Simulate from a Multivariate Normal Distribution with One Pre-Existing Vector
#'
#' \code{multirnorm} Produces one or more samples from the specified multivariate normal distribution, based on one pre-existing vector
#'
#' @param n the number of smaples required
#' @param mu a vector giving the means of the variables
#' @param Sigma a positive-definite symmetric matrix specifying the covariance matrix of the variables.
#' @param empirical logical. If true, mu and Sigma specify the empirical not population mean and covariance 
#' 
#' @return matrix
#' @examples
#' @export

mvrnormpre <- function (pre = NULL, n = 1, mu, Sigma, tol = 1e-06, 
                        empirical = FALSE) {
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) 
    stop("incompatible arguments")
  if (length(pre) != n)
    stop("pre must have length n")
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L]))) 
    stop("'Sigma' is not positive definite")
  X <- matrix(rnorm(p * n), n)
  X[,1] <- pre # set first column to predefined vector
  if (empirical) {
    X <- scale(X, TRUE, FALSE)
    X <- X %*% svd(X, nu = 0)$v
    X <- scale(X, FALSE, TRUE)
  }
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% 
    t(X)
  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) 
    nm <- dn[[1L]]
  dimnames(X) <- list(nm, NULL)
  if (n == 1) 
    drop(X)
  else t(X)
}


# test
pre = rnorm(100)
tol = 1e-06
n = 100
mu = c(0, 0, 0)
sd = c(1, 1, 1)
cor_mat = matrix(c(1, -.2, 0.3, 
                   .5, 1, .5, 
                   .5, .5, 1), 3)
Sigma = (sd %*% t(sd)) * cor_mat

pp <- mvrnormpre(pre = pre, n, mu, Sigma)
