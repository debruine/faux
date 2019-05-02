#' Correlated Normal Vector
#'
#' \code{rnorm_pre} Produces a random normally distributed vector with the specified correlation to an existing vector
#'
#' @param x the existing vector
#' @param mu desired mean of returned vector
#' @param sd desired SD of returned vector
#' @param r desired correlation between existing and returned vectors
#' 
#' @return vector
#' @examples
#' v1 <- rnorm(10)
#' v2 <- rnorm_pre(v1, 0, 1, 0.5)
#' cor(v1, v2)
#' @export

rnorm_pre <- function (x, mu=0, sd=1, r=0) {
  # error checking
  if (!is.vector(x)) stop("x must be a vector") 
  if (!is.numeric(x)) stop("x must be numeric")
  if (length(x) < 3) stop("x must have length > 2")
  
  n <- length(x)
  y <- stats::rnorm(n)
  z <- r * scale(x)[,1] + sqrt(1 - r^2) * 
    scale(stats::resid(stats::lm(y ~ x)))[,1]
  yresult <- mu + sd * z
  
  return(yresult)
}
