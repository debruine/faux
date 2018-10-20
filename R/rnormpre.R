#' Correlated Normal Vector
#'
#' \code{rnormpre} Produces a random normally distributed vector with the specified correlation to an existing vector
#'
#' @param x the existing vector
#' @param rho desired correlation between existing and returned vectors
#' @param ymean desired mean of returned vector
#' @param ysd desired SD of returned vector
#' 
#' @return vector
#' @examples
#' v1 <- rnorm(10)
#' v2 <- rnormpre(v1, 0.5, 0, 1)
#' cor(v1, v2)
#' @export

rnormpre <- function (x, rho=0, ymean=0, ysd=1) {
  n <- length(x)
  y <- stats::rnorm(n)
  z <- rho * scale(x)[,1] + sqrt(1 - rho^2) * 
    scale(stats::resid(lm(y ~ x)))[,1]
  yresult <- ymean + ysd * z
  yresult
}
