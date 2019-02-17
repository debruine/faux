#' Multiple Normally Distributed Vectors
#'
#' \code{rnorm_multi} makes multiple normally distributed vectors with specified relationships
#'
#' @param n the number of samples required
#' @param vars the number of variables to return
#' @param cors the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param mu a vector giving the means of the variables (numeric vector of length 1 or vars)
#' @param sd the standard deviations of the variables (numeric vector of length 1 or vars)
#' @param varnames optional names for the variables (string vector of length vars)
#' @param empirical logical. If true, mu, sd and cors specify the empirical not population mean, sd and covariance 
#' 
#' @return dataframe of vars vectors
#' @examples
#' rnorm_multi(100, 3, c(0.2, 0.4, 0.5), varnames=c("A", "B", "C"))
#' rnorm_multi(100, 3, c(1, 0.2, -0.5, 0.2, 1, 0.5, -0.5, 0.5, 1), varnames=c("A", "B", "C"))
#' @export

rnorm_multi <- function(n, vars = 3, cors = 0, mu = 0, sd = 1, 
                       varnames = NULL, empirical = FALSE) {
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
  
  # correlation matrix
  if (class(cors) == "numeric" & length(cors) == 1) {
    if (cors >=-1 & cors <=1) {
      cors = rep(cors, vars*(vars-1)/2)
    } else {
      stop("cors must be between -1 and 1")
    }
  }
  
  if (class(cors) == "matrix") { 
    if (!is.numeric(cors)) {
      stop("cors matrix not numeric")
    } else if (dim(cors)[1] != vars || dim(cors)[2] != vars) {
      stop("cors matrix wrong dimensions")
    } else if (sum(cors == t(cors)) != (nrow(cors)^2)) {
      stop("cors matrix not symmetric")
    } else {
      cor_mat <- cors
    }
  } else if (length(cors) == vars*vars) {
    cor_mat <- matrix(cors, vars)
  } else if (length(cors) == vars*(vars-1)/2) {
    # generate full matrix from vector of upper right triangle
    
    cor_mat <- matrix(nrow=vars, ncol = vars)
    upcounter = 1
    lowcounter = 1
    for (col in 1:vars) {
      for (row in 1:vars) {
        if (row == col) {
          # diagonal
          cor_mat[row, col] = 1
        } else if (row > col) {
          # lower left triangle
          cor_mat[row, col] = cors[lowcounter]
          lowcounter <- lowcounter + 1
        }
      }
    }
    for (row in 1:vars) {
      for (col in 1:vars) {
        if (row < col) {
          # upper right triangle
          cor_mat[row, col] = cors[upcounter]
          upcounter <- upcounter + 1
        }
      }
    }
  }
  
  # check matrix is positive definite
  tol <- 1e-08
  ev <- eigen(cor_mat, only.values = TRUE)$values
  if (sum(ev < tol)) {
    stop("correlation matrix not positive definite")
  }
  
  sigma <- (sd %*% t(sd)) * cor_mat
  bvn <- MASS::mvrnorm(n, mu, sigma, empirical = empirical)
  df <- data.frame(bvn)
  
  if (length(varnames) == vars) {
    names(df) <- varnames
  }
  
  df
}

