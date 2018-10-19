#' Multiple Normally Distributed Vectors
#'
#' \code{multirnorm} makes multiple normally distributed vectors with specified relationships
#'
#' @param n the number of observations per variable
#' @param vars the number of variables to return
#' @param cors the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param mu the means of the variables (can be a vector of length 1 or vars)
#' @param sd the standard deviations of the variables (can be a vector of length 1 or vars)
#' @param varnames optional names for the variables (string vector of length vars)
#' @param empirical are the above parameters for the population (FALSE) or the sample (TRUE)
#' 
#' @return dataframe of vars vectors
#' @examples
#' multirnorm(100, 3, c(0.2, -0.5, 0.5), varnames=c("A", "B", "C"))
#' #' multirnorm(100, 3, c(1, 0.2, -0.5, 0.2, 1, 0.5, -0.5, 0.5, 1), varnames=c("A", "B", "C"))
#' @export

multirnorm <- function(n, vars = 3, cors = 0, mu = 0, sd = 1, 
                       varnames = NULL, empirical = FALSE) {
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
    if (cors >=0 & cors <=1) {
      cors = rep(cors, vars*(vars-1)/2)
    } else {
      stop("cors must be between 0 and 1")
    }
  }
  
  if (class(cors) == "matrix") { 
    if (mean(dim(cors) == c(vars,vars)) == 1) {
      cor_mat <- cors
    } else {
      stop("matrix badly specified")
    }
  } else if (length(cors) == vars*vars) {
    cor_mat <- matrix(cors, vars)
  } else if (length(cors) == vars*(vars-1)/2) {
    cor_mat <- matrix(nrow=vars, ncol = vars)
    upcounter = 1
    lowcounter = 1
    for (col in 1:vars) {
      for (row in 1:vars) {
        if (row == col) {
          # diagoal
          cor_mat[row, col] = 1
        } else if (row < col) {
          # upper right triangle
          cor_mat[row, col] = cors[upcounter]
          upcounter <- upcounter + 1
        } else {
          # lower left triangle
          cor_mat[row, col] = cors[lowcounter]
          lowcounter <- lowcounter + 1
        }
      }
    }
  }
  
  sigma <- (sd %*% t(sd)) * cor_mat
  bvn <- MASS::mvrnorm(n, mu, sigma, empirical = empirical)
  df <- data.frame(bvn)
  
  if (length(varnames) == vars) {
    names(df) <- varnames
  }
  
  df
}

