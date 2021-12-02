#' Convert r for NORTA
#' 
#' Given a target r-value, returns the correlation you need to induce in a bivariate normal distribution to have the target correlation after converting distributions. 
#' 
#' See \link[stats:Distributions]{Distributions} for distributions and their various arguments to specify in params1 and params2.
#'
#' @param target_r The target correlation
#' @param dist1 The target distribution function for variable 1 (e.g., norm, binom, gamma, truncnorm, cauchy)
#' @param dist2 The target distribution function for variable 2
#' @param params1 Arguments to pass to the functions for distribution 1
#' @param params2 Arguments to pass to the functions for distribution 2
#' @param package1 The package that contains the r{dist} and q{dist} functions for dist1 
#' @param package2 The package that contains the r{dist} and q{dist} functions for dist2 
#'
#' @return r-value to induce in the bivariate normal variables
#' @export
#'
#' @examples
#' convert_r(target_r = 0.5, 
#'           dist1 = "norm", 
#'           dist2 = "binom", 
#'           params1 = list(mean = 100, sd = 10),
#'           params2 = list(size = 1, prob = 0.5))
#'           
#' convert_r(target_r = 0.5, 
#'           dist1 = "norm", 
#'           dist2 = "likert", 
#'           params1 = list(mean = 100, sd = 10),
#'           params2 = list(prob = c(5, 10, 20, 30, 20)))
convert_r <- function(target_r = 0,
                      dist1 = "norm", 
                      dist2 = "norm",
                      params1 = list(), 
                      params2 = list(),
                      package1 = "stats",
                      package2 = "stats") {
  if (target_r == 0) return(0)
  
  if (dist1 == "truncnorm") package1 = "truncnorm"
  if (dist2 == "truncnorm") package2 = "truncnorm"
  if (dist1 == "likert") package1 = "faux"
  if (dist2 == "likert") package2 = "faux"
  
  # check if r and q functions exist
  tryCatch({
    rfunc1 <- utils::getFromNamespace(paste0("r", dist1), package1)
    qfunc1 <- utils::getFromNamespace(paste0("q", dist1), package1)
  }, error = function(e) {
    stop(dist1, " isn't a valid distribution")
  })
  
  tryCatch({
    rfunc2 <- utils::getFromNamespace(paste0("r", dist2), package2)
    qfunc2 <- utils::getFromNamespace(paste0("q", dist2), package2)
  }, error = function(e) {
    stop(dist2, " isn't a valid distribution")
  })
  
  # generate target distributions
  params1$n <- 1e6
  D1 <- sort(do.call(rfunc1, params1)) %>% as.numeric()
  params2$n <- 1e6
  D2 <- sort(do.call(rfunc2, params2)) %>% as.numeric()
  params1$n <- NULL
  params2$n <- NULL
  
  # check if target_r is possible
  max_r <- cor(D1, D2)
  min_r <- cor(D1, rev(D2))
  if (target_r > max_r) {
    warning("Maximum target_r is ", round(max_r, 3))
    return(NA)
  }
  if (target_r < min_r) {
    warning("Minimum target_r is ", round(min_r, 3))
    return(NA)
  }
  
  # define function
  f <- function(r) {
    # simulate bivariate normal with specified r
    mvn <- rnorm_multi(5e4, 2, r = r, empirical = TRUE)
    params1$p <- stats::pnorm(mvn$X1, 0, 1)
    params2$p <- stats::pnorm(mvn$X2, 0, 1)
    
    # convert dist1 & dist2 
    # make numeric in case likert
    X1 <- do.call(qfunc1, params1) %>% as.numeric()
    X2 <- do.call(qfunc2, params2) %>% as.numeric()
    
    # check new correlation
    conv_r <- cor(X1, X2)
    
    # compare to target r (goal to minimise this)
    abs(conv_r - target_r)
  }
  
  # optimise to find converted r
  if (target_r > 0) min_r <- 0; max_r <- 0.99
  if (target_r < 0) max_r <- 0; min_r <- -.99
  opt <- stats::optimise(f, interval = c(min_r, max_r), tol = .001)
  
  # check if found
  if (abs(opt$objective) > .01) {
    warning("The target_r could not be matched any closer than ",
            round(opt$objective, 3))
  }
  
  opt$minimum
}
