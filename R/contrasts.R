#' Contrast code a factor
#' 
#' Contrast coding sets the grand mean as the intercept. 
#' Each contrast compares one level with the reference level (base).
#'
#' @param fct the factor to contrast code (or a vector)
#' @param levels the levels of the factor in order
#' @param base the index of the level to use as baseline
#'
#' @return the factor with contrasts set
#' @export
#'
#' @examples
#' df <- sim_design(between = list(pet = c("cat", "dog")), 
#'                  mu = c(10, 20), plot = FALSE)
#' df$pet <- contrast_code(df$pet)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df <- sim_design(between = list(pet = c("cat", "dog", "ferret")), 
#'                  mu = c(2, 4, 9), empirical = TRUE, plot = FALSE)
#'                  
#' df$pet <- contrast_code(df$pet, base = 1)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- contrast_code(df$pet, base = 2)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- contrast_code(df$pet, base = "ferret")
#' lm(y ~ pet, df) %>% broom::tidy()
contrast_code <- function(fct, levels = NULL, base = 1) {
  # make sure fct is a factor with correct levels
  if (is.null(levels)) {
    levels <- levels(fct)
  } else {
    fct <- factor(fct, levels)
  }
  
  # create coding matrix
  n <- length(levels)
  if (!is.numeric(base)) base <- which(levels == base)
  tr_code <- stats::contr.treatment(n, base)
  ef_code <- matrix(rep(1/n, n*(n-1)), ncol=(n-1))
  my_code <- tr_code - ef_code
  
  # create column names
  colnames <- paste0("_", levels[-base], ".vs.", levels[base])
  dimnames(my_code) <- list(levels, colnames)
  
  # set contrast and return factor
  stats::contrasts(fct) <- my_code
  fct
}


#' Sum code a factor
#' 
#' Sum coding sets the grand mean as the intercept. 
#' Each contrast compares one level with the grand mean.
#'
#' @param fct the factor to contrast code (or a vector)
#' @param levels the levels of the factor in order
#' @param omit the level to omit (defaults to the last level)
#'
#' @return the factor with contrasts set
#'
#' @return
#' @export
#'
#' @examples
#' df <- sim_design(between = list(pet = c("cat", "dog", "ferret")), 
#'                  mu = c(2, 4, 9), empirical = TRUE, plot = FALSE)
#' 
#' df$pet <- sum_code(df$pet)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- sum_code(df$pet, omit = "cat")
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- sum_code(df$pet, omit = 1)
#' lm(y ~ pet, df) %>% broom::tidy()
sum_code <- function(fct, levels = NULL, omit = length(levels)) {
  # make sure fct is a factor with correct levels
  if (is.null(levels)) {
    levels <- levels(fct)
  } else {
    fct <- factor(fct, levels)
  }
  
  # create coding matrix
  n <- length(levels)
  if (!is.numeric(omit)) omit <- which(levels == omit)
  my_code <- stats::contr.sum(n)
  if (n != omit) {
    reorder <- c(setdiff(1:n, omit), omit)
    my_code <- my_code[reorder, , drop = FALSE]
  }
  
  # create column names
  colnames <- paste0("_", levels[-omit], ".vs.intercept")
  dimnames(my_code) <- list(levels, colnames)
  
  # set contrast and return factor
  stats::contrasts(fct) <- my_code
  fct
}



#' Treatment code a factor
#' 
#' Treatment coding sets the mean of the reference group as the intercept. 
#' Each contrast compares one level with the reference level (base).
#'
#' @param fct the factor to contrast code (or a vector)
#' @param levels the levels of the factor in order
#' @param base the index of the level to use as baseline
#'
#' @return the factor with contrasts set
#' @export
#'
#' @examples
#' df <- sim_design(between = list(pet = c("cat", "dog")), 
#'                  mu = c(10, 20), plot = FALSE)
#' df$pet <- treatment_code(df$pet)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df <- sim_design(between = list(pet = c("cat", "dog", "ferret")), 
#'                  mu = c(2, 4, 9), empirical = TRUE, plot = FALSE)
#'                  
#' df$pet <- treatment_code(df$pet)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- treatment_code(df$pet, base = 2)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- treatment_code(df$pet, base = "ferret")
#' lm(y ~ pet, df) %>% broom::tidy()
treatment_code <- function(fct, levels = NULL, base = 1) {
  # make sure fct is a factor with correct levels
  if (is.null(levels)) {
    levels <- levels(fct)
  } else {
    fct <- factor(fct, levels)
  }
  
  # create coding matrix
  n <- length(levels)
  if (!is.numeric(base)) base <- which(levels == base)
  my_code <- stats::contr.treatment(n, base)
  
  # create column names
  colnames <- paste0("_", levels[-base], ".vs.", levels[base])
  dimnames(my_code) <- list(levels, colnames)
  
  # set contrast and return factor
  stats::contrasts(fct) <- my_code
  fct
}



#' Helmert code a factor
#' 
#' Helmert coding sets the grand mean as the intercept. 
#' Each contrast compares one level with the mean of previous levels.
#'
#' @param fct the factor to contrast code (or a vector)
#' @param levels the levels of the factor in order
#'
#' @return the factor with contrasts set
#' @export
#'
#' @examples
#' df <- sim_design(between = list(pet = c("cat", "dog")), 
#'                  mu = c(10, 20), plot = FALSE)
#' df$pet <- helmert_code(df$pet)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df <- sim_design(between = list(pet = c("cat", "dog", "ferret")), 
#'                  mu = c(2, 4, 9), empirical = TRUE, plot = FALSE)
#'                  
#' df$pet <- helmert_code(df$pet)
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- helmert_code(df$pet, levels = c("dog", "cat", "ferret"))
#' lm(y ~ pet, df) %>% broom::tidy()
#' 
#' df$pet <- helmert_code(df$pet, levels = c("ferret", "dog", "cat"))
#' lm(y ~ pet, df) %>% broom::tidy()
helmert_code <- function(fct, levels = NULL) {
  # make sure fct is a factor with correct levels
  if (is.null(levels)) {
    levels <- levels(fct)
  } else {
    fct <- factor(fct, levels)
  }
  
  # create coding matrix
  n <- length(levels)
  my_code <- stats::contr.helmert(n)
  for (i in 1:(n-1)) {
    my_code[, i] <- my_code[, i] / (i + 1)
  }
  
  # create column names
  comparison <- lapply(1:(n-1), function(x) {
    paste(levels[1:x], collapse = ".")
  })
  colnames <- paste0("_", levels[-1], ".vs.", comparison)
  dimnames(my_code) <- list(levels, colnames)
  
  # set contrast and return factor
  stats::contrasts(fct) <- my_code
  fct
}

