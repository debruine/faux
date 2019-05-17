#' Validate design
#'
#' \code{check_design} validates the specified within and between design
#'
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param mu a vector giving the means of the variables
#' @param sd the standard deviations of the variables
#' @param r the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param dv the name of the dv column (y)
#' @param id the name of the ID column (id)
#' @param plot whether to show a plot of the design
#' 
#' @return list
#' 
#' @examples 
#' 
#' within <- list(time = c("day", "night"))
#' between <- list(pet = c("dog", "cat"))
#' mu <- list(dog = 10, cat = 5)
#' check_design(within, between, mu = mu)
#' 
#' between <- list(language = c("dutch", "thai"),
#'                 pet = c("dog", "cat"))
#' mu <- list(dutch_dog = 12, thai_dog = 8, dutch_cat = 7, thai_cat = 3)
#' check_design(within, between, mu = mu)
#' @export
#' 
check_design <- function(within = list(), between = list(), 
                         n = 100, mu = 0, sd = 1, r = 0, 
                         dv = "y", id = "id", plot = TRUE) {
  # name anonymous factors
  if (is.numeric(within) && within %in% 2:10 %>% mean() == 1) { # vector of level numbers
    within_names <- LETTERS[1:length(within)]
    within <- purrr::map2(within_names, within, ~paste0(.x, 1:.y))
    names(within) <- within_names
  }
  if (is.numeric(between) && between %in% 2:10 %>% mean() == 1) { # vector of level numbers
    between_names <- LETTERS[(length(within)+1):(length(within)+length(between))]
    between <- purrr::map2(between_names, between, ~paste0(.x, 1:.y))
    names(between) <- between_names
  }
  
  # check factor specification
  if (!is.list(within) || !is.list(between)) {
    stop("within and between must be lists")
  #} else if (length(within) == 0 && length(between) == 0) {
  #  stop("You must specify at least one factor")
  }
  
  # if within or between factors are named vectors, 
  # use their names as column names and values as labels for plots
  between <- purrr::map(between, fix_name_labels)
  within <- purrr::map(within, fix_name_labels)
  
  # check for duplicate factor names
  factor_overlap <- intersect(names(within), names(between))
  if (length(factor_overlap)) {
    stop("You have multiple factors with the same name (", 
         paste(factor_overlap, collapse = ", "),
         "). Please give all factors unique names.")
  }
  
  # check for duplicate level names within any factor
  dupes <- c(within, between) %>%
    lapply(duplicated) %>%
    lapply(sum) %>%
    lapply(as.logical) %>%
    unlist()
  
  if (sum(dupes)) {
    dupelevels <- c(within, between) %>% 
      names() %>% 
      magrittr::extract(dupes) %>% 
      paste(collapse = ", ")
    stop("You have duplicate levels for factor(s): ", dupelevels)
  }
  
  # define columns
  cells_w <- cell_combos(within, dv)
  cells_b <- cell_combos(between, dv) 
  
  # convert n, mu and sd from vector/list formats
  cell_n  <- convert_param(n,  cells_w, cells_b, "Ns")
  cell_mu <- convert_param(mu, cells_w, cells_b, "means")
  cell_sd <- convert_param(sd, cells_w, cells_b, "SDs")
  
  # set up cell correlations from r (number, vector, matrix or list styles)
  cell_r <- list()
  if (length(within)) {
    for (cell in cells_b) {
      cell_cor <- if(is.list(r)) r[[cell]] else r
      mat <- cormat(cell_cor, length(cells_w))
      rownames(mat) <- cells_w
      colnames(mat) <- cells_w
      cell_r[[cell]] <- mat
    }
  }
  
  design <- list(
    within = within,
    between = between,
    dv = dv,
    id = id,
    cells_w = cells_w,
    cells_b = cells_b,
    n = cell_n,
    mu = cell_mu,
    sd = cell_sd,
    r = cell_r
  )
  
  if (plot) { plot_design(design, id, dv) %>% print() }
  
  invisible(design)
}

#' Cell combos
#' 
#' Creates wide cell combination names, such as A1_B1, A2_B1, A1_B2, A2_B2.
#' 
#' @param factors A list of factors
#' @param dv name of dv column ("y")
#' 
#' @return a list
#' @keywords internal
#' 
cell_combos <- function(factors, dv = "y") {
  if (length(factors) == 0) {
    cells = dv
  } else {
    cells <- lapply(factors, names) %>%
      do.call(expand.grid, .) %>%
      tidyr::unite("b", 1:ncol(.)) %>% 
      dplyr::pull("b")
  }
  
  cells
}

#' Convert parameter
#' 
#' Converts parameter specification from vector or list format
#' 
#' @param param the parameter (mu, sd, or n)
#' @param cells_w a list of within-subject cells combinations
#' @param cells_b a list of between-subject cell combinations
#' @param type the name of the parameter (for error messages)
#' 
#' @return a data frame 
#' @keywords internal
#' 
convert_param <- function (param, cells_w, cells_b, type = "this parameter") {
  w_n <- length(cells_w)
  b_n <- length(cells_b)
  all_n <- b_n*w_n
  
  if (is.matrix(param)) { param <- as.data.frame(param) }
  
  if (is.data.frame(param)) { # convert to list first
    # check for row/column confusion
    cols_are_b <- setdiff(colnames(param), cells_b) %>% length() == 0
    rows_are_w <- setdiff(rownames(param), cells_w) %>% length() == 0
    cols_are_w <- setdiff(colnames(param), cells_w) %>% length() == 0
    rows_are_b <- setdiff(rownames(param), cells_b) %>% length() == 0
    if (cols_are_b && rows_are_w) {
      # check this first in case rows and cols are the same labels
      param <- as.list(param) %>%  lapply(magrittr::set_names, rownames(param))
    } else if (cols_are_w && rows_are_b) {
      param <- t(param) %>% as.data.frame()
      param <- as.list(param) %>%  lapply(magrittr::set_names, rownames(param))
    } else {
      stop("The ", type, " data table is misspecified.")
    }
  }
  
  if (is.list(param)) {
    param2 <- c()
    # add param in right order
    for (f in cells_b) {
      if (length(param[[f]]) == 1) { 
        new_param <- rep(param[[f]], w_n)
      } else if (length(param[[f]]) != w_n) {
        stop("The number of ", type, " for cell ", f, 
             " is not correct. Please specify either 1 or a vector of ", 
             w_n, " per cell")
      } else if (setdiff(cells_w, names(param[[f]])) %>% length() == 0) {
        new_param <- param[[f]][cells_w] # add named parameters in the right order
      } else {
        new_param <- param[[f]] # parameters are not or incorrectly named, add in this order
      }
      param2 <- c(param2, new_param)
    }
    
    if (length(cells_b) == 0) { # no between-subject factors
      if (length(param) == 1) { 
        param2 <- rep(param, w_n)
      } else if (length(param) != w_n) {
        stop("The number of ", type, 
             " is not correct. Please specify either 1 or a vector of ", 
             w_n, " per cell")
      } else if (setdiff(cells_w, names(param)) %>% length() == 0) {
        param2 <- param[cells_w] # add named parameters in the right order
      } else {
        param2 <- param # parameters are not or incorrectly named, add in this order
      }
    }
  } else if (is.numeric(param)) {
    if (length(param) == 1) { 
      param2 <- rep(param, all_n) 
    } else if (length(param) == all_n) {
      param2 <- param
    } else {
      stop("The number of ", type, " is not correct. Please specify 1, a vector of ", 
           all_n , ", or use the list format")
    }
  }
  
  dd <- matrix(param2, ncol = b_n)
  colnames(dd) <- cells_b
  rownames(dd) <- cells_w
  
  t(dd) %>% as.data.frame()
}
