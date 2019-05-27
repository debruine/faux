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
    
    # rotate/expand to dataframe with cols = cells_b and rows = cells_w 
    if (cols_are_b && rows_are_w) {
      # check this first in case rows and cols are the same labels
    } else if (cols_are_w && rows_are_b) {
      param <- t(param) %>% as.data.frame()
    } else if (cols_are_b && nrow(param) == 1) {
      # duplicate rows for each cells_w
      param <- t(param) %>% as.data.frame()
      names(param)[1] <- ".tempvar."
      for (col in cells_w) {
        param[col] <- param[,1]
      }
      param[,1] <- NULL
      param <- t(param) %>% as.data.frame()
    } else if (rows_are_b && ncol(param) == 1) {
      names(param)[1] <- ".tempvar."
      for (col in cells_w) {
        param[col] <- param[,1]
      }
      param[,1] <- NULL
      param <- t(param) %>% as.data.frame()
    } else if (rows_are_w && ncol(param) == 1) {
      for (col in cells_b) {
        param[col] <- param[,1]
      }
      param[,1] <- NULL
    } else if (cols_are_w && nrow(param) == 1) {
      param <- t(param) %>% as.data.frame()
      names(param)[1] <- ".tempvar."
      for (col in cells_b) {
        param[col] <- param[,1]
      }
      param[,1] <- NULL
    } else {
      stop("The ", type, " data table is misspecified.")
    }
    # convert to named list with names = cells_b
    param <- as.list(param) %>% lapply(magrittr::set_names, rownames(param))
  }
  
  if (is.list(param)) {
    param2 <- c()
    # add param in right order
    for (f in cells_b) {
      if (!(f %in% names(param))) {
        stop("Cell ", f, " is not specified for ", type)
      } else if (length(param[[f]]) == 1) { 
        new_param <- rep(param[[f]], w_n)
      } else if (length(param[[f]]) != w_n) {
        stop("The number of ", type, " for cell ", f, 
             " is not correct. Please specify either 1 or a vector of ", 
             w_n, " per cell")
      } else if (setdiff(cells_w, names(param[[f]])) %>% length() == 0) {
        # add named parameters in the right order
        new_param <- param[[f]][cells_w] 
      } else {
        # parameters are not or incorrectly named, add in this order
        new_param <- param[[f]]
      }
      param2 <- c(param2, new_param)
    }
    
    # if (length(cells_b) == 0) { # no between-subject factors
    #   if (length(param) == 1) { 
    #     param2 <- rep(param, w_n)
    #   } else if (length(param) != w_n) {
    #     stop("The number of ", type, 
    #          " is not correct. Please specify either 1 or a vector of ", 
    #          w_n, " per cell")
    #   } else if (setdiff(cells_w, names(param)) %>% length() == 0) {
    #     param2 <- param[cells_w] # add named parameters in the right order
    #   } else {
    #     param2 <- param # parameters are not or incorrectly named, add in this order
    #   }
    # }
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
  
  dd <- matrix(param2, ncol = max(1, b_n))
  colnames(dd) <- cells_b
  rownames(dd) <- cells_w
  
  # all-list version
  dd <- dd %>% as.data.frame() %>% as.list()
  lapply(dd, function(x) { names(x) <- cells_w; as.list(x) } )
  
  # data frame version
  #t(dd) %>% as.data.frame()
}

