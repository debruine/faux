#' Validate design
#'
#' \code{check_design} validates the specified within and between design
#'
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param mu a vector giving the means of the variables (numeric vector of length 1 or vars)
#' @param sd the standard deviations of the variables (numeric vector of length 1 or vars)
#' @param r the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' 
#' @return list
#' 
#' @examples 
#' 
#' within <- list(time = c("day", "night"))
#' between <- list(pet = c("dog", "cat"))
#' check_design(within, between)
#' 
#' @export
#' 
check_design <- function(within = list(), between = list(), 
                         n = 100, mu = 0, sd = 1, r = 0) {
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
  } else if (length(within) == 0 && length(between) == 0) {
    stop("You must specify at least one factor")
  }
  
  # if within or between factors are named vectors, 
  # use their names as column names and values as labels for plots
  between_labels <- purrr::map(between, fix_name_labels)
  between <- lapply(between_labels, names)
  within_labels <- purrr::map(within, fix_name_labels)
  within <- lapply(within_labels, names)
  
  within_factors <- names(within)
  between_factors <- names(between)
  
  # handle no w/in or btwn
  if (length(between_factors) == 0) between_factors <- ".tmpvar."
  if (length(within_factors) == 0) within_factors <- ".tmpvar." 
  
  # check for duplicate factor names
  factor_overlap <- intersect(within_factors, between_factors)
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
  if (length(within) == 0) {
    cells_w = "val"
  } else {
    cells_w <- do.call(expand.grid, within) %>%
      tidyr::unite("b", 1:ncol(.)) %>% dplyr::pull("b")
  }
  if (length(between) == 0) {
    cells_b = ".tmpvar."
  } else {
    cells_b <- do.call(expand.grid, between) %>%
      tidyr::unite("b", 1:ncol(.)) %>% dplyr::pull("b")
  }
  
  # convert n, mu and sd from vector/list formats
  cell_n  <- convert_param(n,  cells_w, cells_b, "Ns")
  cell_mu <- convert_param(mu, cells_w, cells_b, "means")
  cell_sd <- convert_param(sd, cells_w, cells_b, "SDs")
  
  # figure out number of subjects and their IDs
  sub_n <- sum(cell_n[1,])
  sub_id <- make_id(sub_n)
  
  # set up cell correlations from r (number, vector, matrix or list styles)
  cell_r <- list()
  if (length(within)) {
    for (cell in cells_b) {
      cell_cor <- if(is.list(r)) r[[cell]] else r
      cell_r[[cell]] <- cormat(cell_cor, length(cells_w)) 
    }
  }
  
  list(
    within = within,
    between = between,
    within_factors = within_factors,
    between_factors = between_factors,
    within_labels = within_labels,
    between_labels = between_labels,
    cells_w = cells_w,
    cells_b = cells_b,
    cell_n = cell_n,
    cell_mu = cell_mu,
    cell_sd = cell_sd,
    cell_r = cell_r,
    sub_id = sub_id
  )
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
  
  if (is.data.frame(param)) { # convert to list first
    # check for row/column confusion
    cols_are_b <- setdiff(names(param), cells_b) %>% length() == 0
    rows_are_w <- setdiff(rownames(param), cells_w) %>% length() == 0
    cols_are_w <- setdiff(names(param), cells_w) %>% length() == 0
    rows_are_b <- setdiff(rownames(param), cells_b) %>% length() == 0
    if (cols_are_b && rows_are_w) {
      # check this first in case rows and cols are the same labels
      param <- as.list(param) %>%  lapply(magrittr::set_names, rownames(param))
    } else if (cols_are_w && rows_are_b) {
      param <- t(param) %>% as.data.frame()
      param <- as.list(param) %>%  lapply(magrittr::set_names, rownames(param))
    } else {
      stop("The ", type, " dataframe is misspecified.")
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
      message("no between-subject factors")
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
  
  dd <- matrix(param2, ncol = b_n) %>% as.data.frame()
  names(dd) <- cells_b
  rownames(dd) <- cells_w
  
  dd
}


#' Get design from long data
#' 
#' Makes a best guess at the design of a long-format data frame. 
#' Finds all columns that contain a single value per unit of analysis (between factors), 
#' all columns that contain the same values per unit of analysis (within factors), and 
#' all columns that differ over units of analysis (dv, continuous factors)
#' 
#' @param .data the data frame (in long format)
#' @param id the column name(s) that identify a unit of analysis
#' @param dv the column name that identifies the DV
#' 
#' @return the data frame in long format
#' 
#' @export
#'
get_design_long <- function(.data, id = "sub_id", dv = "val") {
  between_factors <- .data %>%
    dplyr::group_by_at(dplyr::vars(tidyselect::one_of(id))) %>%
    dplyr::summarise_all(dplyr::n_distinct) %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::one_of(id)) %>%
    dplyr::summarise_all(max) %>%
    dplyr::select_if(~ . == 1) %>%
    names()
  
  within_factors <- .data %>%
    dplyr::select(-tidyselect::one_of(between_factors)) %>%
    dplyr::group_by_at(dplyr::vars(tidyselect::one_of(id))) %>%
    dplyr::summarise_all(paste0, collapse = ",") %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::one_of(id)) %>%
    dplyr::summarise_all(dplyr::n_distinct) %>%
    dplyr::select_if(~ . == 1) %>%
    names()
  
  within <- .data %>%
    dplyr::select(tidyselect::one_of(within_factors)) %>%
    dplyr::mutate_all(as.factor) %>%
    dplyr::summarise_all(~levels(.) %>% paste0(collapse = ".|.")) %>%
    as.list() %>%
    sapply(strsplit, split=".|.", fixed = TRUE)
  
  between <- .data %>%
    dplyr::select(tidyselect::one_of(between_factors)) %>%
    dplyr::mutate_all(as.factor) %>%
    dplyr::summarise_all(~levels(.) %>% paste0(collapse = ".|.")) %>%
    as.list() %>%
    sapply(strsplit, split=".|.", fixed = TRUE)
  
  between_labels <- purrr::map(between, fix_name_labels)
  within_labels <- purrr::map(within, fix_name_labels)
  
  cells_b <- do.call(expand.grid, between) %>%
    tidyr::unite("b", 1:ncol(.)) %>% dplyr::pull("b")
  
  cells_w <- do.call(expand.grid, within) %>%
    tidyr::unite("b", 1:ncol(.)) %>% dplyr::pull("b")
  
  # get n, mu, sd, r per cell
  chk <- check_sim_stats(.data, between_factors, within_factors, dv, id)
  
  n <- chk %>%
    tidyr::unite(".within", tidyselect::one_of(between_factors)) %>%
    dplyr::select(.within, var, n) %>%
    tidyr::spread(var, n) %>%
    tibble::column_to_rownames(".within")
  
  mu <- chk %>%
    tidyr::unite(".within", tidyselect::one_of(between_factors)) %>%
    dplyr::select(.within, var, mean) %>%
    tidyr::spread(var, mean) %>%
    tibble::column_to_rownames(".within")
  
  sd <- chk %>%
    tidyr::unite(".within", tidyselect::one_of(between_factors)) %>%
    dplyr::select(.within, var, sd) %>%
    tidyr::spread(var, sd) %>%
    tibble::column_to_rownames(".within")
  
  cors <- chk %>%
    tidyr::unite(".between", tidyselect::one_of(between_factors)) %>%
    dplyr::select(tidyselect::one_of(c(".between", "var", cells_w))) %>%
    dplyr::mutate(var = forcats::fct_relevel(var, cells_w)) %>%
    dplyr::arrange(var) %>%
    dplyr::group_by(.between) %>%
    tidyr::nest(.key = "r") %>%
    as.list() 
  
  r <- purrr::map(cors$r, ~tibble::column_to_rownames(., "var"))
  names(r) <- cors$.between
   
  design <- list(
    within = within,
    between = between,
    within_factors = within_factors,
    between_factors = between_factors,
    within_labels = within_labels,
    between_labels = between_labels,
    cells_w = cells_w,
    cells_b = cells_b,
    cell_n = n,
    cell_mu = mu,
    cell_sd = sd,
    cell_r = r,
    sub_id = id
  )
  
  design
}

