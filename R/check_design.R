#' Validates the specified design
#' 
#' Specify any number of within- and between-subject factors with any number of 
#' levels. Specify n for each between-subject cell; mu and sd for each cell, and
#' r for the within-subject cells for each between-subject cell.
#' 
#' This function returns a validated design list for use in sim_design_ to 
#' simulate a data table with this design, or to archive your design.
#' 
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param mu a vector giving the means of the variables
#' @param sd the standard deviations of the variables
#' @param r the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param dv the name of the DV column list(y = "Score")
#' @param id the name of the ID column list(id = "Subject ID")
#' @param plot whether to show a plot of the design
#' @param design a design list including within, between, n, mu, sd, r, dv, id
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
#' mu <- list(dutch_dog = 12, dutch_cat = 7, thai_dog = 8, thai_cat = 3)
#' check_design(within, between, mu = mu)
#' @export
#' 
check_design <- function(within = list(), between = list(), 
                         n = 100, mu = 0, sd = 1, r = 0, 
                         dv = list(y = "Score"), 
                         id = list(id = "Subject ID"), 
                         plot = TRUE, design = NULL) {
  # design passed as design list
  if (!is.null(design)) {
    # double-check the entered design
    list2env(design, envir = environment())
  } else if ("design" %in% class(within)) {
    # design given as first argument: not ideal but handle it
    list2env(within, envir = environment())
  }
  
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
  }
  
  # if within or between factors are named vectors, 
  # use their names as column names and values as labels for plots
  between <- purrr::map(between, fix_name_labels)
  within <- purrr::map(within, fix_name_labels)
  dv <- fix_name_labels(dv, "\\W")
  id <- fix_name_labels(id, "\\W")
  
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
  cells_w <- cell_combos(within, names(dv))
  cells_b <- cell_combos(between, names(dv)) 
  
  # convert n, mu and sd from vector/list formats
  cell_n  <- convert_param(n,  cells_w, cells_b, "Ns")
  for (i in names(cell_n)) {
    cell_n[[i]] <- cell_n[[i]][[1]]
  }
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
    n = cell_n,
    mu = cell_mu,
    sd = cell_sd,
    r = cell_r
  )
  
  class(design) <- c("design", "list")
  
  if (plot) { plot_design(design) %>% print() }
  
  invisible(design)
}

