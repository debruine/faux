#' Make unique pairs of level names for correlations
#'
#' @param v a vector of level names or a number of levels
#'
#' @return a vector of all unique pairs
#' @export
#'
#' @examples
#' unique_pairs(c("O", "C", "E", "A", "N"))
#' unique_pairs(3)
unique_pairs <- function(v) {
  if (is.numeric(v)) { v <- LETTERS[1:v] }
  if (length(v) < 2) { stop("There must be at least 2 levels") }
  if (duplicated(v) %>% sum()) { stop("You have duplicate levels") }
  v <- factor(v, levels = v)
  
  expand.grid(a = v, b = v) %>% 
    dplyr::filter(a != b) %>% t() %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate_all(factor, levels = v) %>%
    dplyr::mutate_all(sort) %>% t() %>%
    tibble::as_tibble() %>% 
    tidyr::unite(combo, 1:2, sep = "-") %>%
    dplyr::distinct(combo) %>%
    dplyr::pull(combo)
}


#' Set design interactively
#'
#' @return list
#' @export
#'
#' @examples
#' des <- interactive_design()
interactive_design <- function() {
  wn <- readline_check(prompt="How many within-subject factors do you have?: ", "integer")
  within <- list()
  if (wn > 0) {
    for (i in 1:wn) {
      p <- paste0("Name of within-subject factor ", i, ": ")
      name <- readline(prompt=p)
      
      p <- paste0("How many levels of ", name, ": ")
      nlevels <- readline_check(prompt=p, "integer")
      
      levels <- c()
      for (j in 1:nlevels) {
        p <- paste0("Name of factor ", name, ", level ", j, ": ")
        levels[j] <- readline(prompt=p)
      }
      
      within[[name]] <- levels
    }
  }
  
  bn <- readline_check(prompt="How many between-subject factors do you have?: ", "integer")
  between <- list()
  if (bn > 0) {
    for (i in 1:bn) {
      p <- paste0("Name of between-subject factor ", i, ": ")
      name <- readline(prompt=p)
      
      p <- paste0("How many levels of ", name, ": ")
      nlevels <- readline_check(prompt=p, "integer")
      
      levels <- c()
      for (j in 1:nlevels) {
        p <- paste0("Name of factor ", name, ", level ", j, ": ")
        levels[j] <- readline(prompt=p)
      }
      between[[name]] <- levels
    }
  }
  
  within <- purrr::map(within, faux:::fix_name_labels)
  between <- purrr::map(between, faux:::fix_name_labels)
  
  cells_w <- faux:::cell_combos(within)
  cells_b <- faux:::cell_combos(between)
  
  up <- unique_pairs(cells_w)
  r <- purrr::map(cells_b, function (b){
    cell_r <- purrr::map_dbl(up, function(x) {
      if (b == "val") {
        p <- paste0("Correlation for ", x, ": ")
      } else {
        p <- paste0("Correlation for ", x, " in between-subject condition ", b, " : ")
      }
      readline_check(prompt=p, "numeric")
    })
    
    if (!is_pos_def(cormat_from_triangle(cell_r))) {
      warning("That correlation matrix is not possible")
    }
    cell_r
  }) %>% magrittr::set_names(cells_b)
  
  n <- purrr::map(cells_b, function(b) {
    if (length(between)) {
      p <- paste0("Number of subjects for condition ", b, ": ")
    } else {
      p <- "Number of subjects: "
    }
    n <- readline_check(prompt=p, "integer")
  }) %>% magrittr::set_names(cells_b)
  
  g <- expand.grid(w = cells_w, b = cells_b)
  mu <- purrr::map2_dbl(g$b, g$w, function(b, w) {
    if (w == "val") {
      p <- paste0("Mean of between-subject condition ", b, ": ")
    } else if (b == "val") {
      p <- paste0("Mean of within-subject cell ", w, ": ")
    } else {
      p <- paste0("Mean of within-subject cell ", w, " in between-subject condition ", b, ": ")
    }
    readline_check(prompt=p, "numeric")
  }) %>% matrix(nrow = length(cells_b), dimnames = list(cells_b, cells_w)) %>%
    as.data.frame()
  
  sd <- purrr::map2_dbl(g$b, g$w, function(b, w) {
    if (w == "val") {
      p <- paste0("SD of between-subject condition ", b, ": ")
    } else if (b == "val") {
      p <- paste0("SD of within-subject cell ", w, ": ")
    } else {
      p <- paste0("SD of within-subject cell ", w, " in between-subject condition ", b, ": ")
    }
    readline_check(prompt=p, "numeric")
  }) %>% matrix(nrow = length(cells_b), dimnames = list(cells_b, cells_w)) %>%
    as.data.frame()
  
  list(
    within = within, 
    between = between, 
    n = n, 
    mu = mu, 
    sd = sd, 
    r = r
  )
  
  check_design(within, between, n, mu, sd, r, plot = FALSE)
}


#' Check readline input
#'
#' @param prompt the prompt for readline
#' @param type what type of check to perform, one of c("numeric", "character", "length", "minlength", "maxlength", "exact", "grep")
#' @param compare the comparator for exact and (min|max)length types
#' @param ... other arguments to pass to grep
#'
#' @return the validated result of readline
#' @export
#'
#' @examples
#' readline_check("Type a number: ", "numeric")
#' readline_check("Type two characters: ", "length", 2)
#' readline_check("Type at least 3 characters: ", "minlength", 3)
#' readline_check("Type no more than 4 characters: ", "maxlength", 4)
#' readline_check("Type a letter and a number: ", "grep", pattern = "^[a-zA-Z]\\d$")
#' 
readline_check <- function(prompt, type = c("numeric", "integer", "length", "minlength", "maxlength", "exact", "grep"), compare = NULL,  ...) {
  input <- readline(prompt)
  if (type == "numeric") {
    warn_text <- "The input must be a number: "
    check <- suppressWarnings(!is.na(as.numeric(input)))
    input <- suppressWarnings(as.numeric(input))
  } else if (type == "integer") {
    warn_text <- "The input must be an integer: "
    check = grep("^\\d+$", input) %>% length() > 0
    input <- suppressWarnings(as.integer(input))
  } else if (type == "exact") {
    warn_text <- paste("The input must be", exact, ": ")
    check <- input == compare
  } else if (type == "length") {
    warn_text <- paste("The input must be", compare, "characters long: ")
    check <- nchar(input) == compare
  } else if (type == "minlength") {
    warn_text <- paste("The input must be at least", compare, "characters long: ")
    check <- nchar(input) >= compare
  } else if (type == "maxlength") {
    warn_text <- paste("The input must be no more than", compare, "characters long: ")
    check <- nchar(input) <= compare
  } else if (type == "grep") {
    warn_text <- "The input is incorrect: "
    check <- grep(x = input, ...) %>% length() > 0
  } else {
    warn_text <- "The input is incorrect: "
    check <- FALSE # default false if type is wrong?
  }
  if (!check) {
    readline_check(warn_text, type[1], compare, ...)
  } else {
    input
  }
}

