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
#' @param plot whether to show a plot of the design
#'
#' @return list
#' @export
#'
#' @examples
#' des <- interactive_design()
interactive_design <- function(plot = FALSE) {
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
  
  # ask for N
  blist <- paste(cells_b, collapse = ", ")
  int_pattern <- paste0("^\\s?(\\d+\\s?,\\s?){0,", (length(cells_b)-1), "}\\s?(\\d+)\\s?$")
  
  if (length(between)) {
    p <- paste0("Number of subjects in conditions ", blist, ": ")
  } else {
    p <- "Number of subjects: "
  }
  n <- readline_check(prompt=p, "grep", pattern = int_pattern, perl = TRUE) %>%
    strsplit("\\s?,\\s?") %>% unlist() %>% as.integer() 
  if (length(n) == 1) n <- rep(n, length(cells_b))
  names(n) <- cells_b
  n <- as.list(n)
  
  wlist <- paste(cells_w, collapse = ", ")
  pattern <- paste0("^\\s?(-?\\d*\\.?\\d?\\s?,\\s?){0,", (length(cells_w)-1), "}\\s?(-?\\d*\\.?\\d?)\\s?$")
  
  # ask for mu
  mu <- purrr::map(cells_b, function(b) {
    p <- paste0("Means of ", wlist, " in condition ", b, ": ")
    mm <- readline_check(prompt=p, "grep", pattern = pattern, perl = TRUE) %>%
      strsplit("\\s?,\\s?") %>% unlist()
    if (length(mm) == 1) mm <- rep(mm, length(cells_w))
    mm
  }) %>% unlist() %>% as.double() %>%
    matrix(nrow = length(cells_w), dimnames = list(cells_w, cells_b)) %>%
    as.data.frame()

  # ask for SD
  sd <- purrr::map(cells_b, function(b) {
    p <- paste0("SDs of ", wlist, " in condition ", b, ": ")
    mm <- readline_check(prompt=p, "grep", pattern = pattern, perl = TRUE) %>%
      strsplit("\\s?,\\s?") %>% unlist()
    if (length(mm) == 1) mm <- rep(mm, length(cells_w))
    mm
  }) %>% unlist() %>% as.double() %>%
    matrix(nrow = length(cells_w), dimnames = list(cells_w, cells_b)) %>%
    as.data.frame()
  
  # ask for r
  up <- unique_pairs(cells_w)
  uplist <- paste(up, collapse = ", ")
  pattern <- paste0("^\\s?(-?\\d*\\.?\\d?\\s?,\\s?){0,", (length(up)-1), "}\\s?(-?\\d*\\.?\\d?)\\s?$")
  r <- purrr::map(cells_b, function(b) {
    p <- paste0("Cors (r) of ", uplist, " in condition ", b, ": ")
    mm <- readline_check(prompt=p, "grep", pattern = pattern, perl = TRUE) %>%
      strsplit("\\s?,\\s?") %>% unlist()
    if (length(mm) == 1) mm <- rep(mm, length(cells_w))
    mm %>% as.double()
  })
  names(r) <- cells_b
  
  message("All done, your data is ready!")

  check_design(within, between, n, mu, sd, r, plot = plot)
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
    warn_text <- paste("The input must be", compare, ": ")
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

