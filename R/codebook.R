#' #' Create PsychDS Codebook from Data
#' #'
#' #' @param data The data frame to generate a codebook for
#' #' @param coldesc Optional named list of column descriptions
#' #' @param as_json Whether the output should be a list or JSON format
#' #' @param interactive Whether the function should prompt the user to describe columns and factor levels
#' #'
#' #' @return a list or json-formatted codebook
#' #' @export
#' #'
#' #' @examples
#' #' 
#' #' data <- sim_design(2, 2, long = TRUE)
#' #' cb <- codebook(data, c("A" = "Factor 1", "B" = "Factor 2"))
#' codebook <- function(data, coldesc = NULL, as_json = TRUE, interactive = FALSE) {
#'   vm <- list()
#'   colnames <- names(data)
#'   if (is.null(coldesc)) {
#'     # set column descriptions to the same as column names
#'     coldesc <- colnames
#'     names(coldesc) <- colnames
#'   }
#'   
#'   for (i in 1:ncol(data)) {
#'     col <- colnames[i]
#'     
#'     if (interactive) {
#'       # ask user for the column description
#'       p <- paste0("Full name of column ", col, ": ")
#'       coldesc[col] <- readline_check(prompt=p, "length", min = 1)
#'     } else if (is.na(coldesc[col])) {
#'       # column not defined in coldesc
#'       coldesc[col] <- col
#'     }
#'     
#'     vm[[i]] <- list(
#'       type = "PropertyValue",
#'       unitText = colnames[i],
#'       name = coldesc[[col]]
#'     )
#'     
#'     vm[[i]]$missingValues <- is.na(data[[i]]) %>% sum()
#'     
#'     if (is.numeric(data[[i]])) {
#'       vm[[i]]$minValue <- min(data[[i]], na.rm = TRUE)
#'       vm[[i]]$maxValue <- max(data[[i]], na.rm = TRUE)
#'       vm[[i]]$meanValue <- mean(data[[i]], na.rm = TRUE)
#'       vm[[i]]$sdValue <- sd(data[[i]], na.rm = TRUE)
#'     } else if (is.factor(data[[i]])) {
#'       lvls <- levels(data[[i]])
#'       names(lvls) <- lvls
#'       if (interactive) {
#'         for (lname in lvls) {
#'           p <- paste0(colnames[i], ", level ", lname, ": ")
#'           lvls[lname] <- readline_check(prompt=p, "length", min = 1)
#'         }
#'       }
#'       
#'       vm[[i]]$levels <- lvls
#'     }
#'   }
#'   
#'   schema <- list(
#'     "@type" = "Dataset",
#'     schemaVersion = "Psych-DS 0.1.0",
#'     variableMeasured = vm
#'   )
#'   
#'   if (isTRUE(as_json)) {
#'     schema <- schema %>%
#'       jsonlite::toJSON(auto_unbox = TRUE) %>%
#'       jsonlite::prettify(4)
#'   }
#'   
#'   return(schema)
#' }


#' Create PsychDS Codebook from Data
#'
#' @param data The data frame to generate a codebook for
#' @param name The name of this dataset (if NULL, will be the same as `data`)
#' @param vardesc Optional variable properties in the format of a named list of vectors (can be named or unnamed and in the same order as the data) from the options description, privacy, type, propertyID, minValue, maxValue, levels, ordered, na, naValues, alternateName, unitCode
#' @param ... Further dataset properties (e.g., description, license, author, citation, funder, url, doi/sameAs, keywords, temporalCoverage, spatialCoverage, datePublished, dateCreated)
#' @param schemaVersion defaults to "Psych-DS 0.1.0"
#' @param as_json Whether the output should be a list or JSON format
#' @param interactive Whether the function should prompt the user to describe columns and factor levels
#'
#' @return a list or json-formatted codebook
#' @export
#'
#' @examples
#'
#' vardesc = list(
#'   description = c("Length of the sepal",
#'                   "Width of the sepal",
#'                   "Length of the petal",
#'                   "Width of the petal",
#'                   "The flower species"),
#'   type = c("float", "float", "float", "float", "string")
#' )
#' codebook(iris, vardesc = vardesc)
#'
codebook <- function(data, name = NULL, vardesc = list(), ...,
                     schemaVersion = "Psych-DS 0.1.0", 
                     as_json = TRUE, interactive = FALSE) {
  # use PsychDS format from https://docs.google.com/document/d/1u8o5jnWk0Iqp_J06PTu5NjBfVsdoPbBhstht6W0fFp0/edit
  
  if (is.null(name)) {
    name <- utils::capture.output(match.call()$data)
  }
  
  # all possible type abbreviations ----
  types <- c(i = "int", 
             s = "string", 
             d = "float", 
             f = "factor", 
             b = "bool",
             int = "int", 
             string = "string", 
             double = "float", 
             factor = "factor", 
             bool = "bool",
             integer = "int", 
             char = "string", 
             character = "string", 
             float = "float", 
             boolean = "bool",
             logical = "bool")
  
  schema <- list(
    "@context" = "https://schema.org/",
    "@type" = "Dataset",
    name = name,
    schemaVersion = schemaVersion
  )
  
  # check datadesc
  datadesc <- list(...)
  
  # fix doi and relabel as sameAs ----
  if (!is.null(datadesc$doi)) {
    doi <- tolower(datadesc$doi) %>%
      gsub("\\s", "", .) %>%
      sub("^doi\\:", "", .) %>%
      sub("^https://doi.org/", "", .)
    datadesc$doi <- NULL
    datadesc$sameAs <- paste0("https://doi.org/", doi)
  }
  
  possible_vals <- c("license", "author", "citation", "funder", "url", "sameAs", "keywords", "temporalCoverage", "spatialCoverage", "datePublished", "dateCreated")
  non_standard <- setdiff(names(datadesc), possible_vals)
  if (length(non_standard) > 0) {
    warning("The following dataset properties are not standard: ",
            paste(non_standard, collapse = ", "), "\n")
  }
  
  # add data properties ----
  schema <- c(schema, datadesc)
  
  # get vardesc from design
  design <- attr(data, "design")
  if (!is.null(design)) {
    win <- ifelse(length(design$within) > 0, 
                  names(design$within), "")
    if (win %in% names(data)) {
      # in long format
      lvls <-  c(design$between, design$within)
      descs <- design$id
    } else {
      lvls <- design$between
      descs <- c(design$id, design$dv)
    }
    
    # overwrite levels and descriptions in design with vardesc
    if (length(lvls) > 0) { 
      # only run this if there are any factor columns
      for (n in names(vardesc$levels)) {
        lvls[[n]] <- as.list(vardesc$levels[[n]])
      }
      vardesc$levels <- lvls
    }
    for (n in names(vardesc$description)) {
      descs[[n]] <- vardesc$description[[n]]
    }
    vardesc$description <- descs
  }
  
  # check vardesc ----
  possible_vals <- c("description", "privacy", "type",
                     "propertyID", "minValue", "maxValue",
                     "levels", "ordered", "na", "naValues",
                     "alternateName", "unitCode")
  
  non_standard <- setdiff(names(vardesc), possible_vals)
  if (length(non_standard) > 0) {
    warning("The following variable properties are not standard: ",
            paste(non_standard, collapse = ", "), "\n")
  }
  
  # TODO: add more validation ----
  
  # make variableMeasured ----
  vm <- list()
  colnames <- names(data)
  
  for (i in 1:ncol(data)) {
    col <- colnames[i]
    
    # @type and name ----
    vm[[i]] <- list(
      `@type` = "PropertyValue",
      name = col,
      description = col # default to be replaced from vardesc
    )
    
    # set variable attributes from vardesc ----
    for (vd in names(vardesc)) {
      vals <- vardesc[[vd]]
      
      if (!is.null(names(vals))) {
        # set from named (if available)
        if (col %in% names(vals)) vm[[i]][vd] <- vals[col]
      } else if (length(vals) == ncol(data)) {
        # set from position
        vm[[i]][vd] <- vals[i]
      } else {
        warning("Couldn't set ", vd, " for ", col)
      }
    }
    
    # set type if not specified ---
    if (!is.null(vm[[i]]$type)) {
      if ((vm[[i]]$type %in% names(types))) {
        vm[[i]]$type <- types[[vm[[i]]$type]]
      } else {
        # not a valid type, so set to null and get from data
        warning(vm[[i]]$name, " does not have a valid type (",
                vm[[i]]$type, ")")
        vm[[i]]$type <- NULL
      }
    }
    
    if (is.null(vm[[i]]$type)) {
      vm[[i]]$type <- dplyr::case_when(
        is.factor(data[[i]]) ~ "factor",
        is.character(data[[i]]) ~ "string",
        is.integer(data[[i]]) ~ "int",
        is.numeric(data[[i]]) ~ "float",
        is.logical(data[[i]]) ~ "bool",
        TRUE ~ typeof(data[[i]])
      )
      if (faux_options("verbose")) {
        message(vm[[i]]$name, " set to type ", vm[[i]]$type)
      }
    }
    
    # get levels for factors if not specified ----
    if (vm[[i]]$type == "factor") {
      if (is.null(vm[[i]]$levels)) {
        lvls <- levels(data[[i]])
        names(lvls) <- lvls
        vm[[i]]$levels <- lvls
      }
      
      if (is.null(vm[[i]]$ordered)) {
        vm[[i]]$ordered <- is.ordered(data[[i]])
      }
    }
  }
  
  schema$variableMeasured <- vm
  
  if (isTRUE(interactive)) {
    schema <- interactive_codebook(data, schema)
  }
  
  if (isTRUE(as_json)) {
    schema <- schema %>%
      jsonlite::toJSON(auto_unbox = TRUE) %>%
      jsonlite::prettify(4)
  } else {
    class(schema) <- c("psychds_codebook", "list")
  }
  
  return(schema)
}

#' Interactive Codebook
#'
#' @param data The data frame to generate a codebook for
#' @param cb The codebook in list format if already generated
#'
#' @return
#' @export
#'
#' @examples
interactive_codebook <- function(data, cb = NULL) {
  if (is.null(cb)) {
    # run codebook function to get best guess
    name <- utils::capture.output(match.call()$data)
    cb <- codebook(data, name, as_json = FALSE)
  }
  
  types <- c(i = "int", s = "string", d = "float", f = "factor", b = "bool")
  type_q <- "Is it an integer (i), string/character (s), double/float (d), factor (f), or boolean/T-F (b)?"
  
  for (i in 1:length(cb$variableMeasured)) {
    v <- cb$variableMeasured[[i]]
    vals <- unique(data[[v$name]])
    if (length(vals) > 5) {
      if (is.numeric(vals)) {
        range <- paste(min(vals), "to", max(vals))
      } else {
        range <- sprintf("e.g., %s", paste(vals[1:5], collapse = ","))
      }
    } else {
      range <- paste(vals, collapse = ",")
    }
    
    txt <- sprintf("Is %s a %s? It has %d unique values (%s) [y/n]",
                   v$name, v$type, length(vals), range)
    
    x <- faux::readline_check(txt, "grep",
                              pattern = "^(y|n|i|s|d|f|b)$",
                              warning = "Enter only y or n (enter i, s, d, f or b to set the data type)") %>%
      substr(1, 1) %>% tolower()
    
    if (x == "y") {
      type <- v$type
    } else if (x %in% c("i", "d", "f", "s")) {
      type <- types[x]
    } else {
      # ask what type it is
      t <- faux::readline_check(type_q, "grep",
                                pattern = "^(i|s|d|f|b)$",
                                warning = "Enter only i, s, d, f or b")
      type <- types[t]
    }
    
    cb$variableMeasured[[i]]$type <- type
    
    if (type == "factor") {
      txt <- sprintf("Does it have these %d levels (%s)? [y/n] ",
                     length(vals), paste(vals, collapse = ","))
      lvl_check <- faux::readline_check(
        txt, "grep", pattern = "^(y|n)$",
        warning = "Enter only y or n")
      
      if (lvl_check == "y") {
        # keep existing levels
        lvl_n <- length(vals)
      } else {
        lvl_n <- faux::readline_check("How many? ", "integer")
      }
      
      # check level names and descriptions
      for (j in 1:lvl_n) {
        message("Type the name and (optionally) the description of each level in this format: 'lvl_name: The description'")
        lvl <- faux::readline_check(paste("Level", j), "length", min = 1)
        lvl2 <- strsplit(lvl, "\\:\\s*")[[1]]
        
        cb$variableMeasured[[i]]$levels[[lvl2[1]]] <- lvl2[length(lvl2)]
      }
    }
  }
  
  cb
}

