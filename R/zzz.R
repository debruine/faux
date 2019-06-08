## set default options for faux_options:
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.faux <- list(
    faux.connection = stdin(),
    faux.sep = "_",
    faux.plot = TRUE
  )
  toset <- !(names(op.faux) %in% names(op))
  if(any(toset)) options(op.faux[toset])
  
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("************\nWelcome to faux. For support visit: http://debruine.github.io/faux/")
  packageStartupMessage("- Get and set global package options with: faux_options()\n- For examples see: browseVignettes(\"faux\")\n************")
}