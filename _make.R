use_version()

# update CITATION
txt <- sprintf('citHeader("To cite faux in publications use:")

citEntry(
  entry    = "Manual",
  title    = "faux: Simulation for Factorial Designs",
  author   = "Lisa DeBruine",
  doi      = "10.5281/zenodo.2669586",
  publisher = "Zenodo",
  year     = "2020",
  note     = "R package version %s",
  url      = "https://debruine.github.io/faux/",
  textVersion = "Lisa DeBruine (2020). faux: Simulation for Factorial Designs. R package version %s. Zenodo. http://doi.org/10.5281/zenodo.2669586"
)', version, version)

write(txt, "inst/CITATION")


# check for redirected links??
devtools::check(remote = TRUE)

# final checks before submission to CRAN
devtools::check_win_devel()