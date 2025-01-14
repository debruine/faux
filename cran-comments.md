## R CMD check results

0 errors | 0 warnings | 2 notes

* DeBruine is my name and not misspelled
* https://doi.org/10.1111/j.1467-6494.1996.tb00813.x is a real DOI
* I moved most of the vignettes to pkgdown articles so they don't take too long
* There was a note about the test for norm2likert, but it was skipped with skip(), which I then changed to skip_on_cran(), and now commented them all out. I am at a loss as to how this keeps coming up as a note!


## responses to coments

> Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)

- pipe.Rd: This is an imported function (from dplyr), so it seems odd for me to replicate their explanations, but I've done it.
- print.design, print.nested_list, print.psychds_codebook: Done!

> \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user. Does not seem necessary. Please replace \dontrun with \donttest.

- faux_options: Done!

> Please do not modifiy the .GlobalEnv. This is not allowed by the CRAN policies.

- R/norta.R: Done!
