# faux 0.0.0.9004

* Added a `NEWS.md` file to track changes to the package.
* Added `sim_design()` to simulate data for mixed ANOVA designs.

# faux 0.0.0.9005

* Bug fixes for `sim_design()` (failed when within or between factor number was 0)

# faux 0.0.0.9006

* Changes to argument order and names (more consistent, but may break old scripts)
* Updated vignettes

# faux 0.0.0.9007

* Added a plot option to `check_design()` and `sim_design()`
* Design lists returned by `check_design()` have a more consistent format 
    - n, mu, and sd are all data frames with between-cells as rows and within-cells as columns
    - `within` and `between` are named lists; factors and labels are no longer separately named
    
# faux 0.0.0.9008

* added `sim_mixed_cc()` to simulate null cross-classified mixed effect designs by subject, item and error SDs
* `sim_design()`, `sim_df()`, `sim_mixed_cc()` and `sim_mixed_df()` take a `seed` argument now for reproducible datasets