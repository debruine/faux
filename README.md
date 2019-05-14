---
output: github_document
always_allow_html: yes
---
<!-- rmarkdown v1 -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/163506566.svg)](https://zenodo.org/badge/latestdoi/163506566)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->





It is useful to be able to simulate data with a specified structure. The `faux` package provides some functions to make this process easier. See the [package website](https://debruine.github.io/faux/) for more details.

## Installation

You can install the newest version of faux from [GitHub](https://github.com/debruine/faux) with:

``` r
devtools::install_github("debruine/faux")
```

* [sim_design](#sim_design)
* [sim_df](#sim_df)
* [sim_mixed_cc](#sim_mixed_cc)
* [sim_mixed_df](#sim_mixed_df)
* [rnorm_multi](#rnorm_multi)
* [rnorm_pre](#rnorm_pre)
* [additional functions](#add_func)


## sim_design

This function creates a dataset with a specific between- and within-subjects design. [see vignette](https://debruine.github.io/faux/articles/sim_design.html)

For example, the following creates a 2w*2b design with 100 observations in each cell. The between-subject factor is `pet` with two levels (`cat` and `dog`). The within-subject factor is `time` with two levels (`day` and `night`). The mean for the `cat_day` cell is 10, the mean for the `cat_night` cell is 20, the mean for the `dog_day` cell is 15, and the mean for the `dog_night` cell is 25. All cells have a SD of 5 and all within-subject cells are correlated <code>r = 0.5</code>. The resulting data has exactly these values (set `empirical = FALSE` to sample from a population with these values). Set `plot = TRUE` to show a plot of means and SDs.



```r
between <- list("pet" = c("cat", "dog"))
within <- list("time" = c("day", "night"))
mu <- data.frame(
  cat = c(10, 20),
  dog = c(15, 25),
  row.names = within$time
)
df <- sim_design(within, between, 
                 n = 100, mu = mu, sd = 5, r = .5,
                 empirical = TRUE, plot = TRUE)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)



|pet |   n|var   | day| night| mean| sd|
|:---|---:|:-----|---:|-----:|----:|--:|
|cat | 100|day   | 1.0|   0.5|   10|  5|
|cat | 100|night | 0.5|   1.0|   20|  5|
|dog | 100|day   | 1.0|   0.5|   15|  5|
|dog | 100|night | 0.5|   1.0|   25|  5|


Table: Sample `sim_design()` stats

## sim_df

This function produces a data table with the same distributions and correlations as an existing data table. It only returns numeric columns and simulates all numeric variables from a continuous normal distribution (for now). [see vignette](https://debruine.github.io/faux/articles/sim_df.html)

For example, the following code creates a new sample from the built-in dataset `iris` with 50 observations of each species.


```r
new_iris <- sim_df(iris, 50, between = "Species") 
```

![Simulated iris dataset](figure/plot-iris-sim-1.png)

## sim_mixed_cc

This function produces a data table for a basic cross-classified design with random intercepts for subjects and items. 

For example, the following code produces the data for 100 subjects responding to 50 items where the response has an overall mean (`grand_i`) of 10. Subjects vary in their average response with an SD of 1, items vary in their average response with an SD of 2, and the residual error term has an SD of 3.


```r
dat <- sim_mixed_cc(
  sub_n = 100,  # subject sample size
  item_n = 50,  # item sample size
  grand_i = 10, # overall mean of the score
  sub_sd = 1,   # SD of subject random intercepts
  item_sd = 2,  # SD of item random intercepts
  error_sd = 3  # SD of residual error
)
```

You can then see how changing these numbers affects the random effects in an intercept-only mixed effects model.


```r
lme4::lmer(val ~ 1 + (1 | sub_id) + (1 | item_id), data = dat) %>%
  broom.mixed::tidy() %>%
  knitr::kable(digits = 3)
```



|effect   |group    |term            | estimate| std.error| statistic|
|:--------|:--------|:---------------|--------:|---------:|---------:|
|fixed    |NA       |(Intercept)     |    9.545|     0.284|    33.594|
|ran_pars |sub_id   |sd__(Intercept) |    1.071|        NA|        NA|
|ran_pars |item_id  |sd__(Intercept) |    1.836|        NA|        NA|
|ran_pars |Residual |sd__Observation |    3.041|        NA|        NA|

## sim_mixed_df

This function uses `lme4::lmer()` to get subject, item and error SDs from an existing dataset and simulates a new dataset with the specified number of subjects and items with distributions drawn from the example data.


```r

new_dat <- sim_mixed_df(faceratings, 
                        sub_n = 100, 
                        item_n = 50, 
                        dv = "rating", 
                        sub_id = "rater_id", 
                        item_id = "face_id")

```


## rnorm_multi

This function makes multiple normally distributed vectors with specified parameters and relationships. [see vignette](https://debruine.github.io/faux/articles/rnorm_multi.html)

For example, the following creates a sample that has 100 observations of 3 variables, drawn from a population where A has a mean of 0 and SD of 1, while B and C have means of 20 and SDs of 5. A correlates with B and C with r = 0.5, and B and C correlate with r = 0.25.


```r

dat <- rnorm_multi(
  n = 100, 
  mu = c(0, 20, 20),
  sd = c(1, 5, 5),
  r = c(0.5, 0.5, 0.25), 
  varnames = c("A", "B", "C"),
  empirical = FALSE
)
```




|   n|var |    A|    B|    C|  mean|   sd|
|---:|:---|----:|----:|----:|-----:|----:|
| 100|A   | 1.00| 0.43| 0.54| -0.14| 1.02|
| 100|B   | 0.43| 1.00| 0.10| 20.27| 5.24|
| 100|C   | 0.54| 0.10| 1.00| 18.89| 5.40|


Table: Sample `rnorm_multi()` stats


## rnorm_pre

This function creates a vector that has a specified correlation with an existing vector. 


```r
# create a pre-existing vector x
x <- rnorm(100, 0, 1)

# create a vector y with exactly mean=0, sd=1, and r(x,y)=0.5
y <- rnorm_pre(x, mu = 0, sd = 1, r = 0.5, empirical = TRUE)

list(
  mean = mean(y),
  sd = sd(y),
  r = cor(x,y)
) %>% str()
#> List of 3
#>  $ mean: num -8.89e-18
#>  $ sd  : num 1
#>  $ r   : num 0.5
```


If `empirical = FALSE` (the default), this resulting vector is sampled from a population with the specified parameters (but won't have *exactly* those properties).

![Distribution of 1000 samples from rnorm_pre](figure/unnamed-chunk-6-1.png)


## Additional functions {#add_func}

### check_sim_stats

If you want to check your simulated stats or just describe an existing dataset, use `check_sim_stats()`.


```r
check_sim_stats(iris)
#> # A tibble: 4 x 8
#>       n var   Sepal.Length Sepal.Width Petal.Length Petal.Width  mean    sd
#>   <dbl> <chr>        <dbl>       <dbl>        <dbl>       <dbl> <dbl> <dbl>
#> 1   150 Sepa…         1          -0.12         0.87        0.82  5.84  0.83
#> 2   150 Sepa…        -0.12        1           -0.43       -0.37  3.06  0.44
#> 3   150 Peta…         0.87       -0.43         1           0.96  3.76  1.77
#> 4   150 Peta…         0.82       -0.37         0.96        1     1.2   0.76
```

You can also group your data and change the digits to round. Display the table using `knitr::kable()` by setting `usekable` to `TRUE` (remember to set `results='asis'` in the chunk header.


```r
check_sim_stats(iris, 
                between = "Species", 
                digits = 3, 
                usekable = TRUE)
```



|Species    |  n|var          | Sepal.Length| Sepal.Width| Petal.Length| Petal.Width|  mean|    sd|
|:----------|--:|:------------|------------:|-----------:|------------:|-----------:|-----:|-----:|
|setosa     | 50|Sepal.Length |        1.000|       0.743|        0.267|       0.278| 5.006| 0.352|
|setosa     | 50|Sepal.Width  |        0.743|       1.000|        0.178|       0.233| 3.428| 0.379|
|setosa     | 50|Petal.Length |        0.267|       0.178|        1.000|       0.332| 1.462| 0.174|
|setosa     | 50|Petal.Width  |        0.278|       0.233|        0.332|       1.000| 0.246| 0.105|
|versicolor | 50|Sepal.Length |        1.000|       0.526|        0.754|       0.546| 5.936| 0.516|
|versicolor | 50|Sepal.Width  |        0.526|       1.000|        0.561|       0.664| 2.770| 0.314|
|versicolor | 50|Petal.Length |        0.754|       0.561|        1.000|       0.787| 4.260| 0.470|
|versicolor | 50|Petal.Width  |        0.546|       0.664|        0.787|       1.000| 1.326| 0.198|
|virginica  | 50|Sepal.Length |        1.000|       0.457|        0.864|       0.281| 6.588| 0.636|
|virginica  | 50|Sepal.Width  |        0.457|       1.000|        0.401|       0.538| 2.974| 0.322|
|virginica  | 50|Petal.Length |        0.864|       0.401|        1.000|       0.322| 5.552| 0.552|
|virginica  | 50|Petal.Width  |        0.281|       0.538|        0.322|       1.000| 2.026| 0.275|

### make_id

It is useful for IDs for random effects (e.g., subjects or stimuli) to be character strings (so you don't accidentally include them as fixed effects) with the same length s(o you can sort them in order like S01, S02,..., S10 rather than S1, S10, S2, ...) This function returns a list of IDs that have the same string length and a specified prefix.


```r
make_id(n = 10, prefix = "ITEM_")
#>  [1] "ITEM_01" "ITEM_02" "ITEM_03" "ITEM_04" "ITEM_05" "ITEM_06" "ITEM_07"
#>  [8] "ITEM_08" "ITEM_09" "ITEM_10"
```

You can also manually set the number of digits and set `n` to a range of integers.


```r
make_id(n = 10:20, digits = 3)
#>  [1] "S010" "S011" "S012" "S013" "S014" "S015" "S016" "S017" "S018" "S019"
#> [11] "S020"
```


### long2wide


```r
between <- list("pet" = c("cat", "dog"))
within <- list("time" = c("day", "night"))
df_long <- sim_design(within, between, long = TRUE)

df_wide <- long2wide(df_long, 
                     within = "time", 
                     between = "pet", 
                     dv = "val", 
                     id = "sub_id")
```




### pos_def_limits

Not all correlation matrices are possible. For example, if variables A and B are correlated with r = 1.0, then the correlation between A and C can only be exactly equal to the correlation between B and C.

The function `pos_def_limits()` lets you know what the possible range of values is for the missing value in a correlation matrix with one missing value. The correlation values are entered just from the top right triangle of the matrix, with a single `NA` for the missing value.


```r
lims <- pos_def_limits(.8, .2, NA)
```



|    min|   max|
|------:|-----:|
| -0.427| 0.747|



For example, if r~AB~ = 0.8 and r~AC~ = 0.2, then -0.427 <= r~BC~ <= 0.747.

If you enter a correlation matrix that contains impossible combinations, your limits will be `NA`.


```r
lims <- pos_def_limits(.8, .2,  0,
                          -.5, NA,
                               .2)
```



|min |max |
|:---|:---|
|NA  |NA  |



### is_pos_def

If you have a full matrix and want to know if it is positive definite, you can use the following code:


```r
c(.2, .3, .4, .2,
      .3, -.1, .2,
           .4, .5,
               .3) %>%
  cormat_from_triangle() %>%
  is_pos_def()
#> [1] TRUE
```


```r
matrix(c(1, .3, -.9, .2,
        .3,  1,  .4, .5,
       -.9, .4,   1, .3,
        .2, .5,  .3,  1), 4) %>%
  is_pos_def()
#> [1] FALSE
```





