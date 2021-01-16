
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kim

<!-- badges: start -->

[![CRAN
checks](https://cranchecks.info/badges/summary/kim)](https://cran.r-project.org/web/checks/check_results_kim.html)
[![Travis build
status](https://travis-ci.com/jinkim3/kim.svg?branch=master)](https://travis-ci.com/jinkim3/kim)
[![R build
status](https://github.com/jinkim3/kim/workflows/R-CMD-check/badge.svg)](https://github.com/jinkim3/kim/actions)

[![Total
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/kim?color=blue)](https://cran.r-project.org/package=kim)
[![](https://img.shields.io/github/last-commit/jinkim3/kim.svg)](https://github.com/jinkim3/kim/commits/master)
[![CodeFactor](https://www.codefactor.io/repository/github/jinkim3/kim/badge)](https://www.codefactor.io/repository/github/jinkim3/kim)
<!-- badges: end -->

This package contains various functions that simplify and expedite
analyses of experimental data. Examples include a function that plots
sample means of groups in a factorial experimental design, a function
that conducts robust regressions with bootstrapped samples, and a
function that conducts robust two-way analysis of variance.

## Installation

You can install the released version of kim from
[CRAN](https://cran.r-project.org/package=kim) with:

``` r
install.packages("kim")
```

You can also install the development version from [kim on
GitHub](https://github.com/jinkim3/kim) with:

``` r
install.packages("devtools")
devtools::install_github("jinkim3/kim")
```

If you run into errors while using the package, try updating the package
to the most recent version available on [kim on
GitHub](https://github.com/jinkim3/kim) with:

``` r
update_kim()
```

## Example

Here are some examples of using this package.

``` r
library(kim)

# create a scatter plot
scatterplot(data = mtcars, x_var_name = "wt", y_var_name = "mpg")

# get descriptive statistics by group
desc_stats_by_group(
  data = mtcars, var_for_stats = "mpg", grouping_vars = c("vs", "am"))

# plot histograms by group
histogram_by_group(data = mtcars, iv_name = "cyl", dv_name = "mpg")

# plot sample means of groups in a factorial experimental design
plot_group_means(data = mtcars, dv_name = "mpg", iv_name = "gear")

# conduct a two-way ANOVA
two_way_anova(
  data = mtcars, dv_name = "mpg", iv_1_name = "vs", iv_2_name = "am")

# conduct a multiple regression analysis
multiple_regression(data = mtcars, formula = mpg ~ gear * cyl)

# conduct a robust regression analysis using bootstrapped samples
robust_regression(data = mtcars, formula = mpg ~ cyl * hp)

# conduct a mediation analysis
mediation_analysis(
  data = mtcars, iv_name = "cyl", mediator_name = "disp", dv_name = "mpg")
```
