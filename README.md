
<!-- README.md is generated from README.Rmd. Please edit that file -->
baymedr: BAYesian inference for MEDical designs in R
====================================================

<!-- badges: start -->
<!-- badges: end -->
`baymedr` is an R package with the goal of providing researchers with functions for the computation of Bayes factors for common biomedical research designs. Implemented are functions to test the equivalence (`equiv_bf`), non-inferiority (`infer_bf`), and superiority (`super_bf`) of an intervention group (e.g., a new medication) compared to some form of control group (e.g., a placebo or an already existing medication). Bayes factors for these three tests can be computed based on raw data (arguments: x, y) or summary statistics (arguments: n\_x, n\_y, mean\_x, mean\_y, sd\_x, sd\_y), making it possible to reanalyze findings (e.g., from publications) without the need to obtain the raw data.

Installation
------------

You can install the latest development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("maxlinde/baymedr")
```

Random example data
-------------------

In order to demonstrate the three functions from `baymedr`, we create an example dataset (data). We need a control group "con" and an experimental group "exp" (condition). Further, random numbers, sampled from the normal distribution, within each group are created, serving as the dependent variable of interest (dv):

``` r
set.seed(1)

data <- data.frame(
  condition = rep(x = c("con", "exp"),
                  c(150, 180)),
  dv = c(rnorm(n = 150,
               mean = 7.3,
               sd = 3.4),
         rnorm(n = 180,
               mean = 8.9,
               sd = 3.1))
)

head(x = data,
     n = 10)
#>    condition        dv
#> 1        con  5.170057
#> 2        con  7.924387
#> 3        con  4.458863
#> 4        con 12.723955
#> 5        con  8.420326
#> 6        con  4.510407
#> 7        con  8.957259
#> 8        con  9.810304
#> 9        con  9.257657
#> 10       con  6.261679
```

The superiority test (`super_bf`)
---------------------------------

With `super_bf` we can test the null hypothesis that the experimental group (e.g., a new medication) is not better than the control group (e.g., a placebo or existing medication). Because research practices diverge on whether to conduct a superiority test as a one-sided or two-sided test, two Bayes factors are obtained from `super_bf`, one for each research strategy.

We can use the raw data to compute Bayes factors:

``` r
library(baymedr)

super_bf(x = data$dv[data$condition == "con"],
         y = data$dv[data$condition == "exp"])
#>   One-sided M1 lower than M2 BF = 46514.41 
#>   Two-sided M1 not equal to M2 BF = 23257.21
#> BF one-tailed BF two-tailed 
#>      46514.41      23257.21
```

Alternatively, if the raw data are not available, we can use summary statistics to compute Bayes factors:

``` r
library(baymedr)

super_bf(n_x = 138,
         n_y = 179,
         mean_x = 18.3,
         mean_y = 21.9,
         sd_x = 3.3,
         sd_y = 4.0)
#>   One-sided M1 lower than M2 BF = 2.041406e+13 
#>   Two-sided M1 not equal to M2 BF = 1.020703e+13
#> BF one-tailed BF two-tailed 
#>  2.041406e+13  1.020703e+13
```

The equivalence test (`equiv_bf`)
---------------------------------

With `equiv_bf` we can test the null hypothesis that the control group (e.g., a placebo or existing medication) and the experimental group (e.g., a new medication) are equivalent. The alternative hypothesis is that the two groups are not equivalent.

We can use the raw data to compute the Bayes factor:

``` r
library(baymedr)

equiv_bf(x = data$dv[data$condition == "con"],
         y = data$dv[data$condition == "exp"])
#>   Two-sided M1 equal to M2;   BF = 0
#> BF equivalence 
#>   4.299741e-05
```

Alternatively, if the raw data are not available, we can use summary statistics to compute the Bayes factor:

``` r
library(baymedr)

equiv_bf(n_x = 138,
         n_y = 179,
         mean_x = 18.3,
         mean_y = 21.9,
         sd_x = 3.3,
         sd_y = 4.0)
#>   Two-sided M1 equal to M2;   BF = 0
#> BF equivalence 
#>   9.797167e-14
```

The non-inferiority test (`infer_bf`)
-------------------------------------

With `infer_bf` we can test the null hypothesis that the experimental group (e.g., a new medication) is not better than the control group (e.g., a placebo or existing medication) minus a constant value (c). The alternative hypothesis is that the experimental group is better than the control group minus a constant value (c). Put more formally, the null hypothesis states that the true population effect size &lt; -c, resulting in the point null hypothesis that the true population effect size = -c against the one-sided alternative hypothesis that the true population effect size &gt; -c.

We can use the raw data to compute the Bayes factor:

``` r
library(baymedr)

infer_bf(x = data$dv[data$condition == "con"],
         y = data$dv[data$condition == "exp"],
         ni_margin = 2)
#>   Mean group 2 at least as high as (mean group 1 - ni_margin) BF = 42943307886
#> non-inferiority BF 
#>        42943307886
```

Alternatively, if the raw data are not available, we can use summary statistics to compute the Bayes factor:

``` r
library(baymedr)

infer_bf(n_x = 138,
         n_y = 179,
         mean_x = 18.3,
         mean_y = 21.9,
         sd_x = 3.3,
         sd_y = 4.0,
         ni_margin = 2)
#>   Mean group 2 at least as high as (mean group 1 - ni_margin) BF = 7033902416
#> non-inferiority BF 
#>         7033902416
```
