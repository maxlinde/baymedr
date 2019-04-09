
<!-- README.md is generated from README.Rmd. Please edit that file -->
baymedr: BAYesian inference for MEDical designs in R
====================================================

<!-- badges: start -->
<!-- badges: end -->
`baymedr` is an R package with the goal of providing researchers with functions for the computation of Bayes factors for common biomedical research designs (see van Ravenzwaaij et al. (2019)). Implemented are functions to test the equivalence (`equiv_bf()`), non-inferiority (`infer_bf()`), and superiority (`super_bf()`) of an intervention group (e.g., a new medication) compared to some form of control group (e.g., a placebo or an already existing medication).

It can be argued that the Bayesian approach to data analysis is superior to the frequentist approach. For example, the frequentist approach requires the data analyst to specify an equivalence interval in order to be able to execute an equivalence test. Moreover, Bayesian statistics, allows to quantify evidence in favour of the null hypothesis, which is impossible with the frequentist approach. Using Bayesian data analysis, it is also possible to test sequentially. That is, it is fully legitimate to sample a certain number of participants, analyse the data, sample new participants, and repeat this cycle as desired. In contrast, using frequentist statistics, this optional stopping rule, although not uncommon, is considered a bad practice because it inflates the Type I error rate (e.g., Schönbrodt et al. (2017)). For a more thorough discussion of Bayesian advantages, see van Ravenzwaaij et al. (2019) and Wagenmakers et al. (2018).

Installation
------------

You can install the latest development version of `baymedr` from [GitHub](https://github.com/), using the `devtools` package, with:

``` r
# install.packages("devtools")
devtools::install_github("maxlinde/baymedr")
```

If you want to access vignettes and install the additional packages listed in "Suggests", use:

``` r
# install.packages("devtools")
devtools::install_github("maxlinde/baymedr",
                         build_vignettes = TRUE)
```

General usage
-------------

All three functions for the three research designs (i.e., equivalence, non-inferiority, and superiority) allow the user to compute Bayes factors based on raw data (if arguments `x` and `y` are defined) or summary statistics (if arguments `n_x`, `n_y`, `mean_x`, `mean_y`, `sd_x`, and `sd_y` are defined). If summary statistics are used, the user has the option to specify `ci_margin` instead of `sd_x` and `sd_y` if tests for equivalence and superiority are employed but not if the non-inferiority test is used.

For the superiority test, users can specify whether they want to do a one- or two-sided tests with the argument `one_sided`. Although not required (see above), a symmetric or an asymmetric equivalence interval can be specified for the equivalence test with the argument `interval`. Lastly, for all three tests the user can set the desired width of the prior distribution using the argument `prior_scale`. The default is set to 1 / sqrt(2), in accordance with the suggestions by Rouder et al. (2009).

Usage of the functions for equivalence, non-inferiority, and superiority designs (i.e., `equiv_bf()`, `infer_bf()`, and `super_bf()`, respectively), results in S4 objects of classes 'baymedrEquivalence', 'baymedrNonInferiority', and 'baymedrSuperiority', respectively. Summary information are shown in the console by printing the created S4 object. To extract the Bayes factor from one of the three S4 objects, use the function `get_bf()`.

Random example data
-------------------

In order to demonstrate the three functions from `baymedr`, we create an example dataset (data). There is a control group "con" and an experimental group "exp" (condition). Further, random numbers, sampled from the normal distribution, within each group are created, serving as the dependent variable of interest (dv):

``` r
set.seed(123)

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
#> 1        con  5.394383
#> 2        con  6.517397
#> 3        con 12.599608
#> 4        con  7.539729
#> 5        con  7.739578
#> 6        con 13.131221
#> 7        con  8.867115
#> 8        con  2.998792
#> 9        con  4.964700
#> 10       con  5.784749
```

The superiority test (`super_bf()`)
-----------------------------------

With `super_bf()` we can test the null hypothesis that the experimental group (e.g., a new medication) is not better than the control group (e.g., a placebo or an already existing medication). Research practices diverge on whether to conduct a superiority test as a one- or two-sided test. Here we use a one-sided test (the default; `one-sided` = TRUE)

We can use the raw data to compute a Bayes factor:

``` r
library(baymedr)

mod_super_raw <- super_bf(x = data$dv[data$condition == "con"],
                          y = data$dv[data$condition == "exp"])

mod_super_raw
#> ******************************
#> Superiority analysis
#> --------------------
#> H0: mu2 = mu1; Ha: mu2 > mu1
#> Data: raw data
#> Prior scale: 0.707
#> 
#>     BF = 983920.9
#> ******************************

get_bf(object = mod_super_raw)
#> [1] 983920.9
```

Alternatively, if the raw data are not available, we can use summary statistics to compute a Bayes factor:

``` r
library(baymedr)

mod_super_sum <- super_bf(n_x = 138,
                          n_y = 179,
                          mean_x = 18.3,
                          mean_y = 21.9,
                          sd_x = 3.3,
                          sd_y = 4.0)

mod_super_sum
#> ******************************
#> Superiority analysis
#> --------------------
#> H0: mu2 = mu1; Ha: mu2 > mu1
#> Data: summary data
#> Prior scale: 0.707
#> 
#>     BF = 2.041406e+13
#> ******************************

get_bf(object = mod_super_sum)
#> [1] 2.041406e+13
```

The equivalence test (`equiv_bf()`)
-----------------------------------

With `equiv_bf()` we can test the null hypothesis that the control group (e.g., a placebo or an already existing medication) and the experimental group (e.g., a new medication) are equivalent. The alternative hypothesis is that the two groups are not equivalent.

We can use the raw data to compute a Bayes factor:

``` r
library(baymedr)

mod_equiv_raw <- equiv_bf(x = data$dv[data$condition == "con"],
                          y = data$dv[data$condition == "exp"])

mod_equiv_raw
#> ******************************
#> Equivalence analysis
#> --------------------
#> H0: mu2 = mu1; Ha: mu2 != mu1
#> Data: raw data
#> Equivalence interval: Lower = 0; Upper = 0
#> Prior scale: 0.707
#> 
#>     BF = 2.032684e-06
#> ******************************

get_bf(object = mod_equiv_raw)
#> [1] 2.032684e-06
```

Alternatively, if the raw data are not available, we can use summary statistics to compute a Bayes factor:

``` r
library(baymedr)

mod_equiv_sum <- equiv_bf(n_x = 138,
                          n_y = 179,
                          mean_x = 18.3,
                          mean_y = 21.9,
                          sd_x = 3.3,
                          sd_y = 4.0)

mod_equiv_sum
#> ******************************
#> Equivalence analysis
#> --------------------
#> H0: mu2 = mu1; Ha: mu2 != mu1
#> Data: summary data
#> Equivalence interval: Lower = 0; Upper = 0
#> Prior scale: 0.707
#> 
#>     BF = 9.797167e-14
#> ******************************

get_bf(object = mod_equiv_sum)
#> [1] 9.797167e-14
```

The non-inferiority test (`infer_bf()`)
---------------------------------------

With `infer_bf()` we can test the null hypothesis that the experimental group (e.g., a new medication) is not better than the control group (e.g., a placebo or an already existing medication) minus a constant value (c). The alternative hypothesis is that the experimental group is better than the control group minus a constant value (c). Put more formally, the null hypothesis states that the true population effect size &lt; -c, resulting in the point null hypothesis that the true population effect size = -c against the one-sided alternative hypothesis that the true population effect size &gt; -c.

We can use the raw data to compute a Bayes factor:

``` r
library(baymedr)

mod_infer_raw <- infer_bf(x = data$dv[data$condition == "con"],
                          y = data$dv[data$condition == "exp"],
                          ni_margin = -0.5)

mod_infer_raw
#> ******************************
#> Non-inferiority analysis
#> ------------------------
#> H0: mu2 - mu1 = ni_margin; Ha: mu2 - mu1 > ni_margin
#> Data: raw data
#> Non-inferiority margin: -0.5
#> Prior scale: 0.707
#> 
#>     BF = 55487.31
#> ******************************

get_bf(object = mod_infer_raw)
#> [1] 55487.31
```

Alternatively, if the raw data are not available, we can use summary statistics to compute a Bayes factor:

``` r
library(baymedr)

mod_infer_sum <- infer_bf(n_x = 138,
                          n_y = 179,
                          mean_x = 18.3,
                          mean_y = 21.9,
                          sd_x = 3.3,
                          sd_y = 4.0,
                          ni_margin = 2)

mod_infer_sum
#> ******************************
#> Non-inferiority analysis
#> ------------------------
#> H0: mu2 - mu1 = ni_margin; Ha: mu2 - mu1 > ni_margin
#> Data: summary data
#> Non-inferiority margin: 2
#> Prior scale: 0.707
#> 
#>     BF = 7033902416
#> ******************************

get_bf(object = mod_infer_sum)
#> [1] 7033902416
```

References
----------

Rouder, J. N., Speckman, P. L., Sun, D., & Morey, R. D. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. *Psychonomic Bulletin & Review*, *16*(2), 225-237.

Schönbrodt, F. D., Wagenmaker, E.-J., Zehetleitner, M., & Perugini, M. (2017). Sequential hypothesis testing with bayes factors: Efficiently testing mean differences. *Psychological Methods*, *22*(2), 322-339.

van Ravenzwaaij, D., Monden, R., Tendeiro, J. N., & Ioannidis, J. P. A. (2019). Bayes factors for superiority, non-inferiority, and equivalence designs. *BMC Medical Research Methodology*, *19*(1), 71.

Wagenmakers, E.-J., Marsman, M., Jamil, T., Ly, A., Verhagen, J., Love, J., ..., & Matzke, D. (2018). Bayesian inference for psychology. Part I: Theoretical advantages and practical ramifications. *Psychonomic Bulletin & Review*, *25*(1), 35-57.
