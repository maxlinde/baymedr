
<!-- README.md is generated from README.Rmd. Please edit that file -->
baymedr: BAYesian inference for MEDical designs in R
====================================================

<!-- badges: start -->
<!-- badges: end -->
`baymedr` is an R package with the goal of providing researchers with functions for the computation of Bayes factors for common biomedical research designs (see van Ravenzwaaij et al. (2019)). Implemented are functions to test the equivalence (`equiv_bf()`), non-inferiority (`infer_bf()`), and superiority (`super_bf()`) of an intervention group (e.g., a new medication) compared to some form of control group (e.g., a placebo or an already existing medication).

It can be argued that the Bayesian approach to data analysis is superior to the frequentist approach. For example, the frequentist approach requires the data analyst to specify an equivalence interval in order to be able to execute an equivalence test. Moreover, Bayesian statistics, allows to quantify evidence in favour of the null hypothesis, which is impossible with the frequentist approach. Using Bayesian data analysis, it is also possible to test sequentially. That is, it is fully legitimate to sample a certain number of participants, analyse the data, sample new participants, and repeat this cycle as desired. In contrast, using frequentist statistics, this optional stopping rule, although not uncommon, is considered a bad practice because it inflates the Type I error rate (e.g., Schönbrodt et al. (2017). For a more thorough discussion of Bayesian advantages, see van Ravenzwaaij et al. (2019) and Wagenmakers et al. (2018).

Installation
------------

You can install the latest development version of `baymedr` from [GitHub](https://github.com/), using the `devtools` package, with:

``` r
# install.packages("devtools")
devtools::install_github("maxlinde/baymedr")
```

General usage
-------------

All three functions for the three research designs (i.e., equivalence, non-inferiority, and superiority) allow the user to compute Bayes factors based on raw data (if arguments `x` and `y` are defined) or summary statistics (if arguments `n_x`, `n_y`, `mean_x`, `mean_y`, `sd_x`, and `sd_y` are defined). If summary statistics are used, the user has the option to specify `ci_margin` instead of `sd_x` and `sd_y` if tests for equivalence and superiority are employed but not if the non-inferiority test is used.

For the superiority test, users can specify whether they want to do a one- or two-sided tests with the argument `one_sided`. Although not required (see above), a symmetric or an asymmetric equivalence interval can be specified for the equivalence test with the argument `interval`. Lastly, for all three tests the user can set the desired width of the prior distribution using the argument `prior_scale`. The default is set to $1 / \\sqrt{2}$, in accordance with the suggestions by Rouder et al. (2009).

Usage of the functions for equivalence, non-inferiority, and superiority designs (i.e., `equiv_bf()`, `infer_bf()`, and `super_bf()`, respectively), results in S4 objects of classes 'baymedrEquivalence', 'baymedrNonInferiority', and 'baymedrSuperiority', respectively. Summary information are shown in the console by printing the created S4 object. To extract the Bayes factor from one of the three S4 objects, use the function `get_bf()`.

Random example data
-------------------

In order to demonstrate the three functions from `baymedr`, we create an example dataset (data). There is a control group "con" and an experimental group "exp" (condition). Further, random numbers, sampled from the normal distribution, within each group are created, serving as the dependent variable of interest (dv):

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

The superiority test (`super_bf()`)
-----------------------------------

With `super_bf()` we can test the null hypothesis that the experimental group (e.g., a new medication) is not better than the control group (e.g., a placebo or existing medication). Because research practices diverge on whether to conduct a superiority test as a one-sided or two-sided test, two Bayes factors are obtained from `super_bf()`, one for each research strategy.

We can use the raw data to compute Bayes factors:

``` r
library(baymedr)

super_bf(x = data$dv[data$condition == "con"],
         y = data$dv[data$condition == "exp"])
#> ******************************
#> Superiority analysis
#> --------------------
#> H0: mu2 = mu1; Ha: mu2 > mu1
#> Prior scale: 0.707
#> 
#>     BF = 46514.41
#> ******************************
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
#> ******************************
#> Superiority analysis
#> --------------------
#> H0: mu2 = mu1; Ha: mu2 > mu1
#> Prior scale: 0.707
#> 
#>     BF = 2.041406e+13
#> ******************************
```

The equivalence test (`equiv_bf()`)
-----------------------------------

With `equiv_bf()` we can test the null hypothesis that the control group (e.g., a placebo or existing medication) and the experimental group (e.g., a new medication) are equivalent. The alternative hypothesis is that the two groups are not equivalent.

We can use the raw data to compute the Bayes factor:

``` r
library(baymedr)

equiv_bf(x = data$dv[data$condition == "con"],
         y = data$dv[data$condition == "exp"])
#> ******************************
#> Equivalence analysis
#> --------------------
#> H0: mu2 = mu1; Ha: mu2 != mu1
#> Equivalence interval: Lower = 0; Upper = 0
#> Prior scale: 0.707
#> 
#>     BF = 0
#> ******************************
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
#> ******************************
#> Equivalence analysis
#> --------------------
#> H0: mu2 = mu1; Ha: mu2 != mu1
#> Equivalence interval: Lower = 0; Upper = 0
#> Prior scale: 0.707
#> 
#>     BF = 0
#> ******************************
```

The non-inferiority test (`infer_bf()`)
---------------------------------------

With `infer_bf()` we can test the null hypothesis that the experimental group (e.g., a new medication) is not better than the control group (e.g., a placebo or existing medication) minus a constant value (c). The alternative hypothesis is that the experimental group is better than the control group minus a constant value (c). Put more formally, the null hypothesis states that the true population effect size &lt; -c, resulting in the point null hypothesis that the true population effect size = -c against the one-sided alternative hypothesis that the true population effect size &gt; -c.

We can use the raw data to compute the Bayes factor:

``` r
library(baymedr)

infer_bf(x = data$dv[data$condition == "con"],
         y = data$dv[data$condition == "exp"],
         ni_margin = 2)
#> ******************************
#> Non-inferiority analysis
#> ------------------------
#> H0: mu2 - mu1 = ni_margin; Ha: mu2 - mu1 > ni_margin
#> Non-inferiority margin: 2
#> Prior scale: 0.707
#> 
#>     BF = 42943307886
#> ******************************
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
#> ******************************
#> Non-inferiority analysis
#> ------------------------
#> H0: mu2 - mu1 = ni_margin; Ha: mu2 - mu1 > ni_margin
#> Non-inferiority margin: 2
#> Prior scale: 0.707
#> 
#>     BF = 7033902416
#> ******************************
```

References
----------

Rouder, J. N., Speckman, P. L., Sun, D., & Morey, R. D. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. *Psychonomic Bulletin & Review*, *16*(2), 225-237.

Schönbrodt, F. D., Wagenmaker, E.-J., Zehetleitner, M., & Perugini, M. (2017). Sequential hypothesis testing with bayes factors: Efficiently testing mean differences. *Psychological Methods*, *22*(2), 322-339.

van Ravenzwaaij, D., Monden, R., Tendeiro, J. N., & Ioannidis, J. P. A. (2019). Bayes factors for superiority, non-inferiority, and equivalence designs. *BMC Medical Research Methodology*, *19*(1), 71.

Wagenmakers, E.-J., Marsman, M., Jamil, T., Ly, A., Verhagen, J., Love, J., ..., & Matzke, D. (2018). Bayesian inference for psychology. Part I: Theoretical advantages and practical ramifications. *Psychonomic Bulletin & Review*, *25*(1), 35-57.
