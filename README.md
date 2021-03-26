
# baymedr: BAYesian inference for MEDical designs in R

`baymedr` is an R package with the goal of providing researchers with
easy-to-use tools for the computation of Bayes factors for common
biomedical research designs (see van Ravenzwaaij et al., 2019).
Implemented are functions to test the equivalence (`equiv_bf()`),
non-inferiority (`infer_bf()`), and superiority (`super_bf()`) of an
experimental group (e.g., a new medication) compared to a control group
(e.g., a placebo or an already existing medication) on a continuous
dependent variable. A special focus of `baymedr` lies on a user-friendly
interface, so that a wide variety or researchers (i.e., not only
statisticians) can utilize `baymedr` for their analyses.

## Installation and attaching

To install `baymedr` use:

``` r
install.packages("baymedr")
```

You can install the latest development version of `baymedr` from
[GitHub](https://github.com/), using the `devtools` package, with:

``` r
# install.packages("devtools")
devtools::install_github("maxlinde/baymedr")
```

Subsequently, you can load `baymedr`, so that it is ready to use:

``` r
library(baymedr)
```

## General usage

All three functions for the three research designs (i.e., equivalence,
non-inferiority, and superiority) allow the user to compute Bayes
factors based on raw data (if arguments `x` and `y` are defined) or
summary statistics (if arguments `n_x`, `n_y`, `mean_x`, `mean_y`,
`sd_x`, and `sd_y` are defined). If summary statistics are used, the
user has the option to specify `ci_margin` and `ci_level` instead of
`sd_x` and `sd_y`. In general, arguments with ‘x’ as a name or suffix
correspond to the control group and those with ‘y’ as a name or suffix
refer to the experimental group. Importantly, the dependent variable
must be continuous in order to obtain valid results.

Usage of the functions for equivalence (`equiv_bf()`), non-inferiority
(`infer_bf()`), and superiority designs (`super_bf()`), results in S4
objects of classes `baymedrEquivalence`, `baymedrNonInferiority`, and
`baymedrSuperiority`, respectively. Summary information are shown in the
console by printing the created S4 object. To extract the Bayes factor
from one of the three S4 objects, use the function `get_bf()`.

The Bayes factors resulting from `super_bf()` and `infer_bf()` quantify
evidence in favor of the data under the alternative hypothesis (i.e.,
superiority and non-inferiority, respectively) relative to the data
under the null hypothesis. In contrast, the Bayes factor resulting from
`equiv_bf()` quantifies evidence in favor of the data under the null
hypothesis (i.e., equivalence) relative to the data under the
alternative hypothesis. In case the evidence for the data under the
other hypothesis is desired, the user can take the reciprocal of the
Bayes factor.

## The Cauchy prior distribution

Bayesian inference requires the specification of a prior distribution,
which mirrors prior beliefs about the likelihood of parameter values.
For the equivalence, non-inferiority, and superiority tests, the
parameter of interest is the effect size between the experimental and
control conditions (see, e.g., Rouder et al., 2009; van Ravenzwaaij et
al., 2019). If relevant information is available, this knowledge could
be expressed in an idiosyncratic prior distribution. Most of the time,
however, relevant information is missing. In that case, it is reasonable
to define a prior distribution that is as objective as possible. It has
been argued that the Cauchy probability density function centered on 0
represents such a function (see, e.g., Rouder et al., 2009). The
standard Cauchy distribution resembles a standard Normal distribution,
except that the Cauchy distribution has less mass at the center but
instead heavier tails. The center of the distribution is determined by
the location parameter, while the width is specified by the scale
parameter. By varying the scale of the Cauchy prior, the user can change
the range of reasonable effect sizes. This is accomplished with the
argument `prior_scale`.

## Random example data

In order to demonstrate the three functions within `baymedr`, we create
an example dataset (data). There is a control group “con” and an
experimental group “exp” (condition). Further, random numbers, sampled
from the Normal distribution, within each group are created, serving as
the dependent variable of interest (dv):

``` r
set.seed(123456789)

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

data[c(1:5, 151:155), ]
#>     condition        dv
#> 1         con  9.016566
#> 2         con  8.645978
#> 3         con 12.112828
#> 4         con  4.844097
#> 5         con  5.197586
#> 151       exp  8.541017
#> 152       exp 10.693952
#> 153       exp 12.782147
#> 154       exp  9.094879
#> 155       exp  7.347376
```

## The superiority test (`super_bf()`)

With `super_bf()` we can test whether the experimental group is better
than the control group. Importantly, sometimes low and sometimes high
values on the measure of interest represent superiority, which can be
specified with the argument `direction`. In the case where low values
represent superiority we have BF-0, indicating that we quantify evidence
for the data under the negative alternative hypothesis (i.e., H-)
relative to the null hypothesis (i.e., H0). In the case where high
values represent superiority we have BF+0, indicating that we quantify
evidence for the data under the positive alternative hypothesis (i.e.,
H+) relative to the null hypothesis (i.e., H0). The default is that high
values represent superiority.

We can use the raw data to compute a Bayes factor:

``` r
mod_super_raw <- super_bf(
  x = data$dv[data$condition == "con"],
  y = data$dv[data$condition == "exp"]
)

mod_super_raw
#> ******************************
#> Superiority analysis
#> --------------------
#> Data:                         raw data
#> H0 (non-superiority):         mu_y == mu_x
#> H+ (superiority):             mu_y > mu_x
#> Cauchy prior scale:           0.707
#> 
#>     BF+0 (superiority) = 44.00
#> ******************************

get_bf(object = mod_super_raw)
#> [1] 44.00176
```

Alternatively, if the raw data are not available, we can use summary
statistics to compute a Bayes factor (cf. van Ravenzwaaij et al., 2019).
The data were obtained from Skjerven et al. (2013):

``` r
mod_super_sum <- super_bf(
  n_x = 201,
  n_y = 203,
  mean_x = 68.1,
  mean_y = 63.6,
  ci_margin = (15.5 - (-6.5)) / 2,
  ci_level = 0.95,
  direction = "low"
)

mod_super_sum
#> ******************************
#> Superiority analysis
#> --------------------
#> Data:                         summary data
#> H0 (non-superiority):         mu_y == mu_x
#> H- (superiority):             mu_y < mu_x
#> Cauchy prior scale:           0.707
#> 
#>     BF-0 (superiority) = 0.24
#> ******************************

get_bf(object = mod_super_sum)
#> [1] 0.2364177
```

## The equivalence test (`equiv_bf()`)

With `equiv_bf()` we can test whether the experimental and the control
groups are (practically) equivalent. With the argument `interval`, an
equivalence interval can be specified. The argument `interval_std` can
be used to specify whether the equivalence interval is given in
standardized (TRUE; the default) or unstandardized (FALSE) units.
However, in contrast to the frequentist equivalence test, `equiv_bf()`
can also incorporate a point null hypothesis, which constitutes the
default in `equiv_bf()` (i.e., `interval` = 0). The Bayes factor (i.e.,
BF01) resulting from `equiv_bf()` quantifies evidence for the data under
the null hypothesis (i.e., H0) relative to the two-sided alternative
hypothesis (i.e., H1).

We can use the raw data to compute a Bayes factor:

``` r
mod_equiv_raw <- equiv_bf(
  x = data$dv[data$condition == "con"],
  y = data$dv[data$condition == "exp"],
  interval = 0.1,
)

mod_equiv_raw
#> ******************************
#> Equivalence analysis
#> --------------------
#> Data:                         raw data
#> H0 (equivalence):             mu_y - mu_x > c_low AND mu_y - mu_x < c_high
#> H1 (non-equivalence):         mu_y - mu_x < c_low OR mu_y - mu_x > c_high
#> Equivalence interval:         Lower = -0.10; Upper = 0.10 (standardised)
#>                               Lower = -0.33; Upper = 0.33 (unstandardised)
#> Cauchy prior scale:           0.707
#> 
#>     BF01 (equivalence) = 0.11
#> ******************************

get_bf(object = mod_equiv_raw)
#> [1] 0.108721
```

Alternatively, if the raw data are not available, we can use summary
statistics to compute a Bayes factor (cf. van Ravenzwaaij et al., 2019).
The data were obtained from Steiner et al. (2015):

``` r
mod_equiv_sum <- equiv_bf(
  n_x = 560,
  n_y = 538,
  mean_x = 8.683,
  mean_y = 8.516,
  sd_x = 3.6,
  sd_y = 3.6
)

mod_equiv_sum
#> ******************************
#> Equivalence analysis
#> --------------------
#> Data:                         summary data
#> H0 (equivalence):             mu_y == mu_x
#> H1 (non-equivalence):         mu_y != mu_x
#> Equivalence interval:         Lower = -0.00; Upper = 0.00 (standardised)
#>                               Lower = -0.00; Upper = 0.00 (unstandardised)
#> Cauchy prior scale:           0.707
#> 
#>     BF01 (equivalence) = 11.05
#> ******************************

get_bf(object = mod_equiv_sum)
#> [1] 11.04945
```

## The non-inferiority test (`infer_bf()`)

With `infer_bf()` we can test whether the experimental group is not
worse by a certain amount–which is given by the non-inferiority
margin–than the control group. Importantly, sometimes low and sometimes
high values on the dependent variable represent non-inferiority, which
can be specified with the argument `direction`. In the case where low
values represent non-inferiority we have BF-+, indicating that we
quantify evidence for the data under the negative alternative hypothesis
(i.e., H-) relative to the positive null hypothesis (i.e., H+). In the
case where high values represent non-superiority we have BF+-,
indicating that we quantify evidence for the data under the positive
alternative hypothesis (i.e., H+) relative to the negative null
hypothesis (i.e., H-). The default is that high values represent
non-inferiority. The non-inferiority margin can be specified with the
argument `ni_margin`. The argument `ni_margin_std` can be used to
specify whether the non-inferiority margin is given in standardized
(TRUE; the default) or unstandardized (FALSE) units.

We can use the raw data to compute a Bayes factor:

``` r
mod_infer_raw <- infer_bf(
  x = data$dv[data$condition == "con"],
  y = data$dv[data$condition == "exp"],
  ni_margin = 1.5,
  ni_margin_std = FALSE
)

mod_infer_raw
#> ******************************
#> Non-inferiority analysis
#> ------------------------
#> Data:                         raw data
#> H- (inferiority):             mu_y - mu_x < -ni_margin
#> H+ (non-inferiority):         mu_y - mu_x > -ni_margin
#> Non-inferiority margin:       0.45 (standardised)
#>                               1.50 (unstandardised)
#> Cauchy prior scale:           0.707
#> 
#>     BF+- (non-inferiority) = 6.85e+10
#> ******************************

get_bf(object = mod_infer_raw)
#> [1] 68546673627
```

Alternatively, if the raw data are not available, we can use summary
statistics to compute a Bayes factor (cf. van Ravenzwaaij et al., 2019).
The data were obtained from Andersson et al. (2013):

``` r
mod_infer_sum <- infer_bf(
  n_x = 33,
  n_y = 32,
  mean_x = 17.1,
  mean_y = 13.6,
  sd_x = 8,
  sd_y = 9.8,
  ni_margin = 2,
  ni_margin_std = FALSE,
  direction = "low"
)

mod_infer_sum
#> ******************************
#> Non-inferiority analysis
#> ------------------------
#> Data:                         summary data
#> H+ (inferiority):             mu_y - mu_x > ni_margin
#> H- (non-inferiority):         mu_y - mu_x < ni_margin
#> Non-inferiority margin:       0.22 (standardised)
#>                               2.00 (unstandardised)
#> Cauchy prior scale:           0.707
#> 
#>     BF-+ (non-inferiority) = 79.59
#> ******************************

get_bf(object = mod_infer_sum)
#> [1] 79.59441
```

## References

Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2020). Informed Bayesian
t-tests. *The American Statistician*, *74*(2), 137-143.

Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G.
(2009). Bayesian t tests for accepting and rejecting the null
hypothesis. *Psychonomic Bulletin & Review*, *16*(2), 225-237.

Schönbrodt, F. D., Wagenmakers, E.-J., Zehetleitner, M., & Perugini, M.
(2017). Sequential hypothesis testing with Bayes factors: Efficiently
testing mean differences. *Psychological Methods*, *22*(2), 322-339.

van Ravenzwaaij, D., Monden, R., Tendeiro, J. N., & Ioannidis, J. P. A.
(2019). Bayes factors for superiority, non-inferiority, and equivalence
designs. *BMC Medical Research Methodology*, *19*(1), 71.

Wagenmakers, E.-J., Marsman, M., Jamil, T., Ly, A., Verhagen, J., Love,
J., … Morey, R. D. (2018). Bayesian inference for psychology. Part I:
Theoretical advantages and practical ramifications. *Psychonomic
Bulletin & Review*, *25*(1), 35-57.
