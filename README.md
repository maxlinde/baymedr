
# baymedr: BAYesian inference for MEDical designs in R

`baymedr` is an R package with the goal of providing researchers with
tools for the computation of Bayes factors for common biomedical
research designs (see van Ravenzwaaij et al., 2019). Implemented are
functions to test the equivalence (`equiv_bf()`), non-inferiority
(`infer_bf()`), and superiority (`super_bf()`) of an experimental group
(e.g., a new medication) compared to a control group (e.g., a placebo or
an already existing medication). A special focus of `baymedr` lies on a
user-friendly interface, so that a wide variety or researchers (i.e.,
not only statisticians) can utilise `baymedr` for their analyses.

The Bayesian approach to inference has several advantages over the
conventional frequentist approach. To mention only a few, with Bayesian
inference it is legitimate to monitor results during data collection and
decide to stop or continue data collection based on the inspection of
interim analyses. This is considered a bad practice within the
frequentist framework because it would result in an inflated Type I
error rate (e.g., Schönbrodt et al., 2017). Furthermore, null hypothesis
significance tests and the corresponding *p*-values do not allow for the
quantification of evidence for the null hypothesis (e.g., Wagenmakers et
al., 2018). The Bayesian framework remedies this shortcoming, which is
particularly important for the equivalence design (van Ravenzwaaij et
al., 2019). Lastly, in some situations the frequentist approaches to
equivalence and non-inferiority tests bear certain interpretational
ambiguities. For instance, when the confidence interval of the
difference between the two group means fully lies between the
non-inferiority margin and 0, this means that the experimental group is
non-inferior with regard to the non-inferiority margin but inferior with
regard to 0. The same applies to the equivalence design (van Ravenzwaaij
et al., 2019). Fortunately, these ambiguities are fully resolved within
the Bayesian framework. For a more thorough discussion of Bayesian
advantages, see, for example, Wagenmakers et al. (2018).

## Installation

You can install the latest development version of `baymedr` from
[GitHub](https://github.com/), using the `devtools` package, with:

``` r
# install.packages("devtools")
devtools::install_github("maxlinde/baymedr")
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
refer to the experimental group.

Usage of the functions for equivalence (`equiv_bf()`), non-inferiority
(`infer_bf()`), and superiority designs (`super_bf()`), results in S4
objects of classes ‘baymedrEquivalence’, ‘baymedrNonInferiority’, and
‘baymedrSuperiority’, respectively. Summary information are shown in
the console by printing the created S4 object. To extract the Bayes
factor from one of the three S4 objects, use the function `get_bf()`.

## The Cauchy prior distribution

Bayesian inference requires the specification of a prior distribution,
which mirrors prior beliefs about the likelihood of parameter values.
For the equivalence, non-inferiority, and superiority tests, the
parameter of interest is the effect size between the experimental and
control conditions (see Rouder et al., 2009). If relevant information is
available, this knowledge could be expressed in an idiosyncratic prior
distribution. Most of the time, however, relevant information is
missing. In that case, it is reasonable to define a prior distribution
that is as objective as possible. It has been argued that the Cauchy
probability density function represents such a function (see, e.g.,
Rouder et al., 2009). The standard Cauchy distribution resembles a
standard Normal distribution, except that the Cauchy distribution has
less mass at the centre but instead heavier tails. The centre of the
distribution is determined by the location parameter, while the width is
specified by the scale parameter. By varying the scale of the Cauchy
prior, the user can change the range of reasonable effect sizes. This is
accomplished with the argument `prior_scale`.

## Random example data

In order to demonstrate the three functions from `baymedr`, we create an
example dataset (data). There is a control group “con” and an
experimental group “exp” (condition). Further, random numbers, sampled
from the normal distribution, within each group are created, serving as
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
specified with the argument `direction`. The default is that high values
represent superiority. Moreover, research practices diverge on whether
one-tailed test should be conducted or a two-tailed test with subsequent
confirmation that the results follow the expected direction. This can be
specified with the argument `alternative`, for which the default is a
one-sided test.

We can use the raw data to compute a Bayes factor:

``` r
library(baymedr)
```

``` r
mod_super_raw <- super_bf(
  x = data$dv[data$condition == "con"],
  y = data$dv[data$condition == "exp"]
)

mod_super_raw
#> ******************************
#> Superiority analysis
#> --------------------
#> Data: raw data
#> H0 (non-superiority): mu_y == mu_x
#> H1 (superiority):     mu_y > mu_x
#> Prior scale: 0.707
#> 
#>     BF10 (superiority) = 44.00176
#> ******************************

get_bf(object = mod_super_raw)
#> [1] 44.00176
```

Alternatively, if the raw data are not available, we can use summary
statistics to compute a Bayes factor (cf. van Ravenzwaaij et al., 2019).
The data was obtained from Skjerven et al. (2013):

``` r
library(baymedr)
```

``` r
mod_super_sum <- super_bf(
  n_x = 201,
  n_y = 203,
  mean_x = 68.1,
  mean_y = 63.6,
  ci_margin = (15.5 - (-6.5)) / 2,
  ci_level = 0.95,
  direction = "low",
  alternative = "one.sided"
)

mod_super_sum
#> ******************************
#> Superiority analysis
#> --------------------
#> Data: summary data
#> H0 (non-superiority): mu_y == mu_x
#> H1 (superiority):     mu_y < mu_x
#> Prior scale: 0.707
#> 
#>     BF10 (superiority) = 0.2364177
#> ******************************

get_bf(object = mod_super_sum)
#> [1] 0.2364177
```

## The equivalence test (`equiv_bf()`)

With `equiv_bf()` we can test whether the experimental group and the
control group are equivalent. With the argument `interval`, an
equivalence interval can be specified. However, in contrast to the
frequentist equivalence test, `equiv_bf()` can also incorporate a point
null hypothesis, which constitutes the default in `equiv_bf()` (i.e.,
`interval` = 0).

We can use the raw data to compute a Bayes factor:

``` r
library(baymedr)
```

``` r
mod_equiv_raw <- equiv_bf(
  x = data$dv[data$condition == "con"],
  y = data$dv[data$condition == "exp"]
)

mod_equiv_raw
#> ******************************
#> Equivalence analysis
#> --------------------
#> Data: raw data
#> H0 (equivalence):     mu_y == mu_x
#> H1 (non-equivalence): mu_y != mu_x
#> Equivalence interval: Lower = 0; Upper = 0
#> Prior scale: 0.707
#> 
#>     BF01 (equivalence) = 0.04542366
#> ******************************

get_bf(object = mod_equiv_raw)
#> [1] 0.04542366
```

Alternatively, if the raw data are not available, we can use summary
statistics to compute a Bayes factor (cf. van Ravenzwaaij et al., 2019).
The data was obtained from Steiner et al. (2015):

``` r
library(baymedr)
```

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
#> Data: summary data
#> H0 (equivalence):     mu_y == mu_x
#> H1 (non-equivalence): mu_y != mu_x
#> Equivalence interval: Lower = 0; Upper = 0
#> Prior scale: 0.707
#> 
#>     BF01 (equivalence) = 11.04945
#> ******************************

get_bf(object = mod_equiv_sum)
#> [1] 11.04945
```

## The non-inferiority test (`infer_bf()`)

With `infer_bf()` we can test whether the experimental group is worse by
a certain amount - which is given by the non-inferiority margin - than
the the control group. Importantly, sometimes low and sometimes high
values on the measure of interest represent non-inferiority, which can
be specified with the argument `direction`. The default is that high
values represent non-inferiority. The non-inferiority margin can be
specified with the argument `ni_margin`.

We can use the raw data to compute a Bayes factor:

``` r
library(baymedr)
```

``` r
mod_infer_raw <- infer_bf(
  x = data$dv[data$condition == "con"],
  y = data$dv[data$condition == "exp"],
  ni_margin = 1.5
)

mod_infer_raw
#> ******************************
#> Non-inferiority analysis
#> ------------------------
#> Data: raw data
#> H0 (inferiority):     mu_y - mu_x < ni_margin
#> H1 (non-inferiority): mu_y - mu_x > ni_margin
#> Non-inferiority margin: 1.5
#> Prior scale: 0.707
#> 
#>     BF10 (non-inferiority) = 31818926201
#> ******************************

get_bf(object = mod_infer_raw)
#> [1] 31818926201
```

Alternatively, if the raw data are not available, we can use summary
statistics to compute a Bayes factor (cf. van Ravenzwaaij et al., 2019).
The data was obtained from Andersson et al. (2013):

``` r
library(baymedr)
```

``` r
mod_infer_sum <- infer_bf(
  n_x = 33,
  n_y = 32,
  mean_x = 17.1,
  mean_y = 13.6,
  sd_x = 8,
  sd_y = 9.8,
  ni_margin = 2,
  direction = "low"
)

mod_infer_sum
#> ******************************
#> Non-inferiority analysis
#> ------------------------
#> Data: summary data
#> H0 (inferiority):     mu_y - mu_x > ni_margin
#> H1 (non-inferiority): mu_y - mu_x < ni_margin
#> Non-inferiority margin: 2
#> Prior scale: 0.707
#> 
#>     BF10 (non-inferiority) = 90.51541
#> ******************************

get_bf(object = mod_infer_sum)
#> [1] 90.51541
```

## Note

At the present stage in the development of `baymedr`, it is only
possible to conduct superiority, equivalence, and non-inferiority
designs for independent-groups designs. In the future, the option to
conduct these tests for dependent-groups designs will be implemented.

## References

Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2019). Informed bayesian
t-tests. *The American Statistician*.

Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G.
(2009). Bayesian t tests for accepting and rejecting the null
hypothesis. *Psychonomic Bulletin & Review*, *16*(2), 225-237.

Schönbrodt, F. D., Wagenmaker, E.-J., Zehetleitner, M., & Perugini, M.
(2017). Sequential hypothesis testing with bayes factors: Efficiently
testing mean differences. *Psychological Methods*, *22*(2), 322-339.

van Ravenzwaaij, D., Monden, R., Tendeiro, J. N., & Ioannidis, J. P. A.
(2019). Bayes factors for superiority, non-inferiority, and equivalence
designs. *BMC Medical Research Methodology*, *19*(1), 71.

Wagenmakers, E.-J., Marsman, M., Jamil, T., Ly, A., Verhagen, J., Love,
J., … Morey, R. D. (2018). Bayesian inference for psychology. Part I:
Theoretical advantages and practical ramifications. *Psychonomic
Bulletin & Review*, *25*(1), 35-57.
