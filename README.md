
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statBasics

`R` package of the course **Métodos Estatísticos** at Federal University
of Bahia.

## Confidence Interval

Here, we compute the confidence interval using the methods as presented
by Montgomery and Runger (2010). All methods implemented are slighted
modification of methods already implemented in `stats`. The user can
compute bilateral and unilateral confidence intervals.

### Confidence Interval for Proportion

There are three approaches to compute confidence interval for proportion
in this package.

#### Number of success in `n` (scalar value) trials

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
#> ✓ tibble  3.1.6     ✓ dplyr   1.0.8
#> ✓ tidyr   1.2.0     ✓ stringr 1.4.0
#> ✓ readr   2.1.2     ✓ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(statBasics)
size  <- 1000
sample <- rbinom(size, 1, prob = 0.5)
n_success <- sum(sample)
ci_bern(n_success, size, conf_level = 0.99)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.469    0.551       0.99
```

#### Number of success in a vector

``` r
library(tidyverse)
library(statBasics)
n <- c(30, 20, 10)
x <- n %>% map_int(~ sum(rbinom(1, size = .x, prob = 0.75)))
ci_bern(x, n, conf_level = 0.99)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.567    0.900       0.99
```

#### Vector of success

``` r
library(tidyverse)
library(statBasics)
x <- rbinom(50, size = 1, prob = 0.75)
ci_bern(x, conf_level = 0.99)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.618    0.982       0.99
```

### Confidence interval of mean (normal Distribution)

Let’s illustrated with a simulated cases the confidence interval for two
cases: 1) standard deviation is known; 2) standard deviation is not
known.

#### Standard Deviation is unknown

``` r
library(tidyverse)
library(statBasics)
media_pop <- 10
sd_pop <- 2
x <- rnorm(100, mean = media_pop)
ci_norm(x, conf_level = 0.91)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     9.65     9.95       0.91
```

#### Standard Deviation is known

``` r
library(tidyverse)
library(statBasics)
media_pop <- 10
sd_pop <- 2
x <- rnorm(100, mean = media_pop, sd = sd_pop)
ci_norm(x, sd_pop = sd_pop, conf_level = 0.91)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     9.70     10.4       0.91
```

### Confidence interval for standard deviation (normal distribution)

``` r
library(tidyverse)
library(statBasics)
media_pop <- 10
sd_pop <- 2
x <- rnorm(100, mean = media_pop, sd = sd_pop)
ci_norm(x, parameter = 'variance', conf_level = 0.91)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     2.73     4.43       0.91
```

### Confidence interval for mean (exponential distribution)

``` r
library(tidyverse)
library(statBasics)
media_pop <- 800
taxa_pop <- 1 / media_pop
x <- rexp(100, rate = taxa_pop)
ci_exp(x)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     656.     972.       0.95
```

### Confidence interval for mean

Confidence interval using *t* distribution is suitable, even the
distribution is not normal, as illustrated in the example bellow.

``` r
library(tidyverse)
library(statBasics)
media_pop <- 50
x <- rpois(100, lambda  = media_pop)
ci_general(x)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     48.7     51.5       0.95
```

## Hypothesis testing

In the following, we will illustrate the use of this package to test
scientific hypothesis for one sample with examples. All methods are
already implemented in `R`, I have only made slightly modifications to
teaching purposes.

### Hypothesis testing for mean

In the examples below, `mean_null` is the mean in the null hypothesis
`H0`:

1.  `alternative == "two.sided"`: `H0: mu == mean_null` and
    `H1: mu != mean_null`. Default value.
2.  `alternative == "less"`: `H0: mu >= mean_null` and
    `H1: mu < mean_null`
3.  `alternative == "greater"`: `H0: mu =< mean_null` and
    `H1: mu > mean_null`

#### Normal distribution with known variance

``` r
library(tidyverse)
library(statBasics)
mean_null <- 5
sd_pop <- 2
x <- rnorm(100, mean = 10, sd = sd_pop)
ht_1pop_mean(x, mu = mean_null, conf_level = 0.95, sd_pop = sd_pop, alternative = "two.sided")
#> # A tibble: 1 × 10
#>   statistic p_value critical_value critical_region   alternative    mu sig_level
#>       <dbl>   <dbl>          <dbl> <chr>             <chr>       <dbl>     <dbl>
#> 1      25.0       0           1.96 (-Inf,-1.960)U(1… two.sided       5      0.05
#> # … with 3 more variables: lower_ci <dbl>, upper_ci <dbl>, conf_level <dbl>
```

#### Normal distribution with unknown variance

``` r
library(tidyverse)
library(statBasics)
mean_null <- 5
sd_pop <- 2
x <- rnorm(100, mean = 10, sd = sd_pop)
ht_1pop_mean(x, mu = mean_null, conf_level = 0.95, sd_pop = sd_pop, alternative = "two.sided")
#> # A tibble: 1 × 10
#>   statistic p_value critical_value critical_region   alternative    mu sig_level
#>       <dbl>   <dbl>          <dbl> <chr>             <chr>       <dbl>     <dbl>
#> 1      24.9       0           1.96 (-Inf,-1.960)U(1… two.sided       5      0.05
#> # … with 3 more variables: lower_ci <dbl>, upper_ci <dbl>, conf_level <dbl>
```

### Hypothesis testing for variance

In the examples below, `sigma_null` is the standard deviation in the
null hypothesis `H0`:

1.  `alternative == "two.sided"`: `H0: sigma == sigma_null` and
    `H1: sigma != sigma_null`. Default value.
2.  `alternative == "less"`: `H0: sigma >= sigma_null` and
    `H1: sigma < sigma_null`
3.  `alternative == "greater"`: `H0: sigma =< sigma_null` and
    `H1: sigma > sigma_null`

``` r
library(tidyverse)
library(statBasics)
sigma_null <- 4
sd_pop <- 2
x <- rnorm(100, mean = 10, sd = sd_pop)
ht_1pop_var(x, sigma = sigma_null, conf_level = 0.95, alternative = "two.sided")
#> # A tibble: 2 × 10
#>   statistic  p_value critical_value critical_region  alternative sigma sig_level
#>       <dbl>    <dbl>          <dbl> <chr>            <chr>       <dbl>     <dbl>
#> 1      20.0 8.96e-19           73.4 (0,73.361)U(128… two.sided       4      0.05
#> 2      20.0 8.96e-19          128.  (0,73.361)U(128… two.sided       4      0.05
#> # … with 3 more variables: lower_ci <dbl>, upper_ci <dbl>, conf_level <dbl>
```

### Hypothesis testing for proportion

In the examples below, `proportion_null` is the standard deviation in
the null hypothesis `H0`:

1.  `alternative == "two.sided"`: `H0: proportion == proportion_null`
    and `H1: proportion != proportion_null`. Default value.
2.  `alternative == "less"`: `H0: proportion >= proportion_null` and
    `H1: proportion < proportion_null`
3.  `alternative == "greater"`: `H0: proportion =< proportion_null` and
    `H1: proportion > proportion_null`

#### Number of success

You can have the number of success (in a number of trials) as a scalar.

``` r
library(tidyverse)
library(statBasics)
proportion_null <- 0.1
p0 <- 0.75
x <- rbinom(1, size = 1000, prob = p0)
ht_1pop_prop(x, 1000, proportion = p0, alternative = "two.sided", conf_level = 0.95)
#> # A tibble: 1 × 10
#>   statistic p_value critical_value critical_region        alternative proportion
#>       <dbl>   <dbl>          <dbl> <chr>                  <chr>            <dbl>
#> 1      1.24   0.214           1.96 (-Inf,-1.960)U(1.960,… two.sided         0.75
#> # … with 4 more variables: sig_level <dbl>, lower_ci <dbl>, upper_ci <dbl>,
#> #   conf_level <dbl>
```

#### Number of success in a vector

You can have the number of success as a vector. You must also give the
vector of number of trials.

``` r
library(tidyverse)
library(statBasics)
proportion_null <- 0.9
p0 <- 0.75
n <- c(10, 20, 30)
x <- n %>% map_int(~ rbinom(1, .x, prob = p0))
ht_1pop_prop(x, n, proportion = p0, alternative = "less", conf_level = 0.99)
#> # A tibble: 1 × 10
#>   statistic p_value critical_value critical_region alternative proportion
#>       <dbl>   <dbl>          <dbl> <chr>           <chr>            <dbl>
#> 1      1.79   0.963          -1.64 (-Inf,-1.645)   less              0.75
#> # … with 4 more variables: sig_level <dbl>, lower_ci <dbl>, upper_ci <dbl>,
#> #   conf_level <dbl>
```

#### Vector of success (0 or 1)

You can have the number of success (zero ou one).

``` r
library(tidyverse)
library(statBasics)
proportion_null <- 0.1
p0 <- 0.75
x <- rbinom(1000, 1, prob = p0)
ht_1pop_prop(x, proportion = p0, alternative = "greater", conf_level = 0.95)
#> # A tibble: 1 × 10
#>   statistic p_value critical_value critical_region alternative proportion
#>       <dbl>   <dbl>          <dbl> <chr>           <chr>            <dbl>
#> 1      1.17   0.121           1.64 (1.645, Inf)    greater           0.75
#> # … with 4 more variables: sig_level <dbl>, lower_ci <dbl>, upper_ci <dbl>,
#> #   conf_level <dbl>
```
