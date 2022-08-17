
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statBasics

`R` package of the course **Métodos Estatísticos** at Federal University
of Bahia.

## Confidence interval for a single population parameter

Confidence interval are computed using the methods presented by
Montgomery and Runger (2010). All implemented methods are slighted
modification of methods already implemented in `stats`. The user can
compute bilateral and unilateral confidence intervals.

### Confidence interval for a population proportion

In this package, there are three approaches for computing a confidence
interval for a population proportion.

#### Number of successes in `n` (scalar value) trials

``` r
library(tidyverse)
library(statBasics)
size  <- 1000
sample <- rbinom(size, 1, prob = 0.5)
n_success <- sum(sample)
ci_1pop_bern(n_success, size, conf_level = 0.99)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.475    0.557       0.99
```

#### Number of successes in a vector

``` r
library(tidyverse)
library(statBasics)
n <- c(30, 20, 10)
x <- n |> map_int(~ sum(rbinom(1, size = .x, prob = 0.75)))
ci_1pop_bern(x, n, conf_level = 0.99)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.500    0.833       0.99
```

#### Vector of successes

``` r
library(tidyverse)
library(statBasics)
x <- rbinom(50, size = 1, prob = 0.75)
ci_1pop_bern(x, conf_level = 0.99)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.658        1       0.99
```

### Confidence interval for a populationa mean (normal distribution)

We illustrate how to compute a confidence interval for the mean of a
normal distribution in two cases: 1) the standard deviation is known; 2)
the standard deviation is unknown.

#### Known standard deviation

``` r
library(tidyverse)
library(statBasics)
media_pop <- 10
sd_pop <- 2
x <- rnorm(100, mean = media_pop, sd = sd_pop)
ci_1pop_norm(x, sd_pop = sd_pop, conf_level = 0.91)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     9.62     10.3       0.91
```

#### Unknown standard deviation

``` r
library(tidyverse)
library(statBasics)
media_pop <- 10
sd_pop <- 2
x <- rnorm(100, mean = media_pop)
ci_1pop_norm(x, conf_level = 0.91)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     9.73     10.1       0.91
```

### Confidence interval for a population standard deviation (normal distribution)

``` r
library(tidyverse)
library(statBasics)
media_pop <- 10
sd_pop <- 2
x <- rnorm(100, mean = media_pop, sd = sd_pop)
ci_1pop_norm(x, parameter = 'variance', conf_level = 0.91)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     3.92     6.37       0.91
```

### Confidence interval for a population mean (exponential distribution)

``` r
library(tidyverse)
library(statBasics)
media_pop <- 800
taxa_pop <- 1 / media_pop
x <- rexp(100, rate = taxa_pop)
ci_1pop_exp(x)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     739.    1095.       0.95
```

### Confidence interval for a population mean (general case)

In the general case, a confidence interval using the *t-Student*
distribution is still suitable, even if the distribution is not normal,
as illustrated in the example bellow.

``` r
library(tidyverse)
library(statBasics)
media_pop <- 50
x <- rpois(100, lambda  = media_pop)
ci_1pop_general(x)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     47.8     50.7       0.95
```

## Hypothesis testing for a single population parameter

Next, we will illustrate how to use this package to test a statistical
hypothesis about a single population parameter. All methods are already
implemented in `R`. This package provides slight modifications for
teaching purposes.

### Hypothesis testing for a population mean

In the examples below, `mean_null` is the mean in the null hypothesis
`H0`:

1.  `alternative == "two.sided"`: `H0: mu == mean_null` and
    `H1: mu != mean_null`. Default value.
2.  `alternative == "less"`: `H0: mu >= mean_null` and
    `H1: mu < mean_null`
3.  `alternative == "greater"`: `H0: mu =< mean_null` and
    `H1: mu > mean_null`

#### Normal distribution with known standard deviation

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
#> 1      24.4       0           1.96 (-Inf,-1.960)U(1… two.sided       5      0.05
#> # … with 3 more variables: lower_ci <dbl>, upper_ci <dbl>, conf_level <dbl>
```

#### Normal distribution with unknown standard deviation

``` r
library(tidyverse)
library(statBasics)
mean_null <- 5
sd_pop <- 2
x <- rnorm(100, mean = 10, sd = sd_pop)
ht_1pop_mean(x, mu = mean_null, conf_level = 0.95, alternative = "two.sided")
#> # A tibble: 1 × 10
#>   statistic p_value critical_value critical_region   alternative    mu sig_level
#>       <dbl>   <dbl>          <dbl> <chr>             <chr>       <dbl>     <dbl>
#> 1      24.5       0           1.98 (-Inf,-1.984)U(1… two.sided       5      0.05
#> # … with 3 more variables: lower_ci <dbl>, upper_ci <dbl>, conf_level <dbl>
```

### Hypothesis testing for a population standard deviation

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
#> 1      27.1 8.75e-14           73.4 (0,73.361)U(128… two.sided       4      0.05
#> 2      27.1 8.75e-14          128.  (0,73.361)U(128… two.sided       4      0.05
#> # … with 3 more variables: lower_ci <dbl>, upper_ci <dbl>, conf_level <dbl>
```

### Hypothesis testing for a population proportion

In the examples below, `proportion_null` is the proportion in the null
hypothesis `H0`:

1.  `alternative == "two.sided"`: `H0: proportion == proportion_null`
    and `H1: proportion != proportion_null`. Default value.
2.  `alternative == "less"`: `H0: proportion >= proportion_null` and
    `H1: proportion < proportion_null`
3.  `alternative == "greater"`: `H0: proportion =< proportion_null` and
    `H1: proportion > proportion_null`

#### Number of successes

The following example illustrates how to perform a hypothesis test when
the number of successes (in a number of trials) is a scalar.

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
#> 1      1.61   0.108           1.96 (-Inf,-1.960)U(1.960,… two.sided         0.75
#> # … with 4 more variables: sig_level <dbl>, lower_ci <dbl>, upper_ci <dbl>,
#> #   conf_level <dbl>
```

#### Number of successes in a vector

The example below shows how to perform a hypothesis test when the number
of successes (in a number of trials) is a vector. The vector of number
of trials must also be provided.

``` r
library(tidyverse)
library(statBasics)
proportion_null <- 0.9
p0 <- 0.75
n <- c(10, 20, 30)
x <- n |> map_int(~ rbinom(1, .x, prob = p0))
ht_1pop_prop(x, n, proportion = p0, alternative = "less", conf_level = 0.99)
#> # A tibble: 1 × 10
#>   statistic p_value critical_value critical_region alternative proportion
#>       <dbl>   <dbl>          <dbl> <chr>           <chr>            <dbl>
#> 1     -2.98 0.00143          -1.64 (-Inf,-1.645)   less              0.75
#> # … with 4 more variables: sig_level <dbl>, lower_ci <dbl>, upper_ci <dbl>,
#> #   conf_level <dbl>
```

#### Vector of successes (0 or 1)

The following example shows how to perform a hypothesis test when the
number of successes (in a number of trials) is a vector of zeroes and
ones.

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
#> 1    -0.803   0.789           1.64 (1.645, Inf)    greater           0.75
#> # … with 4 more variables: sig_level <dbl>, lower_ci <dbl>, upper_ci <dbl>,
#> #   conf_level <dbl>
```

## Confidence interval for two populations

### Bernoulli distribution

In this package, there are two approaches for computing a confidence
interval for the difference in proportions.

#### Confidence Interval – Number of successes

In this case, we have the number of trials (`n_x` and `n_y`) and the
number of success (`x` and `y`) for both popuations.

``` r
x <- 3
n_x <- 100
y <- 50
n_y <- 333
ci_2pop_bern(x, y, n_x, n_y)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1   -0.232 -0.00840       0.95
```

#### Confidence Interval – Vectors of 0 and 1

In this case, we have a vector of 0 and 1 (`x` and `y`) for both
populations.

``` r
x <- rbinom(100, 1, 0.75)
y <- rbinom(500, 1, 0.75)
ci_2pop_bern(x, y)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1   -0.119   0.0954       0.95
```

### Normal distribution

In this case, we can build the interval for the difference in means of
two populations with known or unknown standard deviations, and we can
build the ratio of the variances of two populations. DÚVIDA!

#### Confidence Interval – Comparing means when standard deviations are unknown

Next, we illustrate how to compute a confidence interval for the
difference in means of two populations when population standard
deviations are unknown.

``` r
x <- rnorm(1000, mean = 0, sd = 2)
y <- rnorm(1000, mean = 0, sd = 1)
# unknown variance and confidence interval for difference of means
ci_2pop_norm(x, y)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1   -0.178   0.0962       0.95
```

#### Confidence Interval – Comparing means when standard deviations are known

The example below illustrates how to obtain a confidence interval for
the difference in means of two populations when population standard
deviations are known.

``` r
x <- rnorm(1000, mean = 0, sd = 2)
y <- rnorm(1000, mean = 0, sd = 3)
# known variance and confidence interval for difference of means
ci_2pop_norm(x, y, sd_pop_1 = 2, sd_pop_2 = 3)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1   -0.309    0.138       0.95
```

#### Confidence Interval – Comparing standard deviations

In thsi case, a confidence interval is obtained by considering the ratio
of standard deviations (or variances) of the two populations.

``` r
x <- rnorm(1000, mean = 0, sd = 2)
y <- rnorm(1000, mean = 0, sd = 3)
# confidence interval for the variance ratio of 2 populations
ci_2pop_norm(x, y, parameter = "variance")
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.360    0.461       0.95
```

## Hypothesis testing for two populations

### Comparing proportions in two populations

There two approaches to compare the proportions in two populations:

-   We have the numbers of sucecss (`x` and `y`) and the numbers of
    trials (`n_x` and `n_y`) for both populations;
-   We have vector of 1 (success) and 0 (failure) for both populations.

#### Vector of 1 and 0

In this case, we have a vector of 1 (success) and 0 (failure) for both
populations.

``` r
x <- rbinom(100, 1, 0.75)
y <- rbinom(500, 1, 0.75)
ht_2pop_prop(x, y)
#> [1] FALSE
#> # A tibble: 1 × 6
#>   statistic p_value critical_value critical_region           delta alternative
#>       <dbl>   <dbl>          <dbl> <chr>                     <dbl> <chr>      
#> 1      1.23   0.220           1.96 (-Inf,-1.960)U(1.960,Inf)     0 two.sided
```

#### Number of successes

In this case, we have the number of success and the number of trials for
both populations.

``` r
x <- 3
n_x <- 100
y <- 50
n_y <- 333
ht_2pop_prop(x, y, n_x, n_y)
#> [1] FALSE
#> # A tibble: 1 × 6
#>   statistic p_value critical_value critical_region           delta alternative
#>       <dbl>   <dbl>          <dbl> <chr>                     <dbl> <chr>      
#> 1     -3.21 0.00131           1.96 (-Inf,-1.960)U(1.960,Inf)     0 two.sided
```

## Hypothesis testing for comparing the means of two independent populations

There are three cases to be considere when comparing two means:

1.  `t-test` unknown but equal variances;
2.  `t-test` unknown and unequal variances;
3.  `z-test` known variances.

### Comparing two means – unknown, equal variances (`t-test`)

``` r
x <- rnorm(1000, mean = 10, sd = 2)
y <- rnorm(500, mean = 5, sd = 2)
# H0: mu_1 - mu_2 == -1 versus H1: mu_1 - mu_2 != -1
ht_2pop_mean(x, y, delta = -1, var_equal = TRUE)
#> # A tibble: 1 × 6
#>   statistic p_value critical_value critical_region            delta alternative
#>       <dbl>   <dbl>          <dbl> <chr>                      <dbl> <chr>      
#> 1      54.3       0           1.96 (-Inf,-1.962)U(1.962, Inf)    -1 two.sided
```

### Comparing two means – unknown, unequal variances (`t-test`)

``` r
x <- rnorm(1000, mean = 10, sd = 2)
y <- rnorm(500, mean = 5, sd = 1)
# H0: mu_1 - mu_2 == -1 versus H1: mu_1 - mu_2 != -1
ht_2pop_mean(x, y, delta = -1)
#> # A tibble: 1 × 6
#>   statistic p_value critical_value critical_region            delta alternative
#>       <dbl>   <dbl>          <dbl> <chr>                      <dbl> <chr>      
#> 1      77.8       0           1.96 (-Inf,-1.962)U(1.962, Inf)    -1 two.sided
```

### Comparing two means – known variances (`z-test`)

``` r
x <- rnorm(1000, mean = 10, sd = 3)
x <- rnorm(500, mean = 5, sd = 1)
# H0: mu_1 - mu_2 >= 0 versus H1: mu_1 - mu_2 < 0
ht_2pop_mean(x, y, delta = 0, sd_pop_1 = 3, sd_pop_2 = 1, alternative = "less")
#> # A tibble: 1 × 6
#>   statistic p_value critical_value critical_region delta alternative
#>       <dbl>   <dbl>          <dbl> <chr>           <dbl> <chr>      
#> 1    -0.368   0.357          -1.64 (-Inf, -1.645)      0 less
```

## Hypothesis testing for comparing variances of two independet populations

``` r
x <- rnorm(100, sd = 2)
y <- rnorm(1000, sd = 10)
ht_2pop_var(x, y)
#> # A tibble: 2 × 7
#>   statistic  p_value critical_vale ratio alternative lower_ci upper_ci
#>       <dbl>    <dbl>         <dbl> <dbl> <chr>          <dbl>    <dbl>
#> 1    0.0532 1.86e-43         0.733     1 two.sided     0.0535   0.0535
#> 2    0.0532 1.86e-43         1.32      1 two.sided     0.0535   0.0535
```
