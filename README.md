# Biostat615PMM

## Installation

The package can be installed using `devtools::install_github("jsdrquq/Biostat615PMM")`.

## Creating simulated datasets

The `create_norm_miss_data()` function can create simulated dataset with two variables $x$ and $y$ that satisfy $y_i = \alpha + \beta x_i + \epsilon_i, \epsilon_i \sim \mathrm{N}(0, \sigma^2)$.
$\alpha$, $\beta$, and $\sigma^2$ can be adjusted via parameters `alpha`, `beta`, and `sigma2` respectively.
If `is_mar` is `TRUE`, the missing data will follow the missing at random (MAR) mechanism, where the probability of being missing depends on observed data $x$.
If not, they will follow the missing completely at random (MCAR) mechanism, where the probability of being missing is completely independent of the data.
The proportion of missing values is controlled by parameter `prop_miss` and the sample size by `n`. For example,

```r
create_norm_miss_data(prop_miss = 0.1, alpha = 3, beta = 5, sigma2 = 1, is_mar = TRUE, n = 500)
```

## Missing data imputation through PMM

The function used to impute missing data is `pmm_impute()`.
While imputing, the outcome variable, the design matrix, and the locations of data to impute should be assigned to parameters `y`, `x`, and `ry`.
`bayeswt` determines which kind of regression weights to apply: Bayesian (`TRUE`) or Bootstrap (`FALSE`).
The size of donor pool is specified by `donors`, which is generally an integer between 1 and 10.
`matchtype` can be used to choose the type of matching, which should be selected among 0, 1, and 2.
If there are $n_0$ values that need to be imputed, the output of this function will be a vector of imputed values of length $n_0$.

```r
pmm_impute(y = data$y, ry = !is.na(data$y), x = data$x, bayeswt = TRUE, donors = 5, matchtype = 2)
```

## Evaluation

The function `get_eval_results()` encompasses several functions used to conduct simulations and evaluations on assigned imputation tasks.
Since the coefficient we plan to examine in this project is the regression slope (i.e., `beta` in `create_norm_miss_data()`), `true_coef` should be the true value of the slope.
`donors` and `matchtype` share the same meanings as `pmm_impute()`. `simu_times` determines the number of simulations to carry out.

```r
get_eval_results(data = dt, true_coef = 5, donors = 5, matchtype = 2, simu_times = 200)
```
