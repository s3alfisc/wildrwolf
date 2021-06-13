
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rwolf

The rwolf package implements Romano-Wolf multiple-hypothesis-adjusted
p-values for objects of type `fixest_multi` from the `fixest` package
(currently for one-way clustered inference).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("s3alfisc/rwolf")
```

## Example

``` r
library(fixest)
library(rwolf)

set.seed(123)
N <- 1000
X1 <- rnorm(N)
Y1 <- 1 + 0.02 * X1 + rnorm(N)
Y2 <- 1 + 0.01 * X1 + rnorm(N)
cluster <- rep(1:50, 1000 / 50)

data <- data.frame(Y1 = Y1, 
                   Y2 = Y2, 
                   X1 = X1, 
                   cluster = cluster)

res <- feols(c(Y1, Y2) ~ X1, data = data, cluster = ~ cluster)
class(res)
#> [1] "fixest_multi"
summary(res)
#> Standard-errors: Clustered (cluster) 
#> Dep. var.: Y1
#>             Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) 1.041000   0.032915 31.6280 < 2.2e-16 ***
#> X1          0.108047   0.033791  3.1976  0.002429 ** 
#> ---
#> Dep. var.: Y2
#>              Estimate Std. Error   t value  Pr(>|t|)    
#> (Intercept)  0.980195   0.026814 36.555000 < 2.2e-16 ***
#> X1          -0.009070   0.031132 -0.291323  0.772034

rwolf_res <- rwolf(models = res, parameter = "X1", B = 100)
rwolf_res$models_info
#>    depvar     Estimate Std. Error    t value   Pr(>|t|) RW Pr(>|t|)
#> 1:     Y1  0.108047294 0.03379059  3.1975558 0.00242896           0
#> 2:     Y2 -0.009069571 0.03113237 -0.2913228 0.77203411           1
```
