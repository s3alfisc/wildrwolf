
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wildrwolf 🐺

<!-- badges: start -->

[![R-CMD-check](https://github.com/s3alfisc/wildrwolf/workflows/R-CMD-check/badge.svg)](https://github.com/s3alfisc/wildrwolf/actions)
[![pkgcheck](https://github.com/s3alfisc/wildrwolf/workflows/pkgcheck/badge.svg)](https://github.com/s3alfisc/wildrwolf/actions?query=workflow%3Apkgcheck)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
![runiverse-package](https://s3alfisc.r-universe.dev/badges/wildrwolf)
[![Codecov test
coverage](https://codecov.io/gh/s3alfisc/wildrwolf/branch/main/graph/badge.svg)](https://app.codecov.io/gh/s3alfisc/wildrwolf?branch=main)
<!-- badges: end -->

The `wildrwolf` package implements Romano-Wolf
multiple-hypothesis-adjusted p-values for objects of type `fixest` and
`fixest_multi` from the `fixest` package via a wild (cluster) bootstrap.

Because the bootstrap-resampling is based on the
[fwildclusterboot](https://github.com/s3alfisc/fwildclusterboot)
package, `wildrwolf` is usually really fast.

The package is complementary to
[wildwyoung](https://github.com/s3alfisc/wildwyoung) (still work in
progress), which implements the multiple hypothesis adjustment method
following Westfall and Young (1993).

Adding support for multi-way clustering is work in progress.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("s3alfisc/wildrwolf")

# from r-universe (windows & mac, compiled R > 4.0 required)
install.packages('wildrwolf', repos ='https://s3alfisc.r-universe.dev')
```

## Example I

<!-- As you can see in the example, there seems to be a bug in `rwolf()` for the pairs bootstrap. -->

``` r
library(wildrwolf)
library(fixest)

set.seed(1412)

N <- 1000
X1 <- rnorm(N)
X2 <- rnorm(N)
rho <- 0.5
sigma <- matrix(rho, 4, 4); diag(sigma) <- 1
u <- MASS::mvrnorm(n = N, mu = rep(0, 4), Sigma = sigma)
Y1 <- 1 + 1 * X1 + X2 
Y2 <- 1 + 0.01 * X1 + X2 
Y3 <- 1 + 0.4 * X1 + X2
Y4 <- 1 + -0.02 * X1 + X2 
for(x in 1:4){
  var_char <- paste0("Y", x)
  assign(var_char, get(var_char) + u[,x])
}

data <- data.frame(Y1 = Y1,
                   Y2 = Y2,
                   Y3 = Y3,
                   Y4 = Y4,
                   X1 = X1,
                   X2 = X2,
                   #group_id = group_id,
                   splitvar = sample(1:2, N, TRUE))

fit <- feols(c(Y1, Y2, Y3, Y4) ~ csw(X1,X2),
             data = data,
             se = "hetero",
             ssc = ssc(cluster.adj = TRUE))

# clean workspace except for res & data
rm(list= ls()[!(ls() %in% c('fit','data'))])

res_rwolf1 <- wildrwolf::rwolf(
  models = fit,
  param = "X1", 
  B = 9999, 
  seed = 23
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=========                                                             |  12%  |                                                                              |==================                                                    |  25%  |                                                                              |==========================                                            |  38%  |                                                                              |===================================                                   |  50%  |                                                                              |============================================                          |  62%  |                                                                              |====================================================                  |  75%  |                                                                              |=============================================================         |  88%  |                                                                              |======================================================================| 100%

pvals <- lapply(fit, function(x) pvalue(x)["X1"]) |> unlist()

# Romano-Wolf Corrected P-values
summary(res_rwolf1)
#>   model     Estimate Std. Error    t value      Pr(>|t|) RW Pr(>|t|)
#> 1     1    0.9896609 0.04204902   23.53588  8.811393e-98      0.0001
#> 2     2    0.9713667 0.03201663   30.33945 9.318861e-144      0.0001
#> 3     3 -0.007682607 0.04222391 -0.1819492     0.8556595      0.9798
#> 4     4  -0.02689601 0.03050616 -0.8816584     0.3781741      0.8500
#> 5     5     0.411529 0.04299497   9.571561    7.9842e-21      0.0001
#> 6     6    0.3925661 0.03096423   12.67805  2.946569e-34      0.0001
#> 7     7    0.0206361 0.04405654  0.4684003     0.6396006      0.9542
#> 8     8  0.001657765 0.03337464 0.04967138     0.9603942      0.9798

# Holm Corrected P-values
p.adjust(pvals, method = "holm") |> round(4)
#>      lhs: Y1; rhs: X1.X1 lhs: Y1; rhs: X1 + X2.X1      lhs: Y2; rhs: X1.X1 
#>                        0                        0                        1 
#> lhs: Y2; rhs: X1 + X2.X1      lhs: Y3; rhs: X1.X1 lhs: Y3; rhs: X1 + X2.X1 
#>                        1                        0                        0 
#>      lhs: Y4; rhs: X1.X1 lhs: Y4; rhs: X1 + X2.X1 
#>                        1                        1
```

## Example II

``` r
fit1 <- feols(Y1 ~ X1 , data = data)
fit2 <- feols(Y1 ~ X1 + X2, data = data)
fit3 <- feols(Y2 ~ X1, data = data)
fit4 <- feols(Y2 ~ X1 + X2, data = data)

res_rwolf2 <- rwolf(
  models = list(fit1, fit2, fit3, fit4), 
  param = "X1",  
  B = 9999
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
summary(res_rwolf2)
#>   model     Estimate Std. Error    t value      Pr(>|t|) RW Pr(>|t|)
#> 1     1    0.9896609 0.04341633   22.79467  6.356963e-93      0.0001
#> 2     2    0.9713667 0.03186495   30.48386 9.523796e-145      0.0001
#> 3     3 -0.007682607 0.04403736 -0.1744566      0.861542      0.8567
#> 4     4  -0.02689601 0.03130345 -0.8592027     0.3904352      0.6163
```

## Performance

The above procedure with `S=8` hypotheses, `N=1000` observations and
`k %in% (1,2)` parameters finishes in around 5 seconds.

``` r
if(requireNamespace("microbenchmark")){
  
  microbenchmark::microbenchmark(
    "Romano-Wolf" = wildrwolf::rwolf(
      models = fit,
      param = "X1", 
      B = 9999, 
      seed = 23
    ), 
    times = 1
  )
 
# t: seconds
#         expr      min       lq     mean   median       uq
#  Romano-Wolf 4.834184 4.834184 4.834184 4.834184 4.834184
#       max neval
#  4.834184     1   
}
```

## But does it work? Monte Carlo Experiments

We test $S=6$ hypotheses and generate data as

$$Y_{i,s,g} = \beta_{0} + \beta_{1,s} D_{i} + u_{i,g} + \epsilon_{i,s} $$
where $D_i = 1(U_i > 0.5)$ and $U_i$ is drawn from a uniform
distribution, $u_{i,g}$ is a cluster level shock with intra-cluster
correlation $0.5$, and the idiosyncratic error term is drawn from a
multivariate random normal distribution with mean $0_S$ and covariance
matrix

``` r
S <- 6
rho <- 0.5
Sigma <- matrix(rho, 6, 6)
diag(Sigma) <- 1
Sigma
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]  1.0  0.5  0.5  0.5  0.5  0.5
#> [2,]  0.5  1.0  0.5  0.5  0.5  0.5
#> [3,]  0.5  0.5  1.0  0.5  0.5  0.5
#> [4,]  0.5  0.5  0.5  1.0  0.5  0.5
#> [5,]  0.5  0.5  0.5  0.5  1.0  0.5
#> [6,]  0.5  0.5  0.5  0.5  0.5  1.0
```

with $\rho \geq 0$. We assume that $\beta_{1,s}= 0$ for all $s$.

This experiment imposes a data generating process as in equation (9) in
[Clarke, Romano and Wolf](https://docs.iza.org/dp12845.pdf), with an
additional error term $u_g$ for $G=20$ clusters and intra-cluster
correlation 0.5 and $N=1000$ observations.

You can run the simulations via the `run_fwer_sim()` function attached
in the package.

``` r
# note that this will take some time
res <- run_fwer_sim(
  seed = 76,
  n_sims = 1000,
  B = 499,
  N = 1000,
  s = 6, 
  rho = 0.5 #correlation between hypotheses, not intra-cluster!
)
```

Both Holm’s method and `wildrwolf` control the family wise error rates,
at both the 5 and 10% significance level.

``` r
res
#>                 reject_5 reject_10 rho
#> fit_pvalue         0.282     0.502 0.5
#> fit_pvalue_holm    0.061     0.104 0.5
#> fit_padjust_rw     0.059     0.105 0.5
```

## Comparison with Stata’s rwolf package

``` r
library(RStata)
# initiate RStata
    options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataBE-64\"")
    options("RStata.StataVersion" = 17)
# save the data set so it can be loaded into STATA
write.csv(data, "c:/Users/alexa/Dropbox/rwolf/inst/extdata/readme.csv")

# estimate with stata via Rstata
stata_program <- "
clear
set more off
import delimited c:/Users/alexa/Dropbox/rwolf/inst/data/readme.csv
set seed 1
rwolf y1 y2 y3 y4, indepvar(x1) controls(x2) reps(9999)
"
RStata::stata(stata_program, data.out = TRUE)


# Romano-Wolf step-down adjusted p-values
# 
# 
# Independent variable:  x1
# Outcome variables:   y1 y2 y3 y4
# Number of resamples: 9999
# 
# 
# ------------------------------------------------------------------------------
#    Outcome Variable | Model p-value    Resample p-value    Romano-Wolf p-value
# --------------------+---------------------------------------------------------
#                  y1 |    0.0000             0.0001              0.0001
#                  y2 |    0.3904             0.3755              0.6070
#                  y3 |    0.0000             0.0001              0.0001
#                  y4 |    0.9586             0.9596              0.9596
# ------------------------------------------------------------------------------
```

For comparison, `wildrwolf` produces the following output:

``` r
models <- feols(c(Y1, Y2, Y3, Y4) ~ X1 + X2 
                 , data = data, se = "hetero")
```

``` r
summary(rwolf(models, param = "X1", B = 9999, seed = 123))
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
#>   model    Estimate Std. Error    t value      Pr(>|t|) RW Pr(>|t|)
#> 1     1   0.9713667 0.03201663   30.33945 9.318861e-144      0.0001
#> 2     2 -0.02689601 0.03050616 -0.8816584     0.3781741      0.6082
#> 3     3   0.3925661 0.03096423   12.67805  2.946569e-34      0.0001
#> 4     4 0.001657765 0.03337464 0.04967138     0.9603942      0.9600
```
