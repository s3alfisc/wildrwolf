
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wildrwolf 🐺

<!-- badges: start -->

[![R-CMD-check](https://github.com/s3alfisc/rwolf/workflows/R-CMD-check/badge.svg)](https://github.com/s3alfisc/rwolf/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
![runiverse-package](https://s3alfisc.r-universe.dev/badges/wildrwolf)

[![Codecov test
coverage](https://codecov.io/gh/s3alfisc/rwolf/branch/main/graph/badge.svg)](https://app.codecov.io/gh/s3alfisc/rwolf?branch=main)
<!-- badges: end -->

The `wildrwolf` package implements Romano-Wolf
multiple-hypothesis-adjusted p-values for objects of type `fixest` and
fixest_multi`from the`fixest\` package via a wild cluster bootstrap. At
its current stage, the package is experimental and it is not thoroughly
tested.

Adding support for multi-way clustering is work in progress.

I hope to submit `wildrwolf` to CRAN by the end of the summer - if you
would like to help me get there, please send me an email 😄

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
library(fixest)
library(wildrwolf)

set.seed(8)
N <- 10000
X1 <- rnorm(N)
X2 <- rnorm(N)
Y1 <- 1 + 1 * X1 + X2 + rnorm(N)
Y2 <- 1 + 0.01 * X1 + X2 + rnorm(N)
Y3 <- 1 + 0.01 * X1 + X2 + rnorm(N)
Y4 <- 1 + 0.01 * X1 + X2 + rnorm(N)

# intra-cluster correlation of 0 for all clusters
numb_clusters <- N / 50
group_id <- as.character(sample(1:numb_clusters, N, replace = TRUE))

data <- data.frame(Y1 = Y1, 
                   Y2 = Y2, 
                   Y3 = Y3, 
                   Y4 = Y4,
                   X1 = X1,
                   X2 = X2,
                   group_id = group_id, 
                   splitvar = sample(1:2, N, TRUE))

res <- feols(c(Y1, Y2, Y3, Y4) ~ X1 + X2, 
             data = data,
             cluster = ~ group_id, 
             ssc = ssc(cluster.adj = TRUE))

# clean workspace except for res & data
rm(list= ls()[!(ls() %in% c('res','data'))])

res_rwolf <- rwolf(models = res, param = "X1", B = 9999, nthreads = 2)
summary(res_rwolf)
#>     model depvar    Estimate Std. Error   t value      Pr(>|t|) RW Pr(>|t|)
#> 1 Model 1     Y1   0.9957882 0.01038199  95.91493 1.487056e-168      0.0001
#> 2 Model 2     Y2 0.008968811 0.01012741 0.8855978     0.3769031      0.4142
#> 3 Model 3     Y3   0.0119422 0.01001154  1.192844     0.2343508      0.4142
#> 4 Model 4     Y4  0.02104872 0.01017059  2.069567    0.03978448      0.1126
```

## Example II

``` r
fit1 <- feols(Y1 ~ X1 + X2, data = data, cluster = ~ group_id)
fit2 <- feols(Y2 ~ X1 + X2, data = data, cluster = ~ group_id)
fit3 <- feols(Y3 ~ X1 + X2, data = data, cluster = ~ group_id)
fit4 <- feols(Y4 ~ X1 + X2, data = data, cluster = ~ group_id)

res_rwolf <- rwolf(
  models = list(fit1, fit2, fit3, fit4), 
  param = "X1",  
  B = 9999,
  nthreads = 2
)
summary(res_rwolf)
#>     model depvar    Estimate Std. Error   t value      Pr(>|t|) RW Pr(>|t|)
#> 1 Model 1     Y1   0.9957882 0.01038199  95.91493 1.487056e-168      0.0001
#> 2 Model 2     Y2 0.008968811 0.01012741 0.8855978     0.3769031      0.4119
#> 3 Model 3     Y3   0.0119422 0.01001154  1.192844     0.2343508      0.4119
#> 4 Model 4     Y4  0.02104872 0.01017059  2.069567    0.03978448      0.1147
```

## Example III

``` r
base = iris
names(base) = c("y1", "y2", "x1", "x2", "species")

# no clustering, heteroskedastic wild bootstrap
res_multi = feols(c(y1, y2) ~ x1 + csw(x2, x2^2) | sw(species),
                  data = base)

res_rwolf2 <- rwolf(models = res_multi, param = "x1", B = 9999, nthreads = 2)

summary(res_rwolf2)
#>     model depvar  Estimate Std. Error  t value    Pr(>|t|) RW Pr(>|t|)
#> 1 Model 1     y1 0.9059459 0.08138023 11.13226 0.007972879       1e-04
#> 2 Model 2     y1 0.8996897 0.08366698 10.75322 0.008537546       1e-04
```

## Performance

Using the wild cluster bootstrap implementations in `fwildclusterboot`
is fast:

``` r
microbenchmark::microbenchmark(res_rwolf1 = rwolf(models = res, param = "X1", B = 99999, boot_algo = "R"),
                               res_rwolf2 = rwolf(models = res, param = "X1", B = 99999, boot_algo = "WildBootTests.jl"),
                               times = 1)
# Unit: seconds
# expr         min       lq       mean    median    uq       max    neval
# res_rwolf1 3.562108 3.562108 3.562108 3.562108 3.562108 3.562108     1
# res_rwolf2 1.778090 1.778090 1.778090 1.778090 1.778090 1.778090     1
```

## Comparison with Stata’s rwolf package

``` r
library(RStata)
# initiate RStata
options("RStata.StataVersion" = 16)
options("RStata.StataPath" = "\"C:\\Program Files\\Stata16\\StataIC-64\"")
# save the data set so it can be loaded into STATA
data.table::fwrite(data, "c:/Users/alexa/Dropbox/rwolf/test.csv")

# estimate with stata via Rstata
stata_program <- "
clear 
set more off
import delimited c:/Users/alexa/Dropbox/rwolf/test.csv
set seed 1
rwolf y1 y2 y3 y4, vce(cluster group_id) cluster(group_id)  indepvar(x1) controls(x2) reps(1000) nodots
"
RStata::stata(stata_program, data.out = TRUE)

#> . 
#> . clear 
#> . set more off
#> . import delimited c:/Users/alexa/Dropbox/rwolf/test.csv
#> (7 vars, 10,000 obs)
#> . set seed 1
#> . rwolf y1 y2 y3 y4, vce(cluster group_id) cluster(group_id)  indepvar(x1) cont
#> > rols(x2) reps(1000) nodots
#> Bootstrap replications (1000). This may take some time.
#> 
#> 
#> 
#> 
#> Romano-Wolf step-down adjusted p-values
#> 
#> 
#> Independent variable:  x1
#> Outcome variables:   y1 y2 y3 y4
#> Number of resamples: 1000
#> 
#> 
#> ------------------------------------------------------------------------------
#>    Outcome Variable | Model p-value    Resample p-value    Romano-Wolf p-value
#> --------------------+---------------------------------------------------------
#>                  y1 |    0.0000             0.0010              0.0010
#>                  y2 |    0.3769             0.3756              0.4166
#>                  y3 |    0.2344             0.2408              0.4166
#>                  y4 |    0.0398             0.0410              0.1179
#> ------------------------------------------------------------------------------
```
