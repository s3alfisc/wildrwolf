
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rwolf

The `rwolf` package implements Romano-Wolf multiple-hypothesis-adjusted
p-values for objects of type `fixest_multi` from the `fixest` package
(currently for one-way clustered inference) via the pairs or wild
cluster bootstrap. At its current stage, the package is highly
experimental and it is not sufficiently tested.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("s3alfisc/rwolf")
```

## Example

As you can see in the example, there seems to be a bug in `rwolf()` for
the pairs bootstrap.

``` r
library(fixest)
library(rwolf)

set.seed(5)
N <- 1000
X1 <- rnorm(N)
X2 <- rnorm(N)
Y1 <- 1 + 1 * X1 + X2 + rnorm(N)
Y2 <- 1 + 0.01 * X1 + X2 + rnorm(N)
Y3 <- 1 + 0.01 * X1 + X2 + rnorm(N)
Y4 <- 1 + 0.01 * X1 + X2 + rnorm(N)

# intra-cluster correlation of 0 for all clusters
cluster <- rep(1:50, N / 50)

data <- data.frame(Y1 = Y1, 
                   Y2 = Y2, 
                   Y3 = Y3, 
                   Y4 = Y4,
                   X1 = X1,
                   X2 = X2,
                   cluster = cluster)

res <- feols(c(Y1, Y2, Y3, Y4) ~ X1 + X2, data = data, cluster = ~ cluster)

# adjust inference for null X1 = 0 vs X1 \neq 0
res_rwolf <- rwolf(models = res, parameter = "X1", B = 1000)
#> The number of clusters is relatively small with 50 clusters. Note that for such a small number of clusters, the wild cluster bootstrap might be significantly faster than the pairs bootstrap.
res_rwolf_wild <- rwolf(models = res, parameter = "X1", B = 1000, type = "wild")

summary(res_rwolf)
#> feols(fml = c(Y1, Y2, Y3, Y4) ~ X1 + X2, data = data, cluster = ~cluster)
#>   depvar   Estimate Std. Error    t value     Pr(>|t|) RW Pr(>|t|)
#> 1     Y1 1.00205346 0.02953734 33.9249693 1.081526e-35 0.000999001
#> 2     Y2 0.03562124 0.03238878  1.0998018 2.767926e-01 0.365634366
#> 3     Y3 0.01296455 0.04142947  0.3129305 7.556615e-01 0.658341658
#> 4     Y4 0.02479527 0.02879551  0.8610809 3.933876e-01 0.418581419
summary(res_rwolf_wild)
#> feols(fml = c(Y1, Y2, Y3, Y4) ~ X1 + X2, data = data, cluster = ~cluster)
#>   depvar   Estimate Std. Error    t value     Pr(>|t|) RW Pr(>|t|)
#> 1     Y1 1.00205346 0.02953734 33.9249693 1.081526e-35 0.000999001
#> 2     Y2 0.03562124 0.03238878  1.0998018 2.767926e-01 0.638361638
#> 3     Y3 0.01296455 0.04142947  0.3129305 7.556615e-01 0.775224775
#> 4     Y4 0.02479527 0.02879551  0.8610809 3.933876e-01 0.639360639
```

How does it compare to results from the `rwolf` Stata package?

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
rwolf y1 y2 y3 y4, vce(cluster cluster) cluster(cluster)  indepvar(x1) controls(x2) reps(1000) nodots
"
RStata::stata(stata_program, data.out = TRUE)
#> . 
#> . clear 
#> . set more off
#> . import delimited c:/Users/alexa/Dropbox/rwolf/test.csv
#> (7 vars, 1,000 obs)
#> . set seed 1
#> . rwolf y1 y2 y3 y4, vce(cluster cluster) cluster(cluster)  indepvar(x1) contro
#> > ls(x2) reps(1000) nodots
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
#>                  y2 |    0.2768             0.2797              0.6144
#>                  y3 |    0.7557             0.7632              0.7632
#>                  y4 |    0.3934             0.4206              0.6344
#> ------------------------------------------------------------------------------
```
