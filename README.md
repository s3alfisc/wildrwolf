
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rwolf

The `rwolf` package implements Romano-Wolf multiple-hypothesis-adjusted
p-values for objects of type `fixest_multi` from the `fixest` package
(currently for one-way clustered inference) via a wild cluster
bootstrap. At its current stage, the package is highly experimental and
it is not thoroughly tested.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("s3alfisc/rwolf")
```

## Example

<!-- As you can see in the example, there seems to be a bug in `rwolf()` for the pairs bootstrap. -->

``` r
library(fixest)
library(rwolf)

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
                   group_id = group_id)

res <- feols(c(Y1, Y2, Y3, Y4) ~ X1 + X2, 
             data = data,
             cluster = ~ group_id, 
             dof = dof(cluster.adj = TRUE))

# clean workspace except for res & data
rm(list= ls()[!(ls() %in% c('res','data'))])

res_rwolf <- rwolf(models = res, param = "X1", B = 1000)
summary(res_rwolf)
#> feols(fml = c(Y1, Y2, Y3, Y4) ~ X1 + X2, data = data, cluster = ~group_id, 
#>     dof = dof(cluster.adj = TRUE))
#>   depvar    Estimate Std. Error    t value      Pr(>|t|) RW Pr(>|t|)
#> 1     Y1 0.995788153 0.01038199 95.9149274 1.487056e-168 0.000999001
#> 2     Y2 0.008968811 0.01012741  0.8855978  3.769031e-01 0.408591409
#> 3     Y3 0.011942201 0.01001154  1.1928441  2.343508e-01 0.408591409
#> 4     Y4 0.021048717 0.01017059  2.0695674  3.978448e-02 0.119880120
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
