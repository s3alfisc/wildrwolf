
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
`fixest_multi` from the `fixest` package via a wild cluster bootstrap.
At its current stage, the package is experimental and it is not
thoroughly tested.

Because the bootstrap-resampling is based on the
[fwildclusterboot](https://github.com/s3alfisc/fwildclusterboot)
package, `wildwyoung` is usually really fast.

The package is complementary to
[wildwyoung](https://github.com/s3alfisc/wildwyoung), which implements
the multiple hypothesis adjustment method following Westfall and Young
(1993).

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
library(wildrwolf)
library(wildwyoung)
library(fixest)

set.seed(1412)

N <- 5000
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

res_rwolf <- wildrwolf::rwolf(
  models = fit,
  param = "X1", 
  B = 9999, 
  seed = 23
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=========                                                             |  12%  |                                                                              |==================                                                    |  25%  |                                                                              |==========================                                            |  38%  |                                                                              |===================================                                   |  50%  |                                                                              |============================================                          |  62%  |                                                                              |====================================================                  |  75%  |                                                                              |=============================================================         |  88%  |                                                                              |======================================================================| 100%

res_wyoung <- wildwyoung::wyoung(
  models = fit,
  param = "X1", 
  B = 9999,
  seed = 23
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=========                                                             |  12%  |                                                                              |==================                                                    |  25%  |                                                                              |==========================                                            |  38%  |                                                                              |===================================                                   |  50%  |                                                                              |============================================                          |  62%  |                                                                              |====================================================                  |  75%  |                                                                              |=============================================================         |  88%  |                                                                              |======================================================================| 100%

pvals <- lapply(fit, function(x) pvalue(x)["X1"]) |> unlist()

# Romano-Wolf Corrected P-values
summary(res_rwolf)
#>   model    Estimate Std. Error   t value      Pr(>|t|) RW Pr(>|t|)
#> 1     1    1.015905 0.02023716  50.19996             0      0.0001
#> 2     2    1.014589 0.01416664  71.61819             0      0.0001
#> 3     3  0.03139199 0.01970187  1.593351     0.1111447      0.2997
#> 4     4  0.03008817  0.0140508  2.141384     0.0322913      0.1240
#> 5     5   0.3876217 0.01996071  19.41924  4.817655e-81      0.0001
#> 6     6   0.3863082 0.01397504  27.64272 1.228595e-156      0.0001
#> 7     7 -0.01675694 0.01951484 -0.858677     0.3905599      0.3908
#> 8     8 -0.01807432 0.01388615 -1.301608     0.1931104      0.3513

# Westfall-Young Corrected P-values
summary(res_wyoung)
#>   model    Estimate Std. Error   t value      Pr(>|t|) WY Pr(>|t|)
#> 1     1    1.015905 0.02023716  50.19996             0   0.0000000
#> 2     2    1.014589 0.01416664  71.61819             0   0.0000000
#> 3     3  0.03139199 0.01970187  1.593351     0.1111447   0.3009301
#> 4     4  0.03008817  0.0140508  2.141384     0.0322913   0.1183118
#> 5     5   0.3876217 0.01996071  19.41924  4.817655e-81   0.0000000
#> 6     6   0.3863082 0.01397504  27.64272 1.228595e-156   0.0000000
#> 7     7 -0.01675694 0.01951484 -0.858677     0.3905599   0.3885389
#> 8     8 -0.01807432 0.01388615 -1.301608     0.1931104   0.3539354

# Holm Corrected P-values
p.adjust(pvals, method = "holm") |> round(4)
#>     X1     X1     X1     X1     X1     X1     X1     X1 
#> 0.0000 0.0000 0.3334 0.1292 0.0000 0.0000 0.3906 0.3862
```

## Example II

``` r
fit1 <- feols(Y1 ~ X1 , data = data)
fit2 <- feols(Y1 ~ X1 + X2, data = data)
fit3 <- feols(Y2 ~ X1, data = data)
fit4 <- feols(Y2 ~ X1 + X2, data = data)

res_rwolf <- rwolf(
  models = list(fit1, fit2, fit3, fit4), 
  param = "X1",  
  B = 9999
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
summary(res_rwolf)
#>   model   Estimate Std. Error  t value   Pr(>|t|) RW Pr(>|t|)
#> 1     1   1.015905 0.01996994 50.87171          0      0.0001
#> 2     2   1.014589 0.01397591 72.59557          0      0.0001
#> 3     3 0.03139199 0.02002123 1.567936  0.1169595      0.1123
#> 4     4 0.03008817 0.01418312 2.121407 0.03393663      0.0610
```

## Performance

The above procedures with `S=8` hypotheses, `N=5000` observations and
`k %in% (1,2)` parameters finish each in around 3.5 seconds.

``` r
if(requireNamespace("microbenchmark")){
  
  microbenchmark::microbenchmark(
    "Westfall-Young" = wildwyoung::wyoung(
      models = fit,
      param = "X1", 
      B = 9999,
      seed = 23
    ),
    "Romano-Wolf" = wildrwolf::rwolf(
      models = fit,
      param = "X1", 
      B = 9999, 
      seed = 23
    ), 
    times = 1
  )
 
 # t: seconds
 #           expr      min       lq     mean   median       uq      max neval
 # Westfall-Young 3.625710 3.625710 3.625710 3.625710 3.625710 3.625710     1
 #    Romano-Wolf 3.382969 3.382969 3.382969 3.382969 3.382969 3.382969     1
   
}
```

<!-- ## Comparison with Stata's rwolf package  -->
<!-- ```{r, eval = FALSE} -->
<!-- library(RStata) -->
<!-- # initiate RStata -->
<!-- options("RStata.StataVersion" = 16) -->
<!-- options("RStata.StataPath" = "\"C:\\Program Files\\Stata16\\StataIC-64\"") -->
<!-- # save the data set so it can be loaded into STATA -->
<!-- data.table::fwrite(data, "c:/Users/alexa/Dropbox/rwolf/test.csv") -->
<!-- # estimate with stata via Rstata -->
<!-- stata_program <- " -->
<!-- clear  -->
<!-- set more off -->
<!-- import delimited c:/Users/alexa/Dropbox/rwolf/test.csv -->
<!-- set seed 1 -->
<!-- rwolf y1 y2 y3 y4, vce(cluster group_id) cluster(group_id)  indepvar(x1) controls(x2) reps(1000) nodots -->
<!-- " -->
<!-- RStata::stata(stata_program, data.out = TRUE) -->
<!-- #> .  -->
<!-- #> . clear  -->
<!-- #> . set more off -->
<!-- #> . import delimited c:/Users/alexa/Dropbox/rwolf/test.csv -->
<!-- #> (7 vars, 10,000 obs) -->
<!-- #> . set seed 1 -->
<!-- #> . rwolf y1 y2 y3 y4, vce(cluster group_id) cluster(group_id)  indepvar(x1) cont -->
<!-- #> > rols(x2) reps(1000) nodots -->
<!-- #> Bootstrap replications (1000). This may take some time. -->
<!-- #>  -->
<!-- #>  -->
<!-- #>  -->
<!-- #>  -->
<!-- #> Romano-Wolf step-down adjusted p-values -->
<!-- #>  -->
<!-- #>  -->
<!-- #> Independent variable:  x1 -->
<!-- #> Outcome variables:   y1 y2 y3 y4 -->
<!-- #> Number of resamples: 1000 -->
<!-- #>  -->
<!-- #>  -->
<!-- #> ------------------------------------------------------------------------------ -->
<!-- #>    Outcome Variable | Model p-value    Resample p-value    Romano-Wolf p-value -->
<!-- #> --------------------+--------------------------------------------------------- -->
<!-- #>                  y1 |    0.0000             0.0010              0.0010 -->
<!-- #>                  y2 |    0.3769             0.3756              0.4166 -->
<!-- #>                  y3 |    0.2344             0.2408              0.4166 -->
<!-- #>                  y4 |    0.0398             0.0410              0.1179 -->
<!-- #> ------------------------------------------------------------------------------ -->
<!-- ``` -->
