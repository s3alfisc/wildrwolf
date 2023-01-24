test_that("test wildrwolf against Stata's rwolf", {


  # ## create data + run rwolf in R
  # library(MASS)
  # library(fixest)
  # library(wildrwolf)
  # 
  # set.seed(986)
  # N <- 500
  # s <- 10
  # rho <- 0.2
  # D <- sample(c(0,1), N, TRUE)
  # Sigma <- matrix(rho, s, s); diag(Sigma) <- 1
  # e <- MASS::mvrnorm(n = N, mu = rep(0, s), Sigma)
  # intercept <- rnorm(s)
  # effect <- rep(0, s)
  # # true effect of beta_1 = 0 in each simulations
  # Y <- intercept + e
  # 
  # df <- data.frame(Y = Y)
  # names(df) <- paste0("Y", 1:s)
  # df$treatment <- D
  # df$cluster <- sample(letters, N, TRUE)
  # df$X1 <- rnorm(N)
  # df$X2 <- rnorm(N)
  # 
  # write.csv(df, file = "C:/Users/alexa/Dropbox/wildrwolf/tests/data.csv")
  # 
  # fit <- feols(c(Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10) ~ treatment, data = df)
  # 
  # etable(fit)
  # fit_padjust <- rwolf(fit, param = ~treatment, B = 9999)
  # 
  # 
  # ## run everything via rwolf.ado
  # 
  # run_stata <- FALSE
  # if(run_stata){
  #   library(RStata)
  #   options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataBE-64\"")
  #   options("RStata.StataVersion" = 17)
  # 
  #   res <- stata("C:/Users/alexa/Dropbox/wildrwolf/tests/Stata/stata_tests.do")
  #   # Romano-Wolf step-down adjusted p-values
  #   #
  #   #
  #   # Independent variable:  treatment
  #   # Outcome variables:   y1 y2 y3 y4 y5 y6 y7 y8 y9 y10
  #   # Number of resamples: 9999
  #   #
  #   #
  #   # -------------------------------------------------------------------------
  #   #   > -----
  #   #   Outcome Variable | Model p-value    Resample p-value    Romano-Wolf p-
  #   #   > value
  #   # --------------------+----------------------------------------------------
  #   #   > -----
  #   #   y1 |    0.4744             0.4680              0.9739
  #   # y2 |    0.8702             0.8732              0.9974
  #   # y3 |    0.5812             0.5835              0.9921
  #   # y4 |    0.7858             0.7875              0.9974
  #   # y5 |    0.7648             0.7637              0.9974
  #   # y6 |    0.6877             0.6842              0.9957
  #   # y7 |    0.9359             0.9401              0.9974
  #   # y8 |    0.6376             0.6384              0.9947
  #   # y9 |    0.8481             0.8517              0.9974
  #   # y10 |    0.2328             0.2372              0.9974
  #   # -------------------------------------------------------------------------
  # 
  # }
  # # stata results
  # stata_res <- c(0.9739, 0.9974, 0.9921, 0.9974, 0.9974, 0.9957, 0.9974,
  #                0.9947, 0.9974, 0.9974)
  # 
  # expect_equal(
  #   stata_res,
  #   fit_padjust$pval,
  #   tolerance = 1e-02
  # )
  # 
  # ## test for equivalence
  # 
  # 
  # expect_equal(res_rwolf$pval, stata_res, tolerance = 0.1)



})
