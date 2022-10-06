test_that("test wildrwolf against Stata's rwolf", {
  

  ## create data + run rwolf in R
  library(MASS)
  library(fixest)
  library(wildrwolf)
  
  set.seed(986)
  N <- 5000
  s <- 10
  rho <- 0.2
  D <- sample(c(0,1), N, TRUE)
  Sigma <- matrix(rho, s, s); diag(Sigma) <- 1
  e <- MASS::mvrnorm(n = N, mu = rep(0, s), Sigma)
  intercept <- rnorm(s)
  effect <- rep(0, s)
  # true effect of beta_1 = 0 in each simulations
  Y <- intercept + e 
  
  df <- data.frame(Y = Y)
  names(df) <- paste0("Y", 1:s)
  df$treatment <- D
  df$cluster <- sample(letters, N, TRUE)
  df$X1 <- rnorm(N)
  df$X2 <- rnorm(N)
  
  write.csv(df, file = "C:/Users/alexa/Dropbox/wildrwolf/tests/data.csv")
  
  fit <- feols(c(Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10) ~ treatment, data = df)
  
  etable(fit)
  fit_padjust <- rwolf(fit, param = ~treatment, B = 999)
  
  
  ## run everything via rwolf.ado
  
  run_stata <- FALSE
  if(run_stata){
    library(RStata)
    options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataBE-64\"") 
    options("RStata.StataVersion" = 17)
    
    res <- stata("C:/Users/alexa/Dropbox/wildrwolf/tests/Stata/stata_tests.do")
    saveRDS(res, "C:/Users/alexa/Dropbox/wildrwolf/tests/data/stata_resultsI.rds")
  }
  
  # stata results
  stata_res <- c(0.0010, 0.4166, 0.4166, 0.1179)
  
  
  ## test for equivalence
  
  
  expect_equal(res_rwolf$pval, stata_res, tolerance = 0.1)
    
  
  
})
