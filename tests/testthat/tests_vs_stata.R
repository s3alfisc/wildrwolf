test_that("test wildrwolf against Stata's rwolf", {

  # set to TRUE if you want to run the actual stata 
  # commands via RStata
  run_stata <- FALSE
  
  
  ## create data + run rwolf in R
  library(MASS)
  library(fixest)
  library(wildrwolf)

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
  
  fit <- feols(c(Y1, Y2, Y3, Y4) ~ X1,
               data = data,
               se = "hetero",
               ssc = ssc(cluster.adj = TRUE))

  fit_padjust <- rwolf(fit, param = ~X1, B = 9999)


  ## run everything via rwolf.ado

  if(run_stata){
    # write file to disk
    write.csv(
      data,
      file = "C:/Users/alexa/Dropbox/wildrwolf/inst/extdata/data.csv"
    )
    
    library(RStata)
    options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataBE-64\"")
    options("RStata.StataVersion" = 17)

    res <- stata("C:/Users/alexa/Dropbox/wildrwolf/tests/Stata/stata_tests.do")
  } 
  
  stata_res <- c( 0.0001,  0.1697,  0.0001, 0.3950)

  expect_equal(
    stata_res,
    fit_padjust$pval,
    tolerance = 1e-01
  )



})
