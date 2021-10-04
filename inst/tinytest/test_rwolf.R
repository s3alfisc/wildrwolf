run_tests <- FALSE

if(run_tests){
  
  library(RStata)
  library(rwolf)
  options("RStata.StataVersion" = 16)
  #chooseStataBin()
  options("RStata.StataPath" = "\"C:\\Program Files\\Stata16\\StataIC-64\"")
  
  library(fixest)
  library(rwolf)
  
  set.seed(12345)
  
  N <- 10000
  X1 <- rnorm(N)
  Y1 <- 1 + 1 * X1 + rnorm(N)
  Y2 <- 1 + 0.01 * X1 + rnorm(N)
  Y3 <- 1 + 0.01 * X1 + rnorm(N)
  Y4 <- 1 + 0.01 * X1 + rnorm(N)
  
  B <- 10000
  # intra-cluster correlation of 0 for all clusters
  cluster <- rep(1:50, 1000 / 50)
  
  data <- data.frame(Y1 = Y1, 
                     Y2 = Y2, 
                     Y3 = Y3, 
                     Y4 = Y4,
                     X1 = X1, 
                     cluster = cluster)
  
  data.table::fwrite(data, "test.csv")
  
  res <- feols(c(Y1, Y2, Y3, Y4) ~ X1, data = data, cluster = ~ cluster)
  class(res)
  summary(res)
  
  # estimate with R 
  res_rwolf <- rwolf(models = res, parameter = "X1", B = B, type = NULL)
  res_rwolf_wild <- rwolf(models = res, parameter = "X1", B = B, type = "wild")
  
  # estimate with stata
  stata_program <- "
  clear 
  set more off
  import delimited c:/Users/alexa/Dropbox/rwolf/test.csv
  set seed 1
  rwolf y1 y2 y3 y4, vce(cluster cluster) cluster(cluster)  indepvar(x1) reps(1000)
  "
  stata_res <- RStata::stata(stata_program, data.out = TRUE)
  
  #stata_res
  summary(res_rwolf)
  summary(res_rwolf_wild)

  
  
}
