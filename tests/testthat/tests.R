test_that("test wildrwolf against Stata's rwolf", {
  
  
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

  # stata results
  stata_res <- c(0.0010, 0.4166, 0.4166, 0.1179)
  
  expect_equal(res_rwolf$pval, stata_res, tolerance = 0.01)
    
  
  
})
