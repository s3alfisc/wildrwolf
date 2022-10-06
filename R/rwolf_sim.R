rwolf_heteroskedastic <- function(rho, N, s, B){
  
  
  #rho <- 0
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
  
  #write.csv(df, file = "C:/Users/alexa/Dropbox/wildrwolf/tests/data.csv")
  
  fit <- fixest::feols(c(Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10) ~ treatment, data = df)
  fit_pvalue <- lapply(fit, function(x) fixest::pvalue(x))
  fit_pvalue <- Reduce("rbind", fit_pvalue)[,"treatment"]
  fit_padjust <- rwolf(fit, param = ~treatment, B = B, nthreads = 1)$pval
  fit_pvalue_holm <- p.adjust(fit_pvalue, method = "holm")
  
  data.frame(
    fit_pvalue = fit_pvalue, 
    fit_pvalue_holm = fit_pvalue_holm,
    fit_padjust = fit_padjust
  )
  
}

run_rwolf_heteroskedastic <- function(
    n_sims = 1000, 
    rho = c(0, 0.25, 0.5, 0.75),
    seed = 1234123, 
    B = 3999, 
    N = 1000, 
    s = 10){
  
  set.seed(seed)
  
  all_sims <- list()
  
  for(x in seq_along(rho)){
    
    all_rho <- list()  
    pb = txtProgressBar(min = 0, max = n_sims, initial = 0, style = 3) 
    
    
    for(i in 1:n_sims){
      setTxtProgressBar(pb,i)
      all_rho[[i]] <- rwolf_heteroskedastic(rho = rho[x], B = B, N = N, s = s)
    }
    
    reject_5 <- list()
    for(i in 1:n_sims){
      rho_i <- all_rho[[i]]
      # calculate FWER
      reject_5[[i]] <- sapply(rho_i, function(x){any(x < 0.05)})
    }
    
    reject_10 <- list()
    for(i in 1:n_sims){
      rho_i <- all_rho[[i]]
      # calculate FWER
      reject_10[[i]] <- sapply(rho_i, function(x){any(x < 0.10)})
    }
    
    reject_5 <- Reduce("+", reject_5) / n_sims
    reject_10 <- Reduce("+", reject_10) / n_sims
    
    all_sims[[x]] <- data.frame(reject_5 = reject_5, reject_10 = reject_10)
    
  }
  
  all_sims
  
}

# res <- run_rwolf_heteroskedastic()
# res
