fwer_sim <- function(rho, N, s, B, G = 20){

  #' Simulate data as in Clarke, Romano & Wolf (2019) to simulate family wise
  #' error rates (FWERs)
  #'
  #' @param rho The correlation between the outcome variables
  #' @param N The number of observations
  #' @param s The number of dependent variables
  #' @param B The number of bootstrap draws e
  #' @param G The number of clusters. If NULL, no clustering.
  #'
  #' @importFrom stats p.adjust
  #' @importFrom MASS mvrnorm
  #' @importFrom fabricatr draw_normal_icc
  #' 
  #' @return
  #' A 'data.frame' containing unadjusted p-values & p-values adjusted using the
  #' methods by Holm and Romano & Wolf (2005)


  dreamerr::check_arg(N, "integer scalar")
  dreamerr::check_arg(B, "integer scalar")
  dreamerr::check_arg(s, "integer scalar")
  dreamerr::check_arg(rho, "numeric scalar")
  dreamerr::check_arg(G, "integer scalar")
  

  if(s %% 2 != 0){
    stop("s needs to be divisible by 2.")
  }

  coefs <- list(
    rep(0, s)
  )

    beta <- coefs[[1]]
    intercept <- 1
    treatment <- sample(0:1, N, TRUE)
    # true effect of beta_1 = 0 in each simulations

      Sigma <- matrix(rho, s, s); diag(Sigma) <- 1
      e <- MASS::mvrnorm(n = N, mu = rep(0, s), Sigma)
      
      if(is.null(G)){
        Y <- matrix(NA, N, s)
        for(x in 1:s){
          Y[,x] <- intercept + beta[x] * treatment + e[,x]
        } 
        data2 <- cbind(Y, treatment)
        data2 <- as.data.frame(data2)
        names(data2) <- c(paste0("Y", 1:s), "treatment")
        
      } else {
        Y <- matrix(NA, N, s)
        clusters <- sample(1:G, N, TRUE)
        for(x in 1:s){
          cluster_error <- fabricatr::draw_normal_icc(
            clusters = clusters, ICC = 0.5)
          Y[,x] <- intercept + beta[x] * treatment + cluster_error + e[,x]
        }
        data2 <- cbind(Y, clusters, treatment)
        data2 <- as.data.frame(data2)
        names(data2) <- c(paste0("Y", 1:s), "cluster", "treatment")
      }   


      data2 <<- data2
      
      if(s == 6){
        fml <- c(Y1, Y2, Y3, Y4, Y5, Y6) ~ treatment
      } else if(s == 8){
        fml <- c(Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8) ~ treatment
      } else if(s == 10){
        fml <- c(Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10) ~ treatment
      }

      if(is.null(G)){
        fit <- fixest::feols(
          fml,
          data = data2
        )
      } else {
        fit <- fixest::feols(
          fml,
          data = data2,
          cluster= ~cluster
        )
      }


    fit_pvalue <- lapply(fit, function(x) fixest::pvalue(x))
    fit_pvalue <- Reduce("rbind", fit_pvalue)[,"treatment"]

    # "11" bootstrap variants"
    fit_padjust_rw <- wildrwolf::rwolf(
      fit,
      param = ~treatment,
      B = B,
      nthreads = 1,
      bootstrap_type = "11"
    )$pval
    
    # fit_padjust_wy <- wildwyoung::wyoung(
    #   fit,
    #   param = ~treatment,
    #   B = B,
    #   nthreads = 1,
    #   bootstrap_type = "11"
    # )$pval
 
    fit_pvalue_holm <- p.adjust(fit_pvalue, method = "holm")

    data.frame(
      fit_pvalue = fit_pvalue,
      fit_pvalue_holm = fit_pvalue_holm,
      fit_padjust_rw = fit_padjust_rw#,
      #fit_padjust_wy = fit_padjust_wy,


    )

}

run_fwer_sim <- function(
    n_sims = 100,
    rho = c(0, 0.25, 0.5, 0.75),
    seed = 114411,
    B = 499,
    N = 1000,
    s = 6, 
    G = 20){
  #' 
  #' Run a MC simulation study on family-wise error rates (FWERs)
  #' for the Holm and Romano & Wolf Methods multiple
  #' hypothesis adjustment methods given true null effects
  #'
  #' @param n_sims The number of Monte Carlo iterations. 100 by default.
  #' @param rho The correlation between the outcome variables. Vectorized 
  #'        c(0, 0.25, 0.5, .75) by default
  #' @param seed A random seed.
  #' @param N The number of observations. 1000 by default.
  #' @param s The number of dependent variables. 6 by default.
  #' @param B The number of bootstrap draws. 499 by default.
  #' @param G The number of clusters. If NULL, no clustering. 20 by default
  #'
  #' @export
  #'
  #'
  #' @examples
  #' \dontrun{
  #'
  #' res <- run_fwer_sim(
  #'   seed = 123,
  #'   n_sims = 1000,
  #'   B = 4999,
  #'   N = 1000,
  #'   s = 10
  #' )
  #' res
  #'
  #' }

  set.seed(seed)

  all_rho <- list()
  
  for(x in seq_along(rho)){
    
    all_rho_i <- list()
    for(i in 1:n_sims){
      all_rho_i[[i]] <- fwer_sim(rho = rho[x], B = B, N = N, s = s, G = G)
    }
    all_rho[[x]] <- all_rho_i
  }

    # 1) calculate rejection rates
  all_sims <- list()
  for(x in seq_along(rho)){

    reject_5 <- reject_10 <- list()
    for(i in 1:n_sims){
      rho_i <- all_rho[[x]][[i]]
      # calculate FWER
      reject_5[[i]] <- sapply(rho_i, function(x){any(x < 0.05)})
      reject_10[[i]] <- sapply(rho_i, function(x){any(x < 0.10)})
    }
    reject_5 <- Reduce("+", reject_5) / n_sims
    reject_10 <- Reduce("+", reject_10) / n_sims
    all_sims[[x]] <- data.frame(
      reject_5 = reject_5, 
      reject_10 = reject_10, 
      rho = rho[x]
    )
    
  }

  all_sims <- Reduce("rbind", all_sims)
  all_sims
}
