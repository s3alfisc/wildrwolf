rwolf <- function(models, param, B, R = NULL, r = 0, p_val_type = "two-tailed", weights_type = "rademacher", seed = NULL, boot_algo = "R", nthreads = 1, ...){
  
  #' Romano-Wolf multiple hypotheses adjusted p-values 
  #' 
  #' Function implements the Romano-Wolf multiple hypthesis correction procedure for objects of type fixest_multi (fixest_multi are objects created by `fixest::feols()` that use `feols()` multiple-estimation interface). 
  #' Currently, the command is restricted to two-sided hypotheses and oneway clustered standard errors. For the wild cluster bootstrap, 
  #' the null is always imposed.
  #' @param models An object of type fixest_multi or a list of objects of type fixest
  #' @param param The regression parameter to be tested
  #' @param R Hypothesis Vector giving linear combinations of coefficients. Must be either NULL or a vector of the same length as `param`. If NULL, a vector of ones of length param.
  #' @param r A numeric. Shifts the null hypothesis 
  #'        H0: param = r vs H1: param != r  
  #' @param B The number of bootstrap iterations
  #' @param p_val_type Character vector of length 1. Type of hypothesis test 
  #'        By default "two-tailed". Other options include "equal-tailed", ">" and "<". 
  #' @param weights_type character or function. The character string specifies the type
  #'                     of boostrap to use: One of "rademacher", "mammen", "norm"
  #'                     and "webb". Alternatively, type can be a function(n) for drawing 
  #'                     wild bootstrap factors. "rademacher" by default.  
  #'                     For the Rademacher distribution, if the number of replications B exceeds 
  #'                     the number of possible draw ombinations, 2^(#number of clusters), then `boottest()` 
  #'                     will use each possible combination once (enumeration).               
  #' @param seed Integer. Sets the random seed. NULL by default. 
  #' @param boot_algo Should the wild cluster bootstrap run via fwildclusterboot's R implementation or via WildBootTests.jl? 'R' by default. The other option is 'WildBootTests.jl'.
  #' @param nthreads Integer. The number of threads to use. 
  #' @param ... additional function values passed to the bootstrap function. 
  
  #' @importFrom fwildclusterboot boottest
  #' @importFrom data.table rbindlist
  #' @importFrom fixest coeftable
  #' @importFrom dreamerr check_arg
  #' @importFrom stats terms formula
  #' @export
  #' 
  #' @examples
  #'  
  #' library(fixest)
  #' library(wildrwolf)
  #' 
  #' set.seed(12345)
  #' 
  #' N <- 1000
  #' X1 <- rnorm(N)
  #' Y1 <- 1 + 1 * X1 + rnorm(N)
  #' Y2 <- 1 + 0.01 * X1 + rnorm(N)
  #' Y3 <- 1 + 0.01 * X1 + rnorm(N)
  #' Y4 <- 1 + 0.01 * X1 + rnorm(N)
  #' 
  #' B <- 999
  #' # intra-cluster correlation of 0 for all clusters
  #' cluster <- rep(1:50, N / 50)
  #' 
  #' data <- data.frame(Y1 = Y1, 
  #'                    Y2 = Y2, 
  #'                    Y3 = Y3, 
  #'                    Y4 = Y4,
  #'                    X1 = X1, 
  #'                    cluster = cluster)
  #' 
  #' res <- feols(c(Y1, Y2, Y3, Y4) ~ X1, data = data, cluster = ~ cluster)
  #' res_rwolf <- rwolf(models = res, param = "X1", B = B)
  #' summary(res_rwolf)
  #' 
  #' @references 
  #' Clarke, Romano & Wolf (2019), STATA Journal. IZA working paper: https://ftp.iza.org/dp12845.pdf
  
  
  check_arg(param, "character vector | character scalar | formula")
  check_arg(R, "NULL | numeric vector")
  check_arg(r, "NULL | numeric scalar")
  check_arg(p_val_type, "charin(two_sided, >, <)")
  check_arg(B, "integer scalar GT{99}")
  check_arg(seed, "integer scalar | NULL")
  check_arg(boot_algo, "charin(R, R-lean, WildBootTests.jl)")
  check_arg(nthreads, "scalar integer")
  
  
  # Check if 'models' is of type fixest_multi
  if(!inherits(models, "fixest_multi")){
  } else if(inherits(models, "list")){
    fixest_list <- mean(sapply(models, class) == "fixest") == 1L
    if(!fixest_list){
      stop("The object models needs to be either of type 'fixest_multi' or a list of objects of type 'fixest'.")
    }
  }

  
  call <- models[[1]]$call
  S <- length(models)
    
  # define a function to get statistics from fixest_multi object
  get_stats_fixest <- function(x, stat){
    res <- fixest::coeftable(models[[x]])[which(rownames(fixest::coeftable(models[[x]])) == param), stat]
    res
  }
    
  # and get coefs, t-stats and ses 
  # no absolute values for coefs, ses 
  coefs <- unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "Estimate")))
  ses <- unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "Std. Error")))
  # absolute value for t-stats
  t_stats <- abs(unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "t value"))))
  # repeat line: for multiway clustering, it is not clear how many bootstrap 
  # test statistics will be invalied - for oneway, all vectors of length(boot_coefs) \leq B
  
  boot_coefs <- boot_ses <- matrix(NA, B, S) 
  boot_t_stats <- list()
  
  res <- 
    lapply(seq_along(models), 
           function(x){
             
             clustid <- models[[x]]$call$cluster
             
             if(!is.null(clustid)){
               boottest(
                 models[[x]],
                 param = param,
                 B = B, 
                 R = R, 
                 r = r, 
                 boot_algo = boot_algo, 
                 p_val_type = p_val_type, 
                 type = weights_type, 
                 clustid = formula(clustid))
             } else {
               boottest(
                 models[[x]],
                 param = param,
                 B = B,
                 R = R, 
                 r = r, 
                 boot_algo = boot_algo, 
                 p_val_type = p_val_type, 
                 type = weights_type
                 )
             }
           
             
           })
  
  for(x in seq_along(models)){
    # take absolute values of bootstrap t statistics
    t_stats[x] <- abs(res[[x]]$t_stat)
    boot_t_stats[[x]] <- abs(res[[x]]$t_boot)     
  }
  
  boot_t_stats <- Reduce(cbind, boot_t_stats)
  
  # after calculating all bootstrap t statistics, initiate the RW procedure

  # stepwise p-value calculation
  # note: this code part very closely follows the p_adjust function from the hdm
  # package, written and maintained by Martin Spindler
  # code at https://github.com/cran/hdm/blob/master/R/p_adjust.R
  
  pinit <- corr.padj <- pval <- vector(mode = "numeric", length = S)
  stepdown.index <- order(t_stats, decreasing = TRUE)
  ro <- order(stepdown.index)
  
  for(s in 1:S){
    if(s == 1){
      max_stat <- apply(boot_t_stats, 1, max)
      pinit[s] <- pmin(1, 
                       (sum(max_stat >= abs(t_stats[stepdown.index[s]])) + 1) / (B + 1) 
      )
    }
    if(s > 1){
      boot_t_stat_udp <- boot_t_stats[, -stepdown.index[1:(s-1)], drop = FALSE]     # drop max statistic
      max_stat <- apply(boot_t_stat_udp, 1, max)                                    # for each B, calculate max S
      pinit[s] <- pmin(1, 
                       (sum(max_stat >= abs(t_stats[stepdown.index[s]])) + 1)
                       / (B + 1)
      )
    }
  }
  
  for(j in 1:S){
    if(j == 1){
      corr.padj[j] <- pinit[j]
    }
    if(j > 1){
      corr.padj[j] <- max(pinit[j], corr.padj[j - 1])
    }
  }
  
  # collect the results
  pval <- corr.padj[ro]
  
  
  # summarize all results 
  models_info <- data.table::rbindlist(
    lapply(1:S, function(x){
      tmp <- coeftable(models[[x]])
      tmp1 <- tmp[which(rownames(tmp) == param),]
      suppressWarnings(tmp1$depvar <- as.character(models[[x]]$fml[[2]]))
      tmp1$model <- paste("Model", x)
      tmp1
    })
  )
  
  # some reordering
  models_info <- models_info[, c(6,5, 1:4)]
  models_info[, "RW Pr(>|t|)"] <- pval

  res <- list(
    call = call,
    models_info = models_info,
    coefs = coefs,
    # ses = ses,
    t_stats = t_stats,
    boot_coefs = boot_coefs,
    boot_ses = boot_ses,
    boot_t_stats = boot_t_stats,
    pval = pval
  )
  
  # create class of type rwolf
  class(res) <- "rwolf"
  
  invisible(res)
  
  
}

summary.rwolf <- function(object, digits, ...){
  #' Summary method for objects of type rwolf
  #' @param object An object of type rwolf
  #' @param digits Rounding of digits
  #' @param ... misc. function arguments
  #' @export
  as.data.frame(object$models_info)
}



