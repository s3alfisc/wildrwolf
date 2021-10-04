rwolf <- function(models, parameter, B, seed = NULL, type = "pairs", ...){

  #' Function implements the Romano-Wolf multiple hypthesis correction procedure for objects of type fixest_multi (fixest_multi are objects created by `fixest::feols()` that use `feols()` multiple-estimation interface). 
  #' Currently, the command is restricted to two-sided hypotheses and oneway clustered standard errors. Both pairs cluster bootstrap and wild cluster bootstrap are supported. For the wild cluster bootstrap, 
  #' the null is currently always imposed. Note that for a small number of clusters, using the wild cluster bootstrap will be much faster. 
  #' Author: Alexander Fischer
  #' Acknowledgements: 
  #' The code behind `rwolf()` heavily relies on code written by 
  #' - Martin Spindler and
  #' - Alan Fernihough. 
  #' See appropriately commented sections in the code. 
  #' @param models An object of type fixest_multo
  #' @param parameter The regression parameter to be tested
  #' @param B The number of bootstrap iterations
  #' @param type The type of the bootstrap to use. The ´pairs´ bootstrap by default. "wild" runs a wild cluster bootstrap.
  #' @param seed Integer. Sets the random seed
  #' @param ... additional function values passed to the bootstrap function. 
  #' @import progress
  #' @import fwildclusterboot 
  #' @importFrom data.table rbindlist
  #' @importFrom fixest coeftable
  #' @export
  #' 
  #' @examples
  #'  
  #' library(fixest)
  #' library(rwolf)
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
  #' B <- 100
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
  #' res_rwolf <- rwolf(models = res, parameter = "X1", B = B, type = NULL)
  #' summary(res_rwolf)
  #' 
  #' @references 
  #' Clarke, Romano & Wolf (2019), STATA Journal. IZA working paper: https://ftp.iza.org/dp12845.pdf


  if(!(type %in% c("pairs", "wild"))){
    stop("The only supported bootstrap types are ´pairs´ & ´wild´.")
  }
  
  # as of now: always impose null 
  if(type == "wild"){
    impose_null <- TRUE
  }

  # Check function arguments
  if(!inherits(models, "fixest_multi")){
    stop("The object models needs to be of type fixest_multi.")
  }

  if(length(parameter) != 1){
    stop("rwolf() currently only works for parameters of length 1.")
  }

  if(is.null(seed)){
    seed <- 1
  }

  # if(is.null(cluster)){
  cluster <- as.character(models[[1]]$call$cluster)
  cluster <- cluster[which(cluster != "~")]
  
  if(length(cluster) == 0){
    cluster <- NULL
  }
  
  if(length(cluster) > 1){
    if(type != "wild"){
      # takes also care of no cluster defined in feols - bc length(character(NULL)) == 0
      message("Right now, rwolf() only supports oneway clustered standard errors with the ... bootstrap. As multiple clusters are specified, `rwolf()` will use a `wild bootstrap`.")
      type <- "wild"
    }
  }
  
  # case of no clustering
  if(length(cluster) == 0){
    message("No clustering variable specified in fixest_multi. Therefore, the bootstrap resamples at the individual level.")
    data$cluster <- 1:N
    cluster_vec <- unique(data$cluster)
    n_clusters <- length(cluster_vec)
  } else{
    cluster_vec <- unique(data[, cluster])
    n_clusters <- length(cluster_vec)
  }
  
  # if(dim(cluster_vec)[1] > 1){
  #   stop("Currently, `rwolf()` only supports oneway clustering.")
  # }

  number_clusters <- length(unique(cluster_vec))
  if(type == "pairs" && number_clusters < 500){
    message(paste("The number of clusters is relatively small with", number_clusters, "clusters. Note that for such a small number of clusters, the wild cluster bootstrap might be significantly faster than the pairs bootstrap."))
  }
  
  set.seed(seed)
  call <- models[[1]]$call
  depvars <- names(models)
  S <- length(models)
  data <- as.data.frame(eval(models[[1]]$call$data))
  N <- dim(data)[1]

  # function to get statistics from fixest_multi
  get_stats_fixest <- function(x, stat){
    res <- fixest::coeftable(models[lhs = x])[which(rownames(fixest::coeftable(models[lhs = x])) == parameter), stat]
    res
  }

  # S statistics from the "non-bootstrap" original estimations - absolute values
  # absolute values for two-sided test statistics
  
  t_stats <- abs(unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "t value"))))
  coefs <- abs(unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "Estimate"))))
  ses <- abs(unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "Std. Error"))))

  if(type == "pairs"){
    # bootstrap function:
    boot_function <- function(data, x){
      
      pb$tick()
      
      boot_data <- data[x, ]
      res <- list()
      
      # just pick the first model's call - it is the same for all list elements in fixest_multi
      boot_call <- models[[1]]
      # switch the "data" in the models estimation call
      boot_call$call$data <- quote(boot_data)
      # evaluate models' call with bootstrap sample
      boot_est <- eval(boot_call$call)
      res[["boot_coefs"]] <- unlist(lapply(1:S, function(s) fixest::coeftable(boot_est[lhs = s])[which(rownames(fixest::coeftable(boot_est[lhs = s])) == parameter), "Estimate"]))
      res[["boot_ses"]] <- unlist(lapply(1:S, function(s) fixest::coeftable(boot_est[lhs = s])[which(rownames(fixest::coeftable(boot_est[lhs = s])) == parameter), "Std. Error"]))
      # calculate re-centered bootstrap t-stats
      res[["boot_t_stats"]] <- abs(res[["boot_coefs"]] - coefs) / res[["boot_ses"]]
      # gc() as fixest_multi can be a very large object
      gc()
      res
    }
    
    # initiate progress bar:
    pb <- progress::progress_bar$new(total = B)
    pb$tick(0)
    
    # run the bootstrap:
    # note: this code part very closely follows code by Alan Fernihough
    # code at https://diffuseprior.wordpress.com/about/B, S
    boot_coefs <- boot_ses <- boot_t_stats <- matrix(NA, B, S) 
    
    for(b in 1:B){
      units <- sample(cluster_vec, n_clusters, TRUE)
      df.bs <- sapply(units, function(x) which(data[, cluster] == x))
      x <- unlist(df.bs)
      res <- boot_function(data = data, x = x)
      boot_coefs[b, ] <- res$boot_coefs
      boot_ses[b, ] <- res$boot_ses
      boot_t_stats[b, ] <- res$boot_t_stats
    }
  } else if (type == "wild"){
    
    # repeat line: for multiway clustering, it is not clear how many bootstrap 
    # test statistics will be invalied - all vectors of length(boot_coefs) \leq B
    boot_coefs <- boot_ses <- boot_t_stats <- matrix(NA, B, S) 
    
    # get all outcome variables & formulas
    # for all hypotheses, run fwildclusterboot::boottest()
    for(x in 1:length(models)){
      
      model <- models[[x]]
      # update model$call$fml so that only one outcome variable left
      #model$call$fml <- 
      if(is.null(impose_null) || impose_null == TRUE){
        # need to recalculate "original" t-statistics with null imposed"
        model$call <- rlang::call_modify(model$call, fml = model$fml)
        model$call <- rlang::call_modify(model$call, cluster = NULL)
        
        res <- suppressMessages(
          fwildclusterboot:::boottest.fixest(object = model, 
                                                  clustid = cluster, 
                                                  param = parameter, 
                                                  B = B, 
                                                  conf_int = FALSE, 
                                                  impose_null = TRUE)
        )
        t_stats[x] <- abs(res$t_stat[1])
        boot_t_stats[,x] <- abs(res$t_boot)     # first t-stat excluded as it is just the "reg"  
      } else if(impose_null == FALSE){

        model <- models[[x]]
        
        #fml_new <- quote(eval(depvars[x]))
        model$call <- rlang::call_modify(model$call, fml = model$fml)
        model$call <- rlang::call_modify(model$call, cluster = NULL)
        
        
        res <- fwildclusterboot:::boottest.fixest(object = model, 
                                                  clustid = cluster, 
                                                  param = parameter, 
                                                  B = B, 
                                                  conf_int = FALSE, 
                                                  impose_null = FALSE)
        boot_t_stats[,x] <- res$t_boot
      }
    }
    # retain estimated t-stats
    
  }
  

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

  models_info <- data.table::rbindlist(
    lapply(1:S, function(x){
      tmp <- coeftable(models[[x]])
      tmp1 <- tmp[which(rownames(tmp) == parameter),]
      tmp1$depvar <- as.character(models[[x]]$fml[[2]])
      tmp1
    })
  )
  # some reordering
  models_info <- models_info[, c(5, 1:4)]
  models_info[, "RW Pr(>|t|)"] <- pval
  rownames(models_info) <- depvars

  res <- list(
    call = call,
    models_info = models_info,
    coefs = coefs,
    ses = ses,
    t_stats = t_stats,
    boot_coefs = boot_coefs,
    boot_ses = boot_ses,
    boot_t_stats = boot_t_stats,
    pval = pval
  )

  class(res) <- "rwolf"

  invisible(res)

}


summary.rwolf <- function(object, digits = 3, ...){
  #' Summary method for objects of type rwolf
  #' @param object An object of type rwolf
  #' @param digits Rounding of digits
  #' @export
  stopifnot(inherits(object,"rwolf"))
  call <- object$call
  print(call)
  as.data.frame(object$models_info)
}

