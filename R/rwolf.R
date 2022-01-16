rwolf <- function(models, param, B, seed, package = "fwildclusterboot", ...){
  
  #' Romano-Wolf multiple hypotheses adjusted p-values 
  #' 
  #' Function implements the Romano-Wolf multiple hypthesis correction procedure for objects of type fixest_multi (fixest_multi are objects created by `fixest::feols()` that use `feols()` multiple-estimation interface). 
  #' Currently, the command is restricted to two-sided hypotheses and oneway clustered standard errors. For the wild cluster bootstrap, 
  #' the null is always imposed.
  #' @param models An object of type fixest_multi
  #' @param param The regression param to be tested
  #' @param B The number of bootstrap iterations
  #' @param seed Integer. Sets the random seed
  #' @param ... additional function values passed to the bootstrap function. 
  
  #' @import fwildclusterboot 
  #' @import wildboottestjlr
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
  #' res_rwolf <- rwolf(models = res, param = "X1", B = B)
  #' summary(res_rwolf)
  #' 
  #' @references 
  #' Clarke, Romano & Wolf (2019), STATA Journal. IZA working paper: https://ftp.iza.org/dp12845.pdf
  
  # Check if 'models' is of type fixest_multi
  if(!inherits(models, "fixest_multi")){
    stop("The object models needs to be of type fixest_multi.")
  }
  
  set.seed(seed)
  
  # get the model call
  call <- models[[1]]$call
  # get the name of the dependent variables
  depvars <- names(models)
  # get the number of tested hypotheses
  S <- length(models)
  # get the data frame used in the feols() call from the global environment
  data <- as.data.frame(eval(models[[1]]$call$data))            
  # additional cleaning steps required -> all cluster variables and fixed effects in model should be factors (this is what fixest does)
  N <- dim(data)[1]
  
  # get the clustering variable
  cluster <- as.character(models[[1]]$call$cluster)
  cluster <- cluster[which(cluster != "~")]
  
  # get the number of unique clusters
  G <- length(unique(data[, cluster]))
  
  # define a function to get statistics from fixest_multi object
  get_stats_fixest <- function(x, stat){
    res <- fixest::coeftable(models[lhs = x])[which(rownames(fixest::coeftable(models[lhs = x])) == param), stat]
    res
  }
  
  # and get coefs, t-stats and ses 
  # no absolute values for coefs, ses 
  coefs <- unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "Estimate")))
  ses <- unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "Std. Error")))
  # absolute value for t-stats
  t_stats <- abs(unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "t value"))))
  # t_stats <- abs(coefs / ses)
  
  # repeat line: for multiway clustering, it is not clear how many bootstrap 
  # test statistics will be invalied - for oneway, all vectors of length(boot_coefs) \leq B
  boot_coefs <- boot_ses <- boot_t_stats <- matrix(NA, B, S) 
  
  # run the bootstrap via fwildclusterboot or wildboottestjlr
  for(x in 1:length(models)){
    
    model <- models[[x]]
    
    # need to recalculate "original" t-statistics with null imposed"
    model$call <- rlang::call_modify(model$call, fml = model$fml)
    model$call <- rlang::call_modify(model$call, cluster = NULL)
    
    if(package == "fwildclusterboot"){
      res <- suppressMessages(
        fwildclusterboot:::boottest.fixest(object = model, 
                                           clustid = cluster, 
                                           param = param, 
                                           B = B, 
                                           conf_int = FALSE, 
                                           impose_null = TRUE)
      )
    } else if(package == "wildboottestjlr"){
      res <- suppressMessages(
        wildboottestjlr:::boottest.fixest(object = model, 
                                          clustid = cluster, 
                                          param = param, 
                                          B = B, 
                                          conf_int = FALSE, 
                                          impose_null = TRUE)
      )
    }
    
    # take absolute values of bootstrap t statistics
    t_stats[x] <- abs(res$t_stat[1])
    boot_t_stats[,x] <- abs(res$t_boot)     
    
    
  }
  
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



#' rwolf <- function(models, param, B, type = NULL, seed = NULL, ...){
#'   
#'   #' Romano-Wolf multiple hypotheses adjusted p-values 
#'   #' 
#'   #' Function implements the Romano-Wolf multiple hypthesis correction procedure for objects of type fixest_multi (fixest_multi are objects created by `fixest::feols()` that use `feols()` multiple-estimation interface). 
#'   #' Currently, the command is restricted to two-sided hypotheses and oneway clustered standard errors. Both pairs cluster bootstrap and wild cluster bootstrap are supported. For the wild cluster bootstrap, 
#'   #' the null is currently always imposed. Note that for a small number of clusters, using the wild cluster bootstrap will be much faster. 
#'   #' @param models An object of type fixest_multi
#'   #' @param param The regression param to be tested
#'   #' @param B The number of bootstrap iterations
#'   #' @param type The type of the bootstrap to use. Needs to be NULL or "wild". Both options will runs a wild cluster bootstrap.
#'   #' @param seed Integer. Sets the random seed
#'   #' @param ... additional function values passed to the bootstrap function. 
#'   #' @import fwildclusterboot 
#'   #' @importFrom data.table rbindlist
#'   #' @importFrom fixest coeftable
#'   #' @export
#'   #' 
#'   #' @examples
#'   #'  
#'   #' library(fixest)
#'   #' library(rwolf)
#'   #' 
#'   #' set.seed(12345)
#'   #' 
#'   #' N <- 1000
#'   #' X1 <- rnorm(N)
#'   #' Y1 <- 1 + 1 * X1 + rnorm(N)
#'   #' Y2 <- 1 + 0.01 * X1 + rnorm(N)
#'   #' Y3 <- 1 + 0.01 * X1 + rnorm(N)
#'   #' Y4 <- 1 + 0.01 * X1 + rnorm(N)
#'   #' 
#'   #' B <- 100
#'   #' # intra-cluster correlation of 0 for all clusters
#'   #' cluster <- rep(1:50, N / 50)
#'   #' 
#'   #' data <- data.frame(Y1 = Y1, 
#'   #'                    Y2 = Y2, 
#'   #'                    Y3 = Y3, 
#'   #'                    Y4 = Y4,
#'   #'                    X1 = X1, 
#'   #'                    cluster = cluster)
#'   #' 
#'   #' res <- feols(c(Y1, Y2, Y3, Y4) ~ X1, data = data, cluster = ~ cluster)
#'   #' res_rwolf <- rwolf(models = res, param = "X1", B = B)
#'   #' summary(res_rwolf)
#'   #' 
#'   #' @references 
#'   #' Clarke, Romano & Wolf (2019), STATA Journal. IZA working paper: https://ftp.iza.org/dp12845.pdf
#'   
#'   
#'   if(is.null(type)){
#'     type <- "wild"
#'   }
#'   
#'   #if(!(type %in% c("pairs", "wild"))){
#'   if(!(type %in% "wild")){
#'     stop("The only supported bootstrap types are ´pairs´ & ´wild´.")
#'   }
#'   
#'   # as of now: always impose null 
#'   if(type == "wild"){
#'     impose_null <- TRUE
#'   }
#'   
#'   # Check function arguments
#'   if(!inherits(models, "fixest_multi")){
#'     stop("The object models needs to be of type fixest_multi.")
#'   }
#'   
#'   if(length(param) != 1){
#'     stop("rwolf() currently only works for param values of length 1.")
#'   }
#'   
#'   if(is.null(seed)){
#'     seed <- 1
#'   }
#'   
#'   # if(is.null(cluster)){
#'   cluster <- as.character(models[[1]]$call$cluster)
#'   cluster <- cluster[which(cluster != "~")]
#'   
#'   if(length(cluster) == 0){
#'     cluster <- NULL
#'   }
#'   
#'   if(length(cluster) > 1){
#'     if(type != "wild"){
#'       # takes also care of no cluster defined in feols - bc length(character(NULL)) == 0
#'       message("Right now, rwolf() only supports oneway clustered standard errors with the ... bootstrap. As multiple clusters are specified, `rwolf()` will use a `wild bootstrap`.")
#'       type <- "wild"
#'     }
#'   }
#'   
#'   # case of no clustering
#'   if(length(cluster) == 0){
#'     message("No clustering variable specified in fixest_multi. Therefore, the bootstrap resamples at the individual level.")
#'     data$cluster <- 1:N
#'     cluster_vec <- unique(data$cluster)
#'     G <- length(cluster_vec)
#'   } else{
#'     cluster_vec <- unique(data[, cluster]) # vector, collects unique clusters
#'     G <- length(cluster_vec)      # number of unique clusters
#'   }
#'   
#'   if(length(cluster) > 1){
#'     stop("Currently, `rwolf()` only supports oneway clustering.")
#'   }
#'   
#'   # number_clusters <- length(unique(cluster_vec))
#'   if(type == "pairs" && G < 500 && B > 999){
#'     message(paste("The number of clusters is relatively small with", G, "clusters. Note that for such a small number of clusters, the wild cluster bootstrap might be significantly faster than the pairs bootstrap."))
#'   }
#'   
#'   set.seed(seed)
#'   call <- models[[1]]$call
#'   depvars <- names(models)
#'   S <- length(models)
#'   data <- as.data.frame(eval(models[[1]]$call$data))            # data needs to be pre-processed
#'   # additional cleaning steps required -> all cluster variables and fixed effects in model should be factors (this is what fixest does)
#'   N <- dim(data)[1]
#'   
#'   # function to get statistics from fixest_multi
#'   get_stats_fixest <- function(x, stat){
#'     res <- fixest::coeftable(models[lhs = x])[which(rownames(fixest::coeftable(models[lhs = x])) == param), stat]
#'     res
#'   }
#'   
#'   # S statistics from the "non-bootstrap" original estimations - absolute values
#'   # absolute values for two-sided test statistics
#'   
#'   # no absolute values for coefs, ses 
#'   coefs <- unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "Estimate")))
#'   ses <- unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "Std. Error")))
#'   # absolute value for t-stats
#'   t_stats <- abs(unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "t value"))))
#'   # t_stats <- abs(coefs / ses)
#'   
#'   
#'   if(type == "pairs"){
#'     
#'     # run the bootstrap:
#'     # note: this code part very closely follows code by Alan Fernihough
#'     # code at https://diffuseprior.wordpress.com/about/B, S
#'     boot_coefs <- boot_ses <- matrix(NA, B, S) 
#'     
#'     # just pick the first model's call - it is the same for all list elements in fixest_multi
#'     #call <- models[[1]]$call
#'     # boot_call <- models[[1]]
#'     # switch the "data" in the models estimation call
#'     #call$data <- quote(boot_data)  
#'     
#'     model_call <- models[[1]]$call
#'     #fml_new <- quote(eval(depvars[x]))
#'     model_call <- rlang::call_modify(model_call, data = quote(boot_data)) 
#'     
#'     for(b in 1:B){
#'       # create bootstrap cluster vector & new bootstrap data
#'       units <- sample(cluster_vec, G, TRUE)
#'       df.bs <- lapply(units, function(x) which(data[, cluster] == x)) # index of obs to keep
#'       x <- unlist(df.bs)                                           
#'       boot_data <- data[x,]
#'       
#'       # evaluate models' call with bootstrap sample
#'       boot_est <- eval(model_call)
#'       boot_coefs[b,] <- unlist(lapply(1:S, function(s) fixest::coeftable(boot_est[lhs = s])[which(rownames(fixest::coeftable(boot_est[lhs = s])) == param), "Estimate"]))
#'       boot_ses[b,] <- unlist(lapply(1:S, function(s) fixest::coeftable(boot_est[lhs = s])[which(rownames(fixest::coeftable(boot_est[lhs = s])) == param), "Std. Error"]))
#'     }
#'     # calculate re-centered bootstrap t-stats (boot_coefs are centered around )
#'     boot_t_stats <-   abs( t(t(boot_coefs) - coefs) / boot_ses)
#'     
#'     
#'     if(any(boot_t_stats < 0) || any(t_stats < 0)){
#'       stop("There are negative boot_t_stats.")
#'     }
#'     
#'   } else if (type == "wild"){
#'     
#'     # repeat line: for multiway clustering, it is not clear how many bootstrap 
#'     # test statistics will be invalied - all vectors of length(boot_coefs) \leq B
#'     boot_coefs <- boot_ses <- boot_t_stats <- matrix(NA, B, S) 
#'     
#'     # get all outcome variables & formulas
#'     # for all hypotheses, run fwildclusterboot::boottest()
#'     for(x in 1:length(models)){
#'       
#'       model <- models[[x]]
#'       # update model$call$fml so that only one outcome variable left
#'       #model$call$fml <- 
#'       if(is.null(impose_null) || impose_null == TRUE){
#'         # need to recalculate "original" t-statistics with null imposed"
#'         model$call <- rlang::call_modify(model$call, fml = model$fml)
#'         model$call <- rlang::call_modify(model$call, cluster = NULL)
#'         
#'         res <- suppressMessages(
#'           fwildclusterboot:::boottest.fixest(object = model, 
#'                                              clustid = cluster, 
#'                                              param = param, 
#'                                              B = B, 
#'                                              conf_int = FALSE, 
#'                                              impose_null = TRUE)
#'         )
#'         t_stats[x] <- abs(res$t_stat[1])
#'         boot_t_stats[,x] <- abs(res$t_boot)     # first t-stat excluded as it is just the "reg"  
#'       } else if(impose_null == FALSE){
#'         
#'         model <- models[[x]]
#'         
#'         #fml_new <- quote(eval(depvars[x]))
#'         model$call <- rlang::call_modify(model$call, fml = model$fml)
#'         model$call <- rlang::call_modify(model$call, cluster = NULL)
#'         
#'         
#'         res <- fwildclusterboot:::boottest.fixest(object = model, 
#'                                                   clustid = cluster, 
#'                                                   param = param, 
#'                                                   B = B, 
#'                                                   conf_int = FALSE, 
#'                                                   impose_null = FALSE)
#'         boot_t_stats[,x] <- res$t_boot
#'       }
#'     }
#'     # retain estimated t-stats
#'     
#'   }
#'   
#'   
#'   # stepwise p-value calculation
#'   # note: this code part very closely follows the p_adjust function from the hdm
#'   # package, written and maintained by Martin Spindler
#'   # code at https://github.com/cran/hdm/blob/master/R/p_adjust.R
#'   
#'   pinit <- corr.padj <- pval <- vector(mode = "numeric", length = S)
#'   stepdown.index <- order(t_stats, decreasing = TRUE)
#'   ro <- order(stepdown.index)
#'   
#'   for(s in 1:S){
#'     if(s == 1){
#'       max_stat <- apply(boot_t_stats, 1, max)
#'       pinit[s] <- pmin(1, 
#'                        (sum(max_stat >= abs(t_stats[stepdown.index[s]])) + 1) / (B + 1) 
#'       )
#'     }
#'     if(s > 1){
#'       boot_t_stat_udp <- boot_t_stats[, -stepdown.index[1:(s-1)], drop = FALSE]     # drop max statistic
#'       max_stat <- apply(boot_t_stat_udp, 1, max)                                    # for each B, calculate max S
#'       pinit[s] <- pmin(1, 
#'                        (sum(max_stat >= abs(t_stats[stepdown.index[s]])) + 1)
#'                        / (B + 1)
#'       )
#'     }
#'   }
#'   
#'   for(j in 1:S){
#'     if(j == 1){
#'       corr.padj[j] <- pinit[j]
#'     }
#'     if(j > 1){
#'       corr.padj[j] <- max(pinit[j], corr.padj[j - 1])
#'     }
#'   }
#'   
#'   # collect the results
#'   pval <- corr.padj[ro]
#'   
#'   models_info <- data.table::rbindlist(
#'     lapply(1:S, function(x){
#'       tmp <- coeftable(models[[x]])
#'       tmp1 <- tmp[which(rownames(tmp) == param),]
#'       tmp1$depvar <- as.character(models[[x]]$fml[[2]])
#'       tmp1
#'     })
#'   )
#'   # some reordering
#'   models_info <- models_info[, c(5, 1:4)]
#'   models_info[, "RW Pr(>|t|)"] <- pval
#'   rownames(models_info) <- depvars
#'   
#'   res <- list(
#'     call = call,
#'     models_info = models_info,
#'     coefs = coefs,
#'     # ses = ses,
#'     t_stats = t_stats,
#'     boot_coefs = boot_coefs,
#'     boot_ses = boot_ses,
#'     boot_t_stats = boot_t_stats,
#'     pval = pval
#'   )
#'   
#'   class(res) <- "rwolf"
#'   
#'   invisible(res)
#'   
#' }
#' 
#' 
