rwolf <- function(models, parameter, B, seed = NULL){

  #' Function implements the Romano-Wolf multiple hypthesis correction procedure
  #' for objects of type fixest_multi
  #' @param models An object of type fixest_multo
  #' @param parameter The regression parameter to be tested
  #' @param B The number of bootstrap iterations
  #' @param seed Integer. Sets the random seed
  #' @import progress
  #' @import data.table
  #' @import fixest
  #' @export

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
  if(!(length(cluster) %in% 1)){
    # takes also care of no cluster defined in feols - bc length(character(NULL)) == 0
    stop("Right now, rwolf() only supports oneway clustered standard errors. Please 
         specify the cluster via the `cluster` variabe in `feols()`.")
  }
  # case of no clustering
  if(length(cluster) == 0){
    data$cluster <- 1:N
    cluster_vec <- unique(data$cluster)
    n_clusters <- length(cluster_vec)
  } else{
    cluster_vec <- unique(data[, cluster])
    n_clusters <- length(cluster_vec)
  }
  

  #}
  
  set.seed(seed)
  call <- models[[1]]$call
  depvars <- names(models)
  S <- length(models)
  data <- as.data.frame(eval(models[[1]]$call$data))
  N <- dim(data)[1]
  
  # if(is.null(cluster)){
  #   cluster_fml <- models[[1]]$call$cluster
  #   cluster <- model.frame(cluster_fml, data)
  # }
  
  # if(dim(cluster)[2] > 1){
  #   stop("rwolf() currently only works for oneway clustering.")
  # }

  # function to get statistics from fixest_multi
  get_stats_fixest <- function(x, stat){
    res <- fixest::coeftable(models[lhs = x])[which(rownames(fixest::coeftable(models[lhs = x])) == parameter), stat]
    res
  }

  t_stats <- abs(unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "t value"))))
  coefs <- abs(unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "Estimate"))))
  ses <- abs(unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "Std. Error"))))

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
      pinit[s] <- pmin(1, (sum(max_stat >= abs(t_stats[stepdown.index[s]]) + 1) / (B + 1)))
    }
    if(s > 1){
      boot_t_stat_udp <- boot_t_stats[, -stepdown.index[1:(s-1)], drop = FALSE]
      max_stat <- apply(boot_t_stat_udp, 1, max)
      pinit[s] <- pmin(1, (sum(max_stat >= abs(t_stats[stepdown.index[s]])) + 1 / (B + 1)))
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

