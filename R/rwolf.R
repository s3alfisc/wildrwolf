rwolf <- function(models, parameter, B, cluster, seed){

  #' Function implements the Romano-Wolf multiple hypthesis correction procedure
  #' for objects of type fixest_multi
  #' @param models An object of type fixest_multo
  #' @param parameter The regression parameter to be tested
  #' @param B The number of bootstrap iterations
  #' @param cluster A character vector for clustered standard errors. If NULL, no
  #'                clustering is employed
  #' @param seed Integer. Sets the random seed
  #' @import progress
  #' @import data.table
  #' @import fixest

  # Check function arguments
  if(!inherits(models, "fixest_multi")){
    stop("The object models needs to be of type fixest_multi.")
  }

  if(length(parameter) != 1){
    stop("rwolf() currently only works for parameters of length 1.")
  }

  set.seed(seed)
  data <- as.data.frame(eval(models[[1]]$call$data))
  N <- dim(data)[1]
  call <- models[[1]]$call
  depvars <- names(models)
  S <- length(models)

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
    res[["boot_coef"]] <- unlist(lapply(1:S, function(s) fixest::coeftable(boot_est[lhs = s])[which(rownames(fixest::coeftable(boot_est[lhs = s])) == parameter), "Estimate"]))
    res[["boot_se"]] <- unlist(lapply(1:S, function(s) fixest::coeftable(boot_est[lhs = s])[which(rownames(fixest::coeftable(boot_est[lhs = s])) == parameter), "Std. Error"]))
    # calculate re-centered bootstrap t-stats
    res[["boot_t_stat"]] <- abs((res["boot_coef"]) - coefs) / res[["boot_se"]]
    # gc() as fixest_multi can be a very large object
    gc()
    res
  }

  # initiate progress bar:
  pb <- progress::progress_bar$new(total = B)
  pb$tick(0)

  # case of no clustering
  if(is.null(cluster)){
    cluster <- 1:N
  }

  cluster_vec <- unique(data[, cluster])
  n_clusters <- length(cluster_vec)

  # run the bootstrap:
  # note: this code part very closely follows code by Alan Fernihough
  # code at https://diffuseprior.wordpress.com/about/
  for(b in 1:B){
    units <- sample(cluster_vec, n_clusters, TRUE)
    df.bs <- sapply(units, function(x) which(data[, cluster] == x))
    x <- unlist(df.bs)
    res <- boot_function(data = data, x = x)
    boot_coefs[b, ] <- res$boot_coef
    boot_ses[b, ] <- res$boot_ses
    boot_t_stats[b, ] <- res$boot_t_stat
  }

  # stepwise p-value calculation
  # note: this code part very closely follows the p_adjust function from the hdm
  # package, written and maintained by Martin Spindler
  # code at https://github.com/cran/hdm/blob/master/R/p_adjust.R

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
      tmp <- tmp[which(rownames(tmo) == parameter),]
      tmp$depvar <- as.character(models[[x]]$fml[[2]])
      tmp
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
