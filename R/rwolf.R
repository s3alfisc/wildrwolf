#' Romano-Wolf multiple hypotheses adjusted p-values 
#' 
#' Function implements the Romano-Wolf multiple hypothesis correction procedure
#' for objects of type `fixest_multi` (`fixest_multi` are objects created by 
#' `fixest::feols()` that use `feols()` multiple-estimation interface).  
#' The null hypothesis is always imposed on the bootstrap dgp.
#' 
#' @param models An object of type `fixest_multi` or a list of objects of 
#'        type `fixest`, estimated via ordinary least squares (OLS)
#' @param param The regression parameter to be tested
#' @param R Hypothesis Vector giving linear combinations of coefficients.
#'  Must be either NULL or a vector of the same length as `param`. 
#'  If NULL, a vector of ones of length param.
#' @param r A numeric. Shifts the null hypothesis 
#'        H0: `param.` = r vs H1: `param.` != r  
#' @param B The number of bootstrap iterations
#' @param p_val_type Character vector of length 1. Type of hypothesis test 
#'        By default "two-tailed". Other options include "equal-tailed"
#'         (for one-sided tests), ">" and "<" (for two-sided tests). 
#' @param weights_type character or function. The character string specifies 
#' the type of bootstrap to use: One of "rademacher", "mammen", "norm"
#' and "webb". Alternatively, type can be a function(n) for drawing 
#' wild bootstrap factors. "rademacher" by default.  For the Rademacher 
#' distribution, if the number of replications B exceeds the number of possible
#' draw ombinations, 2^(#number of clusters), then `boottest()` will use each 
#' possible combination once (enumeration). 
#' @param bootstrap_type Either "11", "13", "31", "33", or "fnw11". 
#' "fnw11" by default. See `?fwildclusterboot::boottest` for more details  
#' @param engine Should the wild cluster bootstrap run via `fwildclusterboot's`
#'  R implementation or via `WildBootTests.jl`? 'R' by default. 
#'  The other option is `WildBootTests.jl`. Running the bootstrap through 
#'  `WildBootTests.jl` might significantly reduce the runtime of `rwolf()` 
#'  for complex problems (e.g. problems with more than 500 clusters).
#' @param nthreads Integer. The number of threads to use when running the 
#' bootstrap.
#' @param ... additional function values passed to the bootstrap function. 
#' 
#' @importFrom fwildclusterboot boottest
#' @importFrom fixest coeftable
#' @importFrom dreamerr check_arg
#' @importFrom stats terms formula
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
#' 
#' @return 
#' 
#' A data.frame containing the following columns: 
#' \item{model}{Index of Models}
#' \item{Estimate}{The estimated coefficient of `param` in the respective model.}
#' \item{Std. Error}{The estimated standard error of `param` in the respective model.}
#' \item{t value}{The t statistic of `param` in the respective model.}
#' \item{Pr(>|t|)}{The uncorrected pvalue for `param` in the respective model.}
#' \item{RW Pr(>|t|)}{The Romano-Wolf corrected pvalue of hypothesis test for `param` in the respective model.}
#' 
#' @section Setting Seeds and Random Number Generation:
#' 
#' To guarantee reproducibility, please set a global random seeds via
#' `set.seed()`.
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
#' res <- feols(c(Y1, Y2, Y3) ~ X1, data = data, cluster = ~ cluster)
#' res_rwolf <- rwolf(models = res, param = "X1", B = B)
#' res_rwolf
#' 
#' @references 
#' Clarke, Romano & Wolf (2019), STATA Journal. 
#' IZA working paper: https://ftp.iza.org/dp12845.pdf
#' 


rwolf <- function(
    models,
    param,
    B,
    R = NULL,
    r = 0,
    p_val_type = "two-tailed",
    weights_type = "rademacher",
    engine = "R",
    nthreads = 1,
    bootstrap_type = "fnw11",
  ...){
  

  check_arg(param, "character vector | character scalar | formula")
  check_arg(R, "NULL | numeric vector")
  check_arg(r, "NULL | numeric scalar")
  check_arg(p_val_type, "charin(two_sided, >, <)")
  check_arg(weights_type, "charin(rademacher, mammen, webb, norm)")
  check_arg(bootstrap_type, "charin(11, 12, 13, 31, 33, fnw11)")
  check_arg(B, "integer scalar GT{99}")
  check_arg(engine, "charin(R, R-lean, WildBootTests.jl)")
  check_arg(nthreads, "scalar integer")


  if (inherits(param, "formula")) {
    param <- attr(terms(param), "term.labels")
  }
  
  # Check if 'models' is of type fixest_multi
  if(!inherits(models, "fixest_multi")){
  } else if(inherits(models, "list")){
    fixest_list <- mean(sapply(models, class) == "fixest") == 1L
    if(!fixest_list){
      stop("The object models needs to be either of type 
           'fixest_multi' or a list of objects of type 'fixest'.")
    }
  }


  # brute force objects of type 'fixest_multi' to list
  models <- as.list(models)
  S <- length(models)
  call <- models[[1]]$call

  # define a function to get statistics from fixest_multi object
  get_stats_fixest <- function(x, stat){
    res <- fixest::coeftable(
      models[[x]])[
        which(rownames(fixest::coeftable(models[[x]])) == param), stat
        ]
    res
  }
    
  # and get coefs, t-stats and ses 
  # no absolute values for coefs, ses 
  coefs <- unlist(
    lapply(1:S, function(x) get_stats_fixest(x, stat = "Estimate")))
  ses <- unlist(
    lapply(1:S, function(x) get_stats_fixest(x, stat = "Std. Error")))
  # absolute value for t-stats
  # t_stats <- abs(
  #   unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "t value"))))
  
  # repeat line: for multiway clustering, it is not clear how many bootstrap 
  # test statistics will be invalied - for oneway, 
  # all vectors of length(boot_coefs) \leq B
  
  boot_coefs <- boot_ses <- matrix(NA, B, S) 
  t_stats <- rep(NA, S)
  boot_t_stats <- list()
  
  # boottest() over all models for param
  pb <- txtProgressBar(min = 0, max = S, style = 3)
  
  # reset global seed state once exciting the function
  global_seed <- .Random.seed
  on.exit(set.seed(global_seed))
  
  internal_seed <- sample.int(.Machine$integer.max, 1L)
  
  res <- 
    lapply(seq_along(models), 
           function(x){
             
             # set seed, to guarantee that all S calls to 
             # boottest() generate the same weight matrices
             # affects global seed outside of 'rwolf()'!
             
             set.seed(internal_seed)
             clustid <- models[[x]]$call$cluster
             
             boottest_quote <-
               quote(
                 boottest(
                   models[[x]],
                   param = param,
                   B = B,
                   R = R,
                   r = r,
                   engine = engine,
                   p_val_type = p_val_type,
                   type = weights_type, 
                   sampling = "standard"
                 )
               )
             
             if(!is.null(clustid)){
               if(is.character(clustid)){
                 boottest_quote$clustid <- formula(paste0("~", clustid))
               } else {
                 boottest_quote$clustid <- formula(clustid)
               }
             }
             
             if(!is.null(bootstrap_type)){
               boottest_quote$bootstrap_type <- bootstrap_type
             }
             
             suppressMessages(
              boottest_eval <- 
                  eval(boottest_quote)
             )
             
             setTxtProgressBar(pb, x)
           
             boottest_eval
             
           })
  
  for(x in seq_along(models)){
    # take absolute values of bootstrap t statistics
    t_stats[x] <- (res[[x]]$t_stat)
    boot_t_stats[[x]] <- (res[[x]]$t_boot)     
  }
  
  boot_t_stats <- Reduce(cbind, boot_t_stats)
  
  # after calculating all bootstrap t statistics, initiate the RW procedure

  # stepwise p-value calculation
  pval <- get_rwolf_pval(t_stats = t_stats, 
                          boot_t_stats= boot_t_stats)
  
  # summarize all results
  models_info <-
    lapply(1:S, function(x){
      
      tmp <- coeftable(models[[x]])
      tmp1 <- tmp[which(rownames(tmp) == param),]
      
      suppressWarnings(tmp1$depvar <- as.character(
        as.expression(models[[x]]$fml[[2]])
        )
      )
      
      suppressWarnings(tmp1$covars <- as.character(
        as.expression(models[[x]]$fml[[3]])
        )
      )
      
      tmp1$model <- x
      tmp1
    })
  
  models_info <- Reduce(rbind, models_info)
  
  # some reordering
  models_info <- models_info[, c(7, 1:4)]
  models_info <- as.data.frame(models_info)
  rownames(models_info) <- NULL
  models_info[, "RW Pr(>|t|)"] <- pval

  models_info
  
}
