get_rwolf_pval <- function(t_stats, boot_t_stats){
  
  #' compute Romano-Wolf adjusted p-values based 
  #' on bootstrapped t-statistics
  #' 
  #' @param t_stats A vector of length S - where S is the number of 
  #'                tested hypotheses - containing the original, 
  #'                non-bootstrappe t-statisics
  #' @param boot_t_stats A (B x S) matrix containing the 
  #'        bootstrapped t-statistics 
  #'        
  #' @return A vector of Romano-Wolf corrected p-values        
  
  # note: this part very closely follows the p_adjust function from the hdm
  # package, written and maintained by Martin Spindler
  # code at https://github.com/cran/hdm/blob/master/R/p_adjust.R
  
  S <- ncol(boot_t_stats)
  B <- nrow(boot_t_stats)
  
  pinit <- corr.padj <- pval <- vector(mode = "numeric", length = S)
  stepdown.index <- order(t_stats, decreasing = TRUE)
  ro <- order(stepdown.index)
  
  for(s in 1:S){
    if(s == 1){
      max_stat <- apply(boot_t_stats, 1, max)
      pinit[s] <- pmin(1, 
                       (sum(
                         max_stat >= abs(
                           t_stats[stepdown.index[s]])) + 1) / (B + 1) 
      )
    }
    if(s > 1){
      boot_t_stat_udp <- boot_t_stats[,
                                      -stepdown.index[1:(s-1)], drop = FALSE
                                      ]   
      # drop max statistic
      max_stat <- apply(boot_t_stat_udp, 1, max)    
      # for each B, calculate max S
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
  
  pval
  
}
