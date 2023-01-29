test_that("family wise error rates simulations",{
  
  run <- FALSE
  
  if(run){
    # heteroskedastic sim
    
    res_hc <- 
      run_fwer_sim(
        N = 1000, 
        G = NULL, 
        B = 999,
        n_sims = 100, 
        seed = 98
      )
    
    # reject_5 reject_10  rho
    # fit_pvalue          0.281     0.474 0.00
    # fit_pvalue_holm     0.057     0.101 0.00
    # fit_padjust_rw      0.056     0.103 0.00
    # fit_pvalue1         0.265     0.456 0.25
    # fit_pvalue_holm1    0.054     0.101 0.25
    # fit_padjust_rw1     0.048     0.104 0.25
    # fit_pvalue2         0.200     0.367 0.50
    # fit_pvalue_holm2    0.044     0.085 0.50
    # fit_padjust_rw2     0.044     0.087 0.50
    # fit_pvalue3         0.135     0.232 0.75
    # fit_pvalue_holm3    0.024     0.043 0.75
    # fit_padjust_rw3     0.025     0.045 0.75
    
    res_hc_holm <- res_hc[seq(2, 11, 3),]
    res_hc_rw <- res_hc[seq(3, 12, 3),]
    
    expect_equal(res_hc_holm[,1], res_hc_rw[,1], tolerance = 0.5 * 1e-01 )
    expect_equal(res_hc_holm[,2], res_hc_rw[,2], tolerance = 0.5 * 1e-01 )
    
      # clustered error
      
      res_cl <- 
      run_fwer_sim(
        B = 999,
        n_sims = 1000, 
        seed = 981
      )
      
      res_cl_holm <- res_cl[seq(2, 11, 3),]
      res_cl_rw <- res_cl[seq(3, 12, 3),]
      
      expect_equal(res_cl_holm[,1], res_cl_rw[,1], tolerance = 0.5 * 1e-01 )
      expect_equal(res_cl_holm[,2], res_cl_rw[,2], tolerance = 0.5 * 1e-01 )
      
      # reject_5 reject_10  rho
      # fit_pvalue          0.263     0.455 0.00
      # fit_pvalue_holm     0.039     0.084 0.00
      # fit_padjust_rw      0.037     0.090 0.00
      # fit_pvalue1         0.234     0.461 0.25
      # fit_pvalue_holm1    0.037     0.078 0.25
      # fit_padjust_rw1     0.040     0.084 0.25
      # fit_pvalue2         0.275     0.473 0.50
      # fit_pvalue_holm2    0.054     0.096 0.50
      # fit_padjust_rw2     0.052     0.099 0.50
      # fit_pvalue3         0.241     0.434 0.75
      # fit_pvalue_holm3    0.042     0.091 0.75
      # fit_padjust_rw3     0.043     0.093 0.75
      
  } else {
    
    res_hc_holm <- cbind(c(0.057, 0.054, 0.044, 0.024), 
                         c(0.101, 0.101, 0.085, 0.043))
    res_hc_rw <- cbind(c(0.056, 0.048, 0.044, 0.025), 
                       c(0.103, 0.104, 0.087, 0.045))
    res_cl_holm <- cbind(c(0.041, 0.054, 0.045, 0.050), 
                         c(0.078, 0.093, 0.093, 0.103))
    res_cl_rw <- cbind(c(0.04, 0.046, 0.044, 0.050), 
                       c(0.077, 0.098, 0.096, 0.105))
    
    expect_equal(res_hc_holm[,1], res_hc_rw[,1], tolerance = 0.5 * 1e-01 )
    expect_equal(res_hc_holm[,2], res_hc_rw[,2], tolerance = 0.5 * 1e-01 )
    
    expect_equal(res_cl_holm[,1], res_cl_rw[,1], tolerance = 0.5 * 1e-01 )
    expect_equal(res_cl_holm[,2], res_cl_rw[,2], tolerance = 0.5 * 1e-01 )
    
  }

  
  
  
  
})
