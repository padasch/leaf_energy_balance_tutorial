optimize_leaf_energy_balance <- function(
    tc_air,
    vpd_air,
    gs,
    ppfd,
    patm,
    ...) { # ... for any additional leaf energy balance parameter like wind
      
    ## IN DEV:
    sol_optimize <- uniroot(
      f             = calculate_leaf_energy_balance,
      interval      = c(max(tc_air - 15, 1), tc_air + 15),
      # interval      = c(0, 50),
      tc_air        = tc_air,
      vpd_air       = vpd_air,
      gs            = gs,
      ppfd          = ppfd,
      patm          = patm,
      return_what   = "balance",
      ...
    )
    
    tc_leaf <- sol_optimize$root
    
    return(tc_leaf)
    
    ## TODO: OPTIM() AND OPTIMR() BELOW CRASH FOR SOME REASON...
    
    # out_optim <- optimr::optimr(
    #     
    #     ## Optimization inputs:
    #     par        = tc_air,
    #     lower      = 1,
    #     upper      = 40,
    #     fn         = diff_tcleaf_in_and_tcleaf_eb,
    #     method     = "L-BFGS-B",
    #     control    = list(maxit = 1000),
    #     
    #     ## Function inputs:
    #     tc_air = tc_air,
    #     ppfd = ppfd,
    #     patm = patm,
    #     co2 = co2,
    #     vpd = vpd,
    #     kphio = kphio,
    #     method_jmaxlim_inst = method_jmaxlim_inst,
    #     method_eb = method_eb)
    #     
    # out_optim <- optim(
    # 
    #     ## Optimization inputs:
    #     par        = tc_air,
    #     lower      = 1,
    #     upper      = 40,
    #     fn         = diff_tcleaf_in_and_tcleaf_eb,
    #     method     = "L-BFGS-B",
    #     control    = list(maxit = 1000),
    # 
    #     ## Function inputs:
    #     tc_air = tc_air,
    #     ppfd = ppfd,
    #     patm = patm,
    #     co2 = co2,
    #     vpd = vpd,
    #     kphio = kphio,
    #     method_jmaxlim_inst = method_jmaxlim_inst,
    #     method_eb = method_eb)
    }