get_optimal_tc_leaf <- function(
    tc_air,
    ppfd,
    patm,
    co2,
    vpd,
    kphio) {
      
    ## IN DEV:
    sol_optimize <- optimize(
      get_diff_input_tcleaf_and_eb_tcleaf,
      interval  = c(max(tc_air - 15, 1), tc_air + 15),
      tc_air    = tc_air,
      vpd_air   = vpd_air,
      ppfd      = ppfd,
      co2       = co2,
      patm      = patm,
      kphio		  = kphio
      )
    
    return(sol_optimize$minimum)
    
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