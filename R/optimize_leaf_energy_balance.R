#' Optimization of the leaf energy balance temperature difference
#' 
#' @details This function minimizes the difference between input and energy-balance-closure leaf temperature calculated in calculation_leaf_energy_balance().
#'
#' @param tc_air Leaf temperature [ºC]
#' @param vpd_air Leaf vapor pressure deficit [Pa]
#' @param gs Stomatal conductance [µmol CO2 /m^2/s]
#' @param ppfd Photosynthetically Active Photon Flux Density [µmol/m^2/s]
#' @param patm Atmospheric pressure [Pa] 
#' @param ... Additional arguments to be piped 
#'
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
