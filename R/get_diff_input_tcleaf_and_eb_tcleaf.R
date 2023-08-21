#' Title
#'
#' @param tc_leaf Assumed leaf temperature to be optimized
#' @param tc_air 
#' @param ppfd 
#' @param co2 
#' @param patm 
#' @param vpd 
#' @param kphio 
#' @param method_jmaxlim_inst 
#' @param method_eb 
#' @param gs can be provided in mol CO2 / m2 / s / Pa to skip running the carbon cost optimization
#'
#' @return
#' @export
#'
#' @examples
get_diff_input_tcleaf_and_eb_tcleaf <- function(
    tc_leaf, # This parameter is optimized
    tc_air,
    vpd_air,
    ppfd,
    co2,
    patm,
    kphio,
    gs = NA,
    method_eb = "plantecophys") {
  
  ## Output: difference in tc_leaf assumed for gs and tc_leaf from energy balance
  
  ## 1: Get optimal gs, vcmax and jmax at given tc_leaf (if no gs is inputed)
  if (is.na(gs)) {
    vpd_leaf <- air_vpd_to_leaf_vpd(vpd_air, tc_air, tc_leaf, patm)
    
    optim_vars <- get_optimized_traits_and_costs(
      tc_leaf,
      vpd_leaf,
      patm,
      co2,
      ppfd,
      kphio
    )
    
    gs <- optim_vars$gs_mine
  } else {
    gs <- 1.5e-6
  }
  
  ## 2. Get optimal gs for calculating tc_leaf via energy balance:
  
  if (method_eb == "plantecophys") {
    ## 2.1: Via plantecophys energy balance
    
    leb_optimization <- optimr::optimr(
      
      # Parameter boundaries to optimize within:
      par       = 15,
      lower     = max(1, 15 - tc_air), # OLD: 0
      upper     = 15 + tc_air,         # OLD: 40
      
      # Function to optimize and its inputs:
      fn        = calculate_leaf_energy_balance,
      tc_air    = tc_air,
      vpd_air   = vpd_air,
      gs        = gs,
      patm      = patm,
      ppfd      = ppfd,
      
      # {optimr} settings:
      method    = "L-BFGS-B",
      control   = list(maxit = 10000, maximize = TRUE))$par
    
    tc_leaf_leb <- leb_optimization$par
    
  } else if (method_eb == "tealeaves") {
    stop("ERROR: This part has not yet been revised and is not working!")
    ## 2.2: Via tealeaves energy balance
    
    # Get relative humidity from vpd:
    RH <- (100 - ((vpd) / esat(tc_air, patm/1000))) / 100
    
    # Get leaf parameters:
    leaf_par <- make_leafpar(
      replace = list(
        g_sw = set_units(gs_water, "mol/m^2/s/Pa")))
    
    # Get environmental parameters:
    enviro_par <- make_enviropar(
      replace = list(
        T_air = set_units(tc_air + 273.15, "K"),
        RH    = as_units(RH),
        P     = set_units(patm, "Pa")))
    
    # Get physical constants:
    constants  <- make_constants()
    
    # Get tc_leaf:
    tc_leaf_leb <- tleaf(leaf_par, enviro_par, constants, quiet = TRUE)$T_leaf %>% 
      set_units("degree_Celsius") %>% drop_units()
  }
  
  # 3: Get squared difference between from assumed tc_leaf and actual tc_leaf
  eps <- (tc_leaf_leb - tc_leaf)^2
  
  return(eps)
}