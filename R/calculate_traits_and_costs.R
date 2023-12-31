#' Function to be optimized based on carbon costs for photosynthesis
#'
#' @details To achieve good numerical performance, the input values must be in per-day and not in per-second.
#'
#' @param par Input parameters in order: vcmax, jmax, gs [µmol/m^2/d]
#' @param tc_leaf Leaf temperature [ºC]
#' @param vpd_leaf Leaf vapor pressure deficit [Pa]
#' @param ppfd Photosynthetically Active Photon Flux Density [µmol/m^2/s]
#' @param fapar Fraction absorbed photosynthetically active radiation, set to 1 [-]
#' @param co2 Atmospheric partial pressure of CO2 [Pa]
#' @param patm Atmospheric pressure [Pa]
#' @param kphio Calibrated quantum yield efficiency [-]
#' @param beta Unit cost ratio for acquiring nitrogen over water [-]
#' @param maximize Whether carbon cost metric should be maximized or not [TRUE/FALSE]
#' @param return_all What to return. FALSE = carbon costs only. TRUE = all calculated variables
#' @param units_out_per_second Whether output units should be in per-day or per-second [TRUE/FALSE]
#' @param include_energy_balance Whether energy balance model should be coupled [TRUE/FALSE]
#' @param ... Additional arguments to be piped 
#'
calculate_traits_and_costs <- function(
    par,
    tc_air,
    vpd_air,
    ppfd,
    fapar = 1,
    co2,
    patm,
    kphio,
    beta       = 146.0,
    maximize   = FALSE,
    return_all = TRUE,
    units_out_per_second  = TRUE,
    include_energy_balance = FALSE,
    ...) {
  
  
  ## 1: Parameters to be optimized:
  vcmax <- par[1]
  jmax  <- par[2]
  gs    <- par[3]
  
  ## x: Given gs, calculate the leaf temperature
  if (include_energy_balance == TRUE) {
    tc_leaf <- 
      optimize_leaf_energy_balance(
        tc_air  = tc_air,
        vpd_air = vpd_air,
        gs      = gs   / 3600 / 24, # Adjust input to per-second
        ppfd    = ppfd / 3600 / 24, # Adjust input to per-second
        patm    = patm,
        ...
      )
    
    vpd_leaf <- air_vpd_to_leaf_vpd(vpd_air, tc_air, tc_leaf)
    
  } else {
    tc_leaf  <- tc_air
    vpd_leaf <- vpd_air
  }
  
  ## 2: Get photosynthetic variables based on environmental conditions:
  kmm       <- rpmodel::kmm(tc_leaf, patm)
  gammastar <- rpmodel::gammastar(tc_leaf, patm)
  ns_star   <- rpmodel::viscosity_h2o(tc_leaf, patm) / rpmodel::viscosity_h2o(25, 101325)
  ca        <- rpmodel::co2_to_ca(co2, patm)
  kphio     <- kphio * rpmodel::ftemp_kphio( tc_leaf, c4 = F)
  iabs      <- ppfd * fapar
  
  ## 3: Calculate assimilation rates with to-be-optimized jmax, vcmax and gs:
  
  ## 3.1: Electron transport is limiting
  ## Solve quadratic equation system using: A(Fick's Law) = A(Jmax Limitation)
  ## This leads to a quadratic equation:
  ## A * ci^2 + B * ci + C  = 0
  ## 0 = a + b*x + c*x^2
  
  ## Jmax Limitation following Smith (1937):
  ## A = gs * (ca - ci)
  ## A = kphio * iabs (ci-gammastar)/ci+2*gammastar) * L
  ## L = 1 / sqrt(1 + ((4 * kphio * iabs)/jmax)^2)
  
  ## with
  L <- 1.0 / sqrt(1.0 + ((4.0 * kphio * iabs)/jmax)^2)
  A <- -gs
  B <- gs * ca - 2 * gammastar * gs - L * kphio * iabs
  C <- 2 * gammastar * gs * ca + L * kphio * iabs * gammastar
  
  ci_j <- QUADM(A, B, C)
  a_j  <- kphio * iabs * (ci_j - gammastar)/(ci_j + 2 * gammastar) * L  
  
  c_cost <- 0.103 # As estimated by Wang et al. (2017)
  
  # ............................................................................
  # ## Jmax Limitation following Farquhar (1989):
  #   ## A = gs * (ca - ci)
  #   ## A = j/4 * (ci-gammastar)/ci+2*gammastar)
  #   ## j = (kphio * iabs + jmax - sqrt(( kphio * iabs + jmax)^2 - (4 * kphio * theta * iabs * jmax))) / (2*theta)
  #   
  #   ## with
  #   theta <- 0.85
  #   j <- (kphio * iabs + jmax - sqrt(( kphio * iabs + jmax)^2 - (4 * kphio * theta * iabs * jmax))) / (2 * theta)
  #   A <- -gs
  #   B <- gs * ca - 2 * gammastar * gs - j/4
  #   C <- 2 * gammastar * gs * ca + gammastar * j/4
  #   
  #   ci_j <- ci_j <- QUADM(A, B, C)
  #   a_j <- j/4 * (ci_j - gammastar)/(ci_j + 2 * gammastar)
  #   
  #   c_cost <- 0.053 # As estimated by Smith et al. (2019)
  # ............................................................................
  
  ## 4: Rubisco is limiting
  ## Solve Eq. system
  ## A = gs (ca- ci)
  ## A = Vcmax * (ci - gammastar)/(ci + Kmm)
  
  ## This leads to a quadratic equation:
  ## A * ci^2 + B * ci + C  = 0
  ## 0 = a + b*x + c*x^2
  
  ## with
  A <- -1.0 * gs
  B <- gs * ca - gs * kmm - vcmax
  C <- gs * ca * kmm + vcmax * gammastar
  
  ci_c <- QUADM(A, B, C)
  a_c  <- vcmax * (ci_c - gammastar) / (ci_c + kmm)
  
  ## 5. Take minimum of the two assimilation rates and maximum of the two ci
  ci      <- max(ci_c, ci_j)
  a_gross <- min( a_j, a_c ) # Original approach using min()
  
  # Alternative approach using hyperbolic minumum to avoid discontinuity (see Duursma et al (2015), Eq. (5))
  # a_gross <- -QUADP(A = 1 - 1E-07, B = a_c + a_j, C = a_c*a_j)
  
  ## 6. Get carbon costs
  carbon_costs <- 
    get_carbon_costs(
      vpd_leaf  = vpd_leaf,
      ns_star   = ns_star,
      gs        = gs,
      vcmax     = vcmax,
      jmax      = jmax,
      beta      = beta,
      c_cost    = c_cost,
      a_gross   = a_gross,
      cost_type = "relative_carbon_costs"
    )
  
  ## 7. Get additional variables
  chi  <- ci / ca
  iwue <- ca * (1 - chi) / 1.6
  
  # if (maximize) net_assim <- -carbon_costs
  
  if (return_all) {
    
    ## Turn per-day units back into per-second
    if (units_out_per_second == TRUE) {
      vcmax   <- vcmax   / (3600 * 24) # Final unit: [mol/m2/s]
      jmax    <- jmax    / (3600 * 24) # Final unit: [mol/m2/s]
      gs      <- gs      / (3600 * 24) # Final unit: [mol/m2/s/Pa]
      a_c     <- a_c     / (3600 * 24) # Final unit: [mol/m2/s]
      a_j     <- a_j     / (3600 * 24) # Final unit: [mol/m2/s]
      a_gross <- a_gross / (3600 * 24) # Final unit: [mol/m2/s]
      # carbon_costs <- carbon_costs / (3600 * 24) # Final unit: [-]
    }
    
    ## Output
    return(
      tibble(
        vcmax = vcmax,
        jmax = jmax,
        gs = gs,
        ci = ci,
        chi = chi,
        a_c = a_c,
        a_j = a_j,
        a_gross = a_gross,
        ci_c = ci_c,
        ci_j = ci_j,
        iwue = iwue,
        kmm = kmm,
        gammastar = gammastar,
        ns_star = ns_star,
        cost_transp = carbon_costs$cost_transp,
        cost_vcmax = carbon_costs$cost_vcmax,
        cost_jmax = carbon_costs$cost_jmax,
        carbon_costs = carbon_costs$carbon_costs,
        include_energy_balance = include_energy_balance,
        tc_air  = tc_air,
        tc_leaf = tc_leaf
      )
    )
  } else {
    return( carbon_costs$carbon_costs )
  }
}
