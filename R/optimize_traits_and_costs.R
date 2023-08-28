#' Function to calculate the optimal traits and respective carbon costs
#'
#' @param tc_air Leaf temperature [ºC]
#' @param vpd_air Leaf-level vapor-pressure deficit
#' @param patm Atmospheric pressure [Pa]
#' @param co2 Atmospheric CO2 concentration [ppm]
#' @param ppfd Photosynthetically Active Photon Flux Density [µmol/m2/s]
#' @param kphio Parameter for QYE [-]
#' @param vcmax_start Starting values for vcmax in the optimization routine
#' @param jmax_start Starting values for jmax in the optimization routine
#' @param gs_start Starting values for gs in the optimization routine
#' @param ... Additional arguments to be piped 
#'
optimize_traits_and_costs <- function(
  tc_air,
  vpd_air,
  patm,
  co2,
  ppfd,
  kphio,
  include_energy_balance = FALSE,
  vcmax_start = NA,
  jmax_start  = NA,
  gs_start    = NA,
  ...
) {
  
  ## Input for optimization has to be in per-day to work properly:
  ppfd        <- ppfd * 3600 * 24  # / 3600 / 24
  vcmax_start <- 5                # / 3600 / 24
  jmax_start  <- 10                # / 3600 / 24 
  gs_start    <- 0.05               # / 3600 / 24 
  
  ## Run optimization
  ## (TODO: Output order of magnitude depends on lower/upper boundaries)
  out_optim <- optimr::optimr(
    
    ## Optimization inputs:
    par        = c( vcmax_start,      jmax_start     , gs_start),
    upper      = c( vcmax_start*100, jmax_start*100, gs_start*10 ),
    lower      = c( vcmax_start/100, jmax_start/100, gs_start/10 ),
    fn         = calculate_traits_and_costs,
    method     = "L-BFGS-B",
    control    = list(maxit = 1000),
    
    ## Function inputs:
    tc_air     = tc_air,
    vpd_air    = vpd_air,
    patm       = patm,
    co2        = co2,
    ppfd       = ppfd,
    kphio      = kphio,
    include_energy_balance = include_energy_balance,
    maximize   = TRUE,
    return_all = FALSE,
    ...)
  
  ## Get the carbon costs for the optimized traits
  optimized_par <- calculate_traits_and_costs(
    par        = out_optim$par,
    tc_air     = tc_air,
    vpd_air    = vpd_air,
    patm       = patm,
    co2        = co2,
    ppfd       = ppfd,
    kphio      = kphio,
    include_energy_balance = include_energy_balance,
    units_out_per_second  = TRUE,
    return_all = TRUE,
    ...)
  
  ## Return optimized traits and carbon costs
  return(optimized_par)
}
