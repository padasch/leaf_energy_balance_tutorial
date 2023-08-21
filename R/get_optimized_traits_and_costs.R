#' Function to calculate the optimal traits and respective carbon costs
#'
#' @param tc_leaf Leaf temperature [ºC]
#' @param vpd_leaf Leaf-level vapor-pressure deficit
#' @param patm Atmospheric pressure [Pa]
#' @param co2 Atmospheric CO2 concentration [ppm]
#' @param ppfd Phososynthetic photon flux density [µmol/m2/s]
#' @param kphio Parameter for QYE [-]
#' @param vcmax_start Starting values for vcmax in the optimization routine
#' @param jmax_start Starting values for jmax in the optimization routine
#' @param gs_start Starting values for gs in the optimization routine
#'
#' @return
#' @export
#'
#' @examples
get_optimized_traits_and_costs <- function(
  tc_leaf,
  vpd_leaf,
  patm,
  co2,
  ppfd,
  kphio,
  vcmax_start = NA,
  jmax_start  = NA,
  gs_start    = NA
) {
  
  ## Input for optimization has to be in per-day to work properly:
  ppfd        <- ppfd * 3600 * 24
  vcmax_start <- 20
  jmax_start  <- 20
  gs_start    <- 0.3
  
  ## Run optimization
  ## (TODO: Output order of magnitude depends on lower/upper boundaries)
  out_optim <- optimr::optimr(
    
    ## Optimization inputs:
    par        = c( vcmax_start,       jmax_start      , gs_start),
    lower      = c( vcmax_start*10^-6, jmax_start*10^-6, gs_start*10^-6 ),
    upper      = c( vcmax_start*10^6,  jmax_start*10^6 , gs_start*10^6  ),
    fn         = get_traits_and_carbon_costs,
    method     = "L-BFGS-B",
    control    = list(maxit = 1000),
    
    ## Function inputs:
    tc_leaf    = tc_leaf,
    vpd_leaf   = vpd_leaf,
    patm       = patm,
    co2        = co2,
    ppfd       = ppfd,
    kphio      = kphio,
    maximize   = TRUE,
    return_all = FALSE)
  
  ## Get the carbon costs for the optimized traits
  optimized_par <- get_traits_and_carbon_costs(
    par        = out_optim$par,
    tc_leaf    = tc_leaf,
    vpd_leaf   = vpd_leaf,
    patm       = patm,
    co2        = co2,
    ppfd       = ppfd,
    kphio      = kphio,
    units_out  = "per-s",
    return_all = T)
  
  ## Return optimized traits and carbon costs
  return(optimized_par)
}