#' Function to calcaulte the carbon costs for photosynthesis
#'
#' @param ns_star Relative viscotity of water [-]
#' @param gs Stomatal conductance of CO2 [µmol/m2/s]
#' @param vpd_leaf Vapor pressure deficit at the leaf-level [Pa]
#' @param beta Unit cost ratio of acquiring nitrogen over water [-]
#' @param c_cost Marginal cost of maintaining Jmax [-]
#' @param vcmax Maximum rate of carboxylation [µmol/m2/s]
#' @param jmax Maximum rate of electron transport [µmol/m2/s]
#' @param a_gross Gross assimilation rate [µmol/m2/s]
#' @param cost_type Cost type that should be calculated
#'
#' @return List with cost for each each process and entire photosynthesis
#' @export
#'
#' @examples
get_carbon_costs <- function(
    ns_star,
    gs,
    vpd_leaf,
    beta,
    vcmax,
    c_cost,
    jmax,
    a_gross,
    cost_type) {
  
  # Check input
  cost_options <- c("relative_carbon_costs")
  
  if (!(cost_type %in% cost_options)) {
    stop("get_carbon_costs: Requested cost type not implemented.")
    }
  
  cost_transp <- 1.6 * ns_star * gs * vpd_leaf
  cost_vcmax  <- beta * vcmax
  cost_jmax   <- c_cost * jmax
  
  if (cost_type == "relative_carbon_costs") {
    # With Jmax
    carbon_costs <- (cost_transp + cost_vcmax + cost_jmax) / a_gross
    
    # Without Jmax
    # carbon_costs <- (cost_transp + cost_vcmax) / a_gross
  }
  
  out <- list(
    cost_transp  = cost_transp,
    cost_vcmax   = cost_vcmax,
    cost_jmax    = cost_jmax,
    carbon_costs = carbon_costs
  )
}