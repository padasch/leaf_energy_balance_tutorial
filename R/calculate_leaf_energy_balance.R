#' Function to calculate the energy balance of a leaf
#'
#' @description This function calculates the energy balance of the leaf, given biotic and abiotic drivers. Output options are the difference between input-ed and calculated leaf temperature ("balance") or all calculated energy fluxes. Note: This functions has been taken from the {plantecophys} package and modified for new purposes here.
#' 
#' @details This leaf energy balance model was adapted from Duursma, Remko A. 2015. “Plantecophys - An R Package for Analysing and Modelling Leaf Gas Exchange Data.” Edited by Paul C. Struik. <i>PLOS ONE</i> 10 (11): e0143346. https://doi.org/10/bkmj. It is based on the Penman-Monteith equation in appendix of Leuning, R., F. M. Kelliher, D. G. G. Pury, and E.-D. Schulze. 1995. “Leaf Nitrogen, Photosynthesis, Conductance and Transpiration: Scaling from Leaves to Canopies.” <i>Plant, Cell and Environment</i> 18 (10): 1183–1200. https://doi.org/10.1111/j.1365-3040.1995.tb00628.x.
#'
#' @param tc_leaf Assumed leaf Temperature [ºC]
#' @param tc_air  Air Temperature [ºC]
#' @param gs      Stomatal conductance of CO2 [mol/m2/s]
#' @param ppfd    Photosynthetically Active Photon Flux Density [mol/m2/s]
#' @param vpd_air Vapor pressure deficit of the air [Pa]
#' @param patm    Atmospheric pressure [Pa]
#' @param wind    Wind speed [m/s]
#' @param leaf_size     Characteristic leaf width [m]
#' @param stomata_ratio Stomatal ratio: 1 = Hypostomataous (stomata on one leaf side), 2 = Amphistomataous (stomata on both leaf sides) [-]
#' @param leaf_abs      Leaf absorptance of solar radiation (range [0,1]) [-]
#' @param return_what   Output to be returned ("balance" for squared difference between input and calculated leaf temperature, "fluxes" for all energy fluxes calculated by the energy balance)
#'
calculate_leaf_energy_balance <- function(
  tc_leaf       = 21.5, 
  tc_air        = 20,
  gs            = 1.5e-6,
  ppfd          = 1500e-6, 
  vpd_air       = 2000, 
  patm          = 101325,
  wind          = 2, 
  leaf_size     = 0.02,
  stomata_ratio = 1,
  leaf_abs      = 0.5, 
  return_what   = c("balance","fluxes")
  ){

  # Define arguments
  return_what <- match.arg(return_what)
  
  # Important!: Function uses different values than used in rpmodel
  # gs from rpmodel is in mol CO2 / m2 / s / Pa
  # gs here is in mol H2O / m2 / s
  # Turning stomatal conductance of CO2 into conductance of H2O
  gs   <- 1.6 * gs * patm
  ppfd <- ppfd * 10^6 
  
  # Define constants
  Boltz      <- 5.67 * 10^-8 # w M-2 K-4
  Emissivity <- 0.95         # -
  LatEvap    <- 2.54         # MJ kg-1
  CPAIR      <- 1010.0       # J kg-1 K-1
  
  H2OLV0     <- 2.501e6      # J kg-1
  H2OMW      <- 18e-3        # J kg-1
  AIRMA      <- 29.e-3       # mol mass air (kg/mol)
  AIRDENS    <- 1.204        # kg m-3
  UMOLPERJ   <- 4.57         # Micromole photons per Joule [-]
  DHEAT      <- 21.5e-6      # molecular diffusivity for heat
  
  # Density of dry air
  AIRDENS <- patm / (287.058 * celsius_to_kelvin(tc_air))

  # Latent heat of water vapour at air temperature (J mol-1)
  LHV <- (H2OLV0 - 2.365E3 * tc_air) * H2OMW
  
  # Const s in Penman-Monteith equation  (Pa K-1)
  SLOPE <- (esat(tc_air + 0.1) - esat(tc_air)) / 0.1
  
  # Radiation conductance (mol m-2 s-1)
  Gradiation <- 4. * Boltz * celsius_to_kelvin(tc_air)^3 * Emissivity / (CPAIR * AIRMA)
  
  # See Leuning et al (1995) PC&E 18:1183-1200 Appendix E
  # Boundary layer conductance for heat - single sided, forced convection
  CMOLAR <- patm / (8.314 * celsius_to_kelvin(tc_air)) # .Rgas() in package...
  Gbhforced <- 0.003 * sqrt(wind / leaf_size) * CMOLAR
  
  # Free convection
  GRASHOF <- 1.6E8 * abs(tc_leaf - tc_air) * (leaf_size^3) # Grashof number
  Gbhfree <- 0.5 * DHEAT * (GRASHOF^0.25) / leaf_size * CMOLAR
  
  # Total conductance to heat (both leaf sides)
  Gbh <- 2 * (Gbhfree + Gbhforced)
  
  # Heat and radiative conductance
  Gbhr <- Gbh + 2 * Gradiation
  
  # Boundary layer conductance for water (mol m-2 s-1)
  Gbw <- stomata_ratio * 1.075 * Gbh # Leuning 1995
  gw <- gs * Gbw / (gs + Gbw)
  
  # Longwave radiation
  # (positive flux is heat loss from leaf)
  Rlongup <- Emissivity * Boltz * celsius_to_kelvin(tc_leaf)^4
  
  # Rnet
  Rsol <- 2 * ppfd / UMOLPERJ # W m-2
  Rnet <- leaf_abs * Rsol - Rlongup # full
  
  # Isothermal net radiation (Leuning et al. 1995, Appendix)
  ea <- esat(tc_air, patm) - vpd_air
  ema <- 0.642 * (ea / celsius_to_kelvin(tc_air))^(1 / 7)
  
  # Safety Check
  if (is.na(ema)){
    stop("calculate_leaf_energy_balance: `ema` is NA, likely due to unrealistic combination of given vpd and air temperature (too high vpd for that air temperature).")
  }
  
  Rnetiso <- leaf_abs * Rsol - (1 - ema) * Boltz * celsius_to_kelvin(tc_air)^4
  
  # Isothermal version of the Penmon-Monteith equation
  GAMMA <- CPAIR * AIRMA * patm / LHV
  ET <- (1 / LHV) * (SLOPE * Rnetiso + vpd_air * Gbh * CPAIR * AIRMA) / (SLOPE + GAMMA * Gbhr / gw)
  
  # Latent heat loss
  lambdaET <- LHV * ET
  
  # Heat flux calculated using Gradiation (Leuning 1995, Eq. 11)
  Y <- 1 / (1 + Gradiation / Gbh)
  H2 <- Y * (Rnetiso - lambdaET)
  
  # Heat flux calculated from leaf-air T difference.
  # (positive flux is heat loss from leaf)
  H <- -CPAIR * AIRDENS * (Gbh / CMOLAR) * (tc_air - tc_leaf)
  
  # Leaf-air temperature difference recalculated from energy balance.
  # (same equation as above!)
  tc_leaf2 <- tc_air + H2 / (CPAIR * AIRDENS * (Gbh / CMOLAR))
  
  # Difference between input tc_leaf and calculated, this will be minimized.
  EnergyBal <- (tc_leaf - tc_leaf2)           # OLD, needed to work with uniroot()
  # EnergyBal <- (tc_leaf - tc_leaf2)^2           # NEW, needed to work with optimr()
  # EnergyBal <- abs(tc_leaf - tc_leaf2)        # NEW, needs more iterations than ()^2
  
  if (return_what == "balance") {
    return(EnergyBal) # OLD
  
    # out <- list(tc_leaf       = tc_leaf,     # NEW
    # 						tc_leaf_star  = tc_leaf2,    # NEW
    # 						eps           = EnergyBal) # NEW
    # return(out)                            # NEW
  }
  
  if (return_what == "fluxes") {
    l <- data.frame(ELEAFeb = 1000 * ET, Gradiation = Gradiation, Rsol = Rsol, Rnetiso = Rnetiso, Rlongup = Rlongup, H = H, lambdaET = lambdaET, gw = gw, Gbh = Gbh, H2 = H2, tc_leaf2 = tc_leaf2)
    return(l)
  }
}
