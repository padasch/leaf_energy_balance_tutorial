# Function to turn temperature from degree celsius to kelvin
celsius_to_kelvin <- function(x) {
  x + 273.15
}

# Function to calculate saturation water pressure in [Pa]
esat <- function(
    tc_air, 
    patm = 101325) {
  ## Pa in kPa
  a <- 611.21
  b <- 17.502
  c <- 240.97
  f <- 1.0007 + 3.46 * 10^-8 * patm
  esatval <- f * a * (exp(b * tc_air / (c + tc_air)))
  return(esatval)
}

# Get leaf vpd from air vpd
air_vpd_to_leaf_vpd <- function(
    vpd_air,
    tc_air, 
    tc_leaf, 
    patm = 101325){

  #' @param vpd_air Vapor pressure deficit of air at given air temperature [Pa]
  #' @param tc_air Air temperature [ºC]
  #' @param tc_leaf Leaf temperature [ºC]
    
  # This function was adopted from the R package 'plantecophys'  
  # Duursma (2015) https://doi.org/10/bkmj.
  
  e   <- esat(tc_air, patm) - vpd_air
  vpd <- esat(tc_leaf, patm) - e
  
  # if (min(vpd, 0) == 0) warning("Calculated VPD is below 0!")
  
  return(vpd)
}

# Function to solve quadratic equations
QUADM <- function(A, B, C) {
  if (any(is.na(c(A, B, C)))) {
    return(NA)
  } else {
    if ((B^2 - 4 * A * C) < 0) {
      warning("IMAGINARY ROOTS IN QUADRATIC")
      return(0)
    }

    if (identical(A, 0)) {
      if (identical(B, 0)) {
        return(0)
      } else {
        return(-C / B)
      }
    } else {
      return((-B - sqrt(B^2 - 4 * A * C)) / (2 * A))
    }
  }
}