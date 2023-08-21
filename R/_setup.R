# R functions
sapply(
  list.files(here::here("R"), 
             pattern = "^[^_].*", 
             full.names = TRUE), 
  source
)

# Packages
packages <- c(
  "here", "tidyverse", "ggplot2", "patchwork", "rpmodel"
)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages],
                   repos = "http://cran.us.r-project.org")
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
# conflict_prefer(name = "select", winner = "dplyr")
# conflict_prefer(name = "filter", winner = "dplyr")
