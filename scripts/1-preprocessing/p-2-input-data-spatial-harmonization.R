### ---------------------\\ 
# Script objective:
# Spatially harmonize input data sets -- see sourced scripts for each layer in sub-folder /01-spatial-harmonization-addnl  
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

# water table ratio
source(here("scripts/1-preprocessing/01-spatial-harmonization-addnl/p1-wtr.R"))

# porosity
source(here("scripts/1-preprocessing/01-spatial-harmonization-addnl/p1-porosity.R"))

# GDE density
source(here("scripts/1-preprocessing/01-spatial-harmonization-addnl/p1-gde-types-desnsity.R"))

# dominant farm field size
source(here("scripts/1-preprocessing/01-spatial-harmonization-addnl/p1-dominant-field-size.R"))

# GMIA: area equipped for irrigation from groundwater
terra::writeRaster(x = terra::rast("D:/Geodatabase/Agriculture/GMIA/Raw/gmia_v5_aeigw_pct_aei.asc"),
                   filename = here("data/input/gmia_aeigw_pct_aei.tif"),
                   overwrite = TRUE)

# Government effectiveness data
source(here("scripts/1-preprocessing/01-spatial-harmonization-addnl/p1-wgi-ge.R"))

# unimproved drinking water
source(here("scripts/1-preprocessing/01-spatial-harmonization-addnl/p1-udw.R"))