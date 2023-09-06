# Name: p1-data-input-spatial-harmonization.R
#   description: spatially harmonize input data 

# baseflow index
terra::resample(x = terra::rast("D:/Geodatabase/Streamflow/GSCD/BFI1_raw.tif"),
                y = terra::rast(here("data/ggrid_5arcmin.tif")),
                method = "bilinear",
                threads = TRUE,
                filename = here("data/input/bfi_5arcmin.tif"), overwrite = TRUE)

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

# IWRM data
source(here("scripts/1-preprocessing/01-spatial-harmonization-addnl/p1-iwrm-data.R"))


# unimproved drinking water
source(here("scripts/1-preprocessing/01-spatial-harmonization-addnl/p1-udw.R"))