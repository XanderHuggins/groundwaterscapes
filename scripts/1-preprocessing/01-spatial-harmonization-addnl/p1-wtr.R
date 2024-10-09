# import Doell 2008 recharge data
terra::project(x = terra::rast("D:/Geodatabase/Groundwater/Doell_Recharge2008/r1_doll2008.tif"),
               y = terra::rast(here("data/ggrid_5arcmin.tif")),
               method = "near", 
               gdal = TRUE, threads = TRUE,  filename = here("data/input/gw_recharge_5arcmin.tif"), overwrite = TRUE)

# create reclassification based on threshold used in Cuthbert et al.
rcl.m = c(0, 5, 1,
          5, Inf, 0) |> 
  matrix(ncol = 3, byrow = TRUE)

terra::classify(x = terra::rast(here("data/input/gw_recharge_5arcmin.tif")),
                rcl = rcl.m,
                include.lowest = TRUE,
                filename = here("data/input/gw_recharge_5mm_mask.tif"), overwrite = TRUE)

# reproject
terra::project(x = terra::rast("D:/Geodatabase/Groundwater/GRT_WTR/LOG_WTR_L_01.tif"),
               y = terra::rast(here("data/ggrid_5arcmin.tif")),
               method = "bilinear", 
               gdal = TRUE, threads = TRUE,  filename = here("data/input/wtr_5arcmin.tif"), overwrite = TRUE)

# apply recharge mask
terra::mask(x = terra::rast(here("data/input/wtr_5arcmin.tif")),
            mask = terra::rast(here("data/input/gw_recharge_5mm_mask.tif")),
            maskvalues = 1,
            updatevalue = -9,
            filename = here("data/input/wtr_5arcmin_aridmask.tif"))