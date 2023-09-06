
# import GDE types at 1km data
gde_w = terra::rast("D:/projects/global-groundwatersheds/data/share/gde-map.tif")

# 001: Lotic
# 010: Lentic
# 011: Lentic and lotic
# 100: Terrestrial
# 101: Terrestrial and lotic
# 110: Terrestrial and lentic
# 111: Terrestrial, lentic, and lotic (all)

# reclassify for terrestrial
rasterDT::subsDT(x = raster(gde_w),
                 dict = data.frame(from = c(1,  10, 11, 100, 101, 110, 111),
                                   to   = c(NA, NA, NA,   1,   1,   1,   1)),
                 filename = here("data/temp/gde-terr.tif"))

# reclassify for lotic
rasterDT::subsDT(x = raster(gde_w),
                 dict = data.frame(from = c(1,  10, 11, 100, 101, 110, 111),
                                   to   = c(1,  NA,  1,  NA,   1,  NA,   1)),
                 filename = here("data/temp/gde-lotic.tif"))


# reclassify for lentic
rasterDT::subsDT(x = raster(gde_w),
                 dict = data.frame(from = c(1,  10, 11, 100, 101, 110, 111),
                                   to   = c(NA,  1,  1,  NA,  NA,   1,   1)),
                 filename = here("data/temp/gde-lentic.tif"))

# reclassify for all
rasterDT::subsDT(x = raster(gde_w),
                 dict = data.frame(from = c(1,  10, 11, 100, 101, 110, 111),
                                   to   = c(1,  1,  1,    1,  1,   1,   1)),
                 filename = here("data/temp/gde-all.tif"))

# create stack of above
gde_stack = c(
  # terra::rast(here("data/temp/gde-all.tif")), 
  terra::rast(here("data/temp/gde-terr.tif")),  # terrestrial GDEs
  terra::rast(here("data/temp/gde-lotic.tif")) + terra::rast(here("data/temp/gde-lentic.tif"))) # aquatic GDEs

gde_stack[[2]][gde_stack[[2]] > 1] = 1

# import land area
l_area = terra::rast(here("data/ggrid_30arcsec.tif"))

# loop through layers calculating area fraction at 10km
gde_stack = gde_stack * l_area
gde_stack = terra::aggregate(x = gde_stack, fact = 10, fun = "sum", na.rm = TRUE)

gde_dens_stack = gde_stack / terra::rast(here("data/ggrid_masked_5arcmin.tif"))
gde_dens_stack[gde_dens_stack > 1] = 1
gde_dens_stack[is.na(gde_dens_stack)] = 0
names(gde_dens_stack) = c('gde_t', 'gde_a')

terra::writeRaster(x = gde_dens_stack,
                   filename = here("data/input/gde-density.tif"),
                   overwrite = T)
