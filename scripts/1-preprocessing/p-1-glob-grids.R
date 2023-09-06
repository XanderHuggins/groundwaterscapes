# Name: p0-glob-grids.R

# 30 arcsecond (1km)
writeRaster(x = rast(WGS84_areaRaster(0.5/60)),
            filename = here("data/ggrid_30arcsec.tif"),
            overwrite = T)

# 0.05 decimal degrees
writeRaster(x = rast(WGS84_areaRaster(0.05)),
            filename = here("data/ggrid_0d05dd.tif"),
            overwrite = T)

# 5 arcminute (10km)
writeRaster(x = rast(WGS84_areaRaster(5/60)),
            filename = here("data/ggrid_5arcmin.tif"),
            overwrite = T)

# 5 arcminute masked
emask = terra::rast(here("data/earth_mask_5arcmin.tif"))
emask[emask != 1] = NA

writeRaster(x = ( emask * terra::rast(here("data/ggrid_5arcmin.tif")) ),
            filename = here("data/ggrid_masked_5arcmin.tif"),
            overwrite = T)


emask = terra::rast(here("data/earth_mask_5arcmin.tif"))
emask[emask == 0] = NA
emask[emask > 0] = 1

emask = terra::crop(x = emask, 
                    y = terra::ext(c(-179, 179, -60, 90)))
emask_vect = terra::as.polygons(x = emask, dissolve = T)

writeVector(x = emask_vect,
            filename = here("data/input-earthmask-poly.sqlite"),
            filetype = "SQLite", overwrite = T)
