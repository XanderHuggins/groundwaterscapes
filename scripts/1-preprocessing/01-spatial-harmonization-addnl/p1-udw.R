# import vector of udw
udw = terra::vect(x = "D:/Geodatabase/WRI-Aqueduct/udw_raw.sqlite") 

udw$udw_norm = ( udw$udw_raw - mean(udw$udw_raw, na.rm = T) ) / sd(udw$udw_raw, na.rm = T)

terra::writeVector(x = udw,
                   filename = "D:/Geodatabase/WRI-Aqueduct/udw_norm.gpkg",
                   filetype = "GPKG",
                   overwrite = TRUE)

# rasterize 
terra::rasterize(x = terra::vect("D:/Geodatabase/WRI-Aqueduct/udw_norm.gpkg"),
                 y = terra::rast(here("data/ggrid_5arcmin.tif")),
                 field = 'udw_norm',
                 touches = TRUE,
                 filename = here("data/input/udw_norm.tif"),
                 overwrite = TRUE)

plot(terra::rast(here("data/input/udw_norm.tif")))

# also rasterise the raw value for later use 
terra::rasterize(x = terra::vect("D:/Geodatabase/WRI-Aqueduct/udw_norm.gpkg"),
                 y = terra::rast(here("data/ggrid_5arcmin.tif")),
                 field = 'udw_raw',
                 touches = TRUE,
                 filename = here("data/input/udw_raw.tif"),
                 overwrite = TRUE)