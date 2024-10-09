gdalUtilities::gdal_rasterize(src_datasource = "D:/Geodatabase/Groundwater/GLHYMPS/GLHYMPS_wgs84.shp",
                              dst_filename = here("data/input/porosity_5arcmin.tif"),
                              at = T,
                              a = "Porosity",
                              tr = c((5/60), (5/60)),
                              te = c(-180, -90, 180, 90))