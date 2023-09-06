tmap_clipproj <- function(InputRaster){
  # Function arguments:
  # InputRaster: Raster to convert
  
  clip.r <- raster::crop(raster(InputRaster), extent(-179, 180, -60, 88))
  rpj.r = projectRaster(clip.r, crs = crs("+proj=robin"), method = 'ngb')
  return(rpj.r)
}