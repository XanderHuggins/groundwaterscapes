raster_scale = function(rast.in, exception.val){
  rast_mean = mean(rast.in[rast.in != exception.val] |> as.vector(), na.rm = T)
  rast_sd = sd(rast.in[rast.in != exception.val] |> as.vector(), na.rm = T)
  
  rast.in = ( rast.in - rast_mean ) / rast_sd
  
  rast.in[rast.in > 2]  = 2
  rast.in[rast.in < -2] = -2
  return(rast.in)
}