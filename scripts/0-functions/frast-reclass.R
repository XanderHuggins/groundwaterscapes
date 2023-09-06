frast_reclass = function(rast_to_reclass, reclass_df, old_val, new_val){
  
  rcl.mtx = data.frame(from = reclass_df |> pull(!!as.symbol(old_val)) |> as.numeric(),
                       to   = reclass_df |> pull(!!as.symbol(new_val))|> as.numeric())  
  
  new_rast = rasterDT::subsDT(x = raster(rast_to_reclass), dict = rcl.mtx)
  
  return(rast(new_rast))
}