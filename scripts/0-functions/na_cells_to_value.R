na_cells_to_val = function(rast.in, value){
  rast.in[is.na(rast.in)] = value
  return(rast.in)
}
