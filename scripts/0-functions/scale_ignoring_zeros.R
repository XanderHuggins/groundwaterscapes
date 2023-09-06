
scale_ignoring_zeros = function(dat.in){
  
  keep.in = dat.in[dat.in > 0]
  
  (dat.in - mean(keep.in)) / sd(keep.in)
  
  
}
