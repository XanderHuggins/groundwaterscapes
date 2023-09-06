
# ideographic classifications

# 
wtr_ranges   = c(-Inf, 0.9, 1.1, Inf)

iwrm_ranges = c(-Inf, -5, 5, Inf)

jbreaks = function(dat.in, num.in){
  jb = classInt::classIntervals(dat.in, n = num.in, style = "fisher")
  jbt = jenks.tests(jb)
  message(jbt[2])
  return(jb$brks)
}
