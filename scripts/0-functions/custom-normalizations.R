# data normalizing scripts

wtr_norm = function(dat.in) {
  dat.out = dat.in 
  dat.out[dat.out < -2] = -2 # lower limit -- same as in Cuthbert et al. 2019 legend 
  dat.out[dat.out > 2] = 2 # upper limit -- same as in Cuthbert et al. 2019 legend
  dat.out = (dat.out + 2) / 4 # scales from 0-1
  return(dat.out)
}

field_size_norm = function(dat.in){
  return((dat.in - 1 )/4)
}

aeigw_norm = function(dat.in){ # unsure if any scaling should be applied to already bounded [0,1] data (but heavy-tailed) 
  dat.out = dat.in/0.2
  dat.out[dat.out > 1] = 1
  return(dat.out)
}

iwrm_norm = function(dat.in){
  norm.val = max(c(abs(quantile(dat.in, probs = 0.05)),
                   abs(quantile(dat.in, probs = 0.95))))
  
  dat.in = dat.in / norm.val
  dat.in[dat.in < -1] = -1
  dat.in[dat.in >  1] =  1
  
  return((dat.in + 1) / 2)
}

idr_norm = function(dat.in){
  
  idr = quantile(dat.in, probs = 0.9) - quantile(dat.in, probs = 0.1)
  
  temp.val = ( dat.in - quantile(dat.in, probs = 0.1) ) / idr
  temp.val[temp.val > 1] = 1
  temp.val[temp.val < 0] = 0
  
  return(temp.val)
  
}

