metbrew_pal = function(n, tier) {
  if (tier == 1) {rtn = met.brewer(name="Cross", n=n, type="continuous")}
  if (tier == 2) {rtn = met.brewer(name="Klimt", n=n, type="continuous")}
  return(rtn)
}
