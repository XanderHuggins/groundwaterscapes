clust_reconcile = function(data.in, col_ref, col_reclass){

  # Calculate centroids of reference cluster set
  reference_centroids = data.frame(clust = 1:(data.in[,col_ref] |> unique() |> length()),
                                   climate = rep(NA),
                                   baseflow = rep(NA),
                                   wetland = rep(NA),
                                   ag = rep(NA),
                                   iwrm = rep(NA),
                                   gini = rep(NA),
                                   npv = rep(NA))
  
  reference_centroids$clust %<>% as.numeric()
  
  for (i in reference_centroids$clust) {
    reference_centroids$climate[i]  = data.in |> filter(!!as.symbol(col_ref) == i) |> pull(climate_n) |> mean(na.rm = T)
    reference_centroids$baseflow[i] = data.in |> filter(!!as.symbol(col_ref) == i) |> pull(baseflow_n) |> mean(na.rm = T)
    reference_centroids$wetland[i]  = data.in |> filter(!!as.symbol(col_ref) == i) |> pull(wet_frac_n) |> mean(na.rm = T)
    reference_centroids$ag[i]       = data.in |> filter(!!as.symbol(col_ref) == i) |> pull(ag_dep_n) |> mean(na.rm = T)
    reference_centroids$iwrm[i]     = data.in |> filter(!!as.symbol(col_ref) == i) |> pull(iwrm_n) |> mean(na.rm = T)
    reference_centroids$gini[i]     = data.in |> filter(!!as.symbol(col_ref) == i) |> pull(gini_n) |> mean(na.rm = T)
    reference_centroids$npv[i]      = data.in |> filter(!!as.symbol(col_ref) == i) |> pull(npv_gw_n) |> mean(na.rm = T)
  }
  
  # Calculate centroids of target cluster set (i.e. to reclassify)
  target_centroids = reference_centroids
  target_centroids[] = NA
  target_centroids$clust = 1:(data.in[,col_reclass] |> unique() |> length())
  
  for (i in target_centroids$clust) {
    target_centroids$climate[i]  = data.in |> filter(!!as.symbol(col_reclass) == i) |> pull(climate_n) |> mean(na.rm = T)
    target_centroids$baseflow[i] = data.in |> filter(!!as.symbol(col_reclass) == i) |> pull(baseflow_n) |> mean(na.rm = T)
    target_centroids$wetland[i]  = data.in |> filter(!!as.symbol(col_reclass) == i) |> pull(wet_frac_n) |> mean(na.rm = T)
    target_centroids$ag[i]       = data.in |> filter(!!as.symbol(col_reclass) == i) |> pull(ag_dep_n) |> mean(na.rm = T)
    target_centroids$iwrm[i]     = data.in |> filter(!!as.symbol(col_reclass) == i) |> pull(iwrm_n) |> mean(na.rm = T)
    target_centroids$gini[i]     = data.in |> filter(!!as.symbol(col_reclass) == i) |> pull(gini_n) |> mean(na.rm = T)
    target_centroids$npv[i]      = data.in |> filter(!!as.symbol(col_reclass) == i) |> pull(npv_gw_n) |> mean(na.rm = T)
  }
  
  
  # Determine nearest centroid in each reference set for each target set
  reclass_df = data.frame(target_clut = target_centroids$clust |> unique(),
                          reference_clust = rep(NA))
  

  for (i in reclass_df$target_clut) {
    # i = 1
    
    target_vect = target_centroids |> filter(clust == i) |> dplyr::select(-clust)
    
    dist_df = data.frame(clust = unique(reclass_df$target_clut), 
                         dist = rep(NA), 
                         rank = rep(NA))
    
    for (j in reclass_df$target_clut) {
      # j = 1
      reference_vect = reference_centroids |> filter(clust == j) |> dplyr::select(-clust)    
      dist_df$dist[j] = dist(rbind(target_vect, reference_vect), method = "euclidean")
    }
    
    dist_df$rank = rank(dist_df$dist)
    reclass_df$reference_clust[i] = dist_df |> filter(rank == 1) |> pull(clust)

  }
  
  names(reclass_df) = c(col_reclass, col_ref)

  return(reclass_df)
  
}