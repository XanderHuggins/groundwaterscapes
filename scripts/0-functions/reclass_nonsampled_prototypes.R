reclass_nonsampled_prot = function(sample_df, full_df, membership, id_rast, intvl){
  
  ## data to import below to check that function works
  # sample_df = prototypes_df$codes |> as.data.frame() |> as_tibble()
  # full_df = readr::read_rds(here("data/input_features_full_set_norm.rds")) |> as_tibble()
  # membership = membership_dict |> as_tibble()
  # id_rast = raster(rast(here("data/input-data-stack.tif"))[[8]])
  # intvl = 1e4
  
  ## --------------------------- \
  # function starts here:
  
  
  # identify grid cell IDs that are NOT in the sampled set
  cells_to_assign_df = full_df |>   
    subset(id %!in% membership$cellID)
  
  # determine number of loops in increments of set INTVL (not done all at once for memory purposes)
  nloop = (round(nrow(cells_to_assign_df)/intvl, 0)+1)
  message("number of loops: ", nloop)
  
  # create classification dictionary for all nonsampled cell IDs to prototype IDs
  class_dict = data.frame(
    cID = rep(NA, 0),
    pID = rep(NA, 0)
  )
  
  # for each batch of INTVL rows (to manage RAM, especially when vectorizing), 
  # identify nearest classified grid cell for each nonclassified grid cell, and assign prototype
  
  for (i in 1:nloop) { # 208 loops
    
    if (i == 1) {start_val = 1}
    
    # calculate Euclidean distance from individual grid cell requiring prototype assignment to all prototype centroids
    dist_matrix = 
      rdist.w.na(X = cells_to_assign_df[start_val : min((start_val + intvl-1), nrow(cells_to_assign_df)),] |> 
                   dplyr::select(-id) |> as.matrix(), 
                 Y = sample_df |> as.matrix()) |> 
      t() |>
      c() |> 
      matrix(byrow = FALSE, ncol = min((start_val + intvl-1), nrow(cells_to_assign_df)) - start_val + 1)
    
    # identify the closest prototype for each unsampled cell in loop
    min_dist_loc = apply(dist_matrix, 2, which.min) 
    
    # populate the classification dictionary with result
    class_dict_loop = data.frame(
      cID = cells_to_assign_df[start_val : min((start_val + intvl-1), nrow(cells_to_assign_df)),] |> dplyr::pull(id), # cell ID needing classification
      pID = min_dist_loc # possible because position in table corresponds to the prototype ID
    )
    
    # bind looped classification dictionaries 
    class_dict = rbind(class_dict, class_dict_loop)
    start_val = (i * intvl) + 1
    message("loop ", i, "/", nloop, " is done")
    message("number of rows in class_dict so far is ", nrow(class_dict))
    
  }
  
  # now reclassify ID raster 
  return_raster =  rasterDT::subsDT(x = id_rast,
                                    dict = data.frame(from = class_dict$cID,
                                                      to   = class_dict$pID))
  
  return(return_raster)  
}
