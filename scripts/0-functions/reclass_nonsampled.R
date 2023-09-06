reclass_nonsampled = function(sample_df, full_df, membership, id_rast, intvl){
  
  # sample_df = SOM_archetyes_df
  # full_df = ss_df
  # membership = membership_dict
  # id_rast = terra::rast(here("data/input-data-stack.tif"))
  # output_loc = here("data/clusters/recursive_som_9_clusters_full_cells.tif")
  # intvl = 1e3
  
  cells_to_assign_df = full_df |> as.data.frame() |>  
    subset(id %!in% membership$cellID)
  
  # finding nearest neighbour in parameter space
  sample_copy_ps  = sample_df |> dplyr::select(-ptypeID) # keep only parameter space to map distances to/from
  
  # create classification dictionary for all nonsampled cell IDs to agn7 clusters
  class_dict = data.frame(
    cIDs = rep(NA, 0),
    arche = rep(NA, 0)
  )
  
  # for each batch of 1000 rows (to manage RAM, especially when vectorizing), 
  # identify nearest classified grid cell for each nonclassified grid cell, and set to agn7 cluster
  
  for (i in 1:round(nrow(cells_to_assign_df)/intvl, 0)) { # 208 loops
    
    # i = 1
    # start_val = 1
    
    if (i == 1) {start_val = 1}
    dist_matrix = 
      rdist.w.na(X = cells_to_assign_df[start_val : min((i*intvl), nrow(cells_to_assign_df)),] |> 
                   dplyr::select(-id) |> as.matrix(), 
                 Y = sample_copy_ps |> as.matrix()) |> 
      t() |>
      c() |> 
      matrix(byrow = FALSE, ncol = min((i*intvl), nrow(cells_to_assign_df)) - start_val + 1)
    # dist_matrix
    
    min_dist_loc = apply(dist_matrix, 2, which.min) 
    
    class_dict_loop = data.frame(
      cIDs  = cells_to_assign_df[start_val : min((i*intvl), nrow(cells_to_assign_df)),] |> dplyr::pull(id),
      arche = min_dist_loc # because position in table dictates the archetype number
    )
    
    class_dict = rbind(class_dict, class_dict_loop)
    start_val = (i * intvl) + 1
    message("loop ", i, " is done")
    message("number of rows in class_dict so far is ", nrow(class_dict))
    
  }
  
  # now reclassify ID raster 
  archetypes_full_cells =  rasterDT::subsDT(x = raster(id_rast),
                                            dict = data.frame(from = class_dict$cIDs,
                                                              to   = class_dict$arche))
 return(archetypes_full_cells)  
}
