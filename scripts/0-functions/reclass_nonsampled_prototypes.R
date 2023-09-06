reclass_nonsampled_prot = function(sample_df, full_df, membership, id_rast, intvl){
  
  
  # sample_df = prototypes_df$codes |> as.data.frame() |> as_tibble()
  # full_df = readr::read_rds(here("data/input_features_full_set_norm.rds")) |> as_tibble()
  # membership = membership_dict |> as_tibble()
  # id_rast = raster(rast(here("data/input-data-stack.tif"))[[8]])
  # intvl = 1e4
  # 
  # Cells that aren't in the 250k sampled set
  cells_to_assign_df = full_df |>   
    subset(id %!in% membership$cellID)
  
  nloop = (round(nrow(cells_to_assign_df)/intvl, 0)+1)
  message("number of loops: ", nloop)
  
  # create classification dictionary for all nonsampled cell IDs to prototype IDs
  class_dict = data.frame(
    cID = rep(NA, 0),
    pID = rep(NA, 0)
  )
  
  # for each batch of 1000 rows (to manage RAM, especially when vectorizing), 
  # identify nearest classified grid cell for each nonclassified grid cell, and set to agn7 cluster
  
  for (i in 1:nloop) { # 208 loops
    
    # i = 171
    # start_val = (i*1e4)+1
    # min((start_val + intvl-1), nrow(cells_to_assign_df))
    # temp_df = cells_to_assign_df[start_val : min((start_val + intvl-1), nrow(cells_to_assign_df)),]
    # 7496536 %in% temp_df$id
    # temp_df |> mutate(rowID = seq(1, nrow(temp_df))) |> dplyr::filter(id == 7496536)
    
    if (i == 1) {start_val = 1}
    
    dist_matrix = 
      rdist.w.na(X = cells_to_assign_df[start_val : min((start_val + intvl-1), nrow(cells_to_assign_df)),] |> 
                   dplyr::select(-id) |> as.matrix(), 
                 Y = sample_df |> as.matrix()) |> 
      t() |>
      c() |> 
      matrix(byrow = FALSE, ncol = min((start_val + intvl-1), nrow(cells_to_assign_df)) - start_val + 1)
    # dist_matrix
    
    min_dist_loc = apply(dist_matrix, 2, which.min) 
    
    class_dict_loop = data.frame(
      cID = cells_to_assign_df[start_val : min((start_val + intvl-1), nrow(cells_to_assign_df)),] |> dplyr::pull(id),
      pID = min_dist_loc # because position in table corresponds to the prototype ID
    )
    
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
