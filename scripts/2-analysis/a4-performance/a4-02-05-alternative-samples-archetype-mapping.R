
# Now classify all data points and make maps for each alternative

for (k in 2:6) {
  # k = 2
  # Import sample data, prototypes and archetypes
  input_df = readr::read_rds(paste0(here("data/ds_df_sampled_for_SOM_"), k, ".rds")) |> as_tibble()
  prototypes_df = readr::read_rds(paste0(here("data/som-iter/alternative-samples/kohonen-objs/winning_som1"), "/winning_som1_alt_", k, ".rds"))
  archetypes_df = readr::read_rds(paste0(here("data/som-iter/alternative-samples/kohonen-objs/winning_som2/som2_archetypes_iter_"), k, ".rds"))
  
  prototypes_df$codes[[1]] |> as_tibble() |> nrow()
  # archetypes_df$unit.classif[334] #|> length()
  
  # Create dictionary to map cell IDs to prototypes
  training_prototype_dict = data.frame(
    cellID  =  input_df$id,
    ptypeID =  prototypes_df$unit.classif
  )
  
  # Create second dictionary to map prototype IDs to archetype IDs
  proto_arche_dict = data.frame(
    ptypeID = seq(1:length(archetypes_df$unit.classif)),
    archeID = archetypes_df$unit.classif
  )
  
  membership_dict = merge(x = training_prototype_dict, y = proto_arche_dict,
                          by.x = "ptypeID", by.y = "ptypeID", all.y = TRUE) |> 
    dplyr::select(cellID, ptypeID, archeID) |> 
    set_colnames(c('cellID', 'pID', 'aID'))
  
  # membership_dict |> filter(is.na(cellID)) # these are pID:aID pairs that do not appear in the dataset
  # membership_dict = membership_dict[complete.cases(membership_dict$cellID),]
  # membership_dict$cellID[is.na(membership_dict$cellID)] = 0 # reclass function below can ignore 0 but not NA entry for cellID
  
  prototypes_trainedset = rasterDT::subsDT(x = raster(rast(here("data/input-data-stack.tif"))[[8]]),
                                           dict = data.frame(from = membership_dict$cellID,
                                                             to   = membership_dict$pID) |> dplyr::filter(from > 0), # prototype ID
                                           # filename = here("data/MAP_prototypes_trainedset_iter.tif"),
                                           overwrite = TRUE)
  
  prototypes_assigned = reclass_nonsampled_prot(sample_df = prototypes_df$codes |> as.data.frame() |> as_tibble(), 
                                                full_df = readr::read_rds(here("data/input_features_full_set_norm.rds")) |> as_tibble(),
                                                membership = membership_dict |> as_tibble(),
                                                id_rast = raster(rast(here("data/input-data-stack.tif"))[[8]]),
                                                intvl = 1e4)
  
  prototype_mosaic_full = terra::mosaic(prototypes_trainedset, prototypes_assigned, fun = "min") |> # function does not matter as no overlapping cells
    rast() 
  
  # remove oceans from map
  mask_layer = terra::rast(here("data/project_mask.tif"))
  prototype_mosaic_full[is.na(mask_layer)] = NA
  
  # terra::writeRaster(prototype_mosaic_full,
  #                    filename = here("data/MAP_prototypes.tif"),
  #                    overwrite = TRUE)
  
  # 4/ Now reclassify from prototypes to archetypes using look-up dictionary
  rcl.m = membership_dict |> group_by(pID) |> summarise(aID = unique(aID) |> as.numeric())
  
  archetypes_full = rasterDT::subsDT(x = raster(prototype_mosaic_full),
                                     dict = data.frame(from = rcl.m$pID, 
                                                       to   = rcl.m$aID),
                                     filename = paste0(here("data/MAP_archetypes_iter_"), k, ".tif"),
                                     overwrite = TRUE)
  
  plot(archetypes_full)
  
}
