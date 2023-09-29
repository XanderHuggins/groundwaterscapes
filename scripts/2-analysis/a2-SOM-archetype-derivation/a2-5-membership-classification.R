## --------------------------- \
# Script purpose: Map prototypes and archetypes from data frames to grid cells

# Import sample data, prototypes and archetypes
input_df = readr::read_rds(here("data/ds_df_sampled_for_SOM_1.rds")) |> as_tibble()
prototypes_df = readr::read_rds(here("data/som-iter/final-kohonen-objs/som1_prototypes.rds"))
archetypes_df = readr::read_rds(here("data/som-iter/final-kohonen-objs/som2_archetypes_reclassed.rds"))

prototypes_df$codes[[1]] |> as_tibble() |> nrow()

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
write_rds(membership_dict, file = here("data/DICT_training_prototype_archetype.rds")) # write to file for SI plotting

# Create a raster that populated grid cells with Prototype IDs from grid cells that are in 350e3 sample set
prototypes_trainedset = rasterDT::subsDT(x = raster(rast(here("data/input-data-stack.tif"))[[8]]),
                                         dict = data.frame(from = membership_dict$cellID,
                                                           to   = membership_dict$pID) |> dplyr::filter(from > 0), # prototype ID, and don't classify NA:pID pairs
                                         filename = here("data/MAP_prototypes_trainedset.tif"),
                                         overwrite = TRUE)

# Create a raster that populates grid cells with Prototype IDs for grid cells that are NOT in 350e3 sample set
# Assign based on nearest neighbour (Euclidean distance in input feature space) for each grid cell and the centroids of the derived prototypes
prototypes_assigned = reclass_nonsampled_prot(sample_df = prototypes_df$codes |> as.data.frame() |> as_tibble(), 
                                              full_df = readr::read_rds(here("data/input_features_full_set_norm.rds")) |> as_tibble(),
                                              membership = membership_dict |> as_tibble(),
                                              id_rast = raster(rast(here("data/input-data-stack.tif"))[[8]]),
                                              intvl = 1e4)

# Mosaic the two above rasters 
prototype_mosaic_full = terra::mosaic(prototypes_trainedset, prototypes_assigned, fun = "min") |> # function does not matter as no overlapping cells
  rast() 
                                      
# remove oceans from map
mask_layer = terra::rast(here("data/project_mask.tif"))
prototype_mosaic_full[is.na(mask_layer)] = NA

terra::writeRaster(prototype_mosaic_full,
                   filename = here("data/MAP_prototypes.tif"),
                   overwrite = TRUE)

# 4/ Now reclassify from prototypes to archetypes using look-up dictionary
rcl.m = membership_dict |> group_by(pID) |> summarise(aID = unique(aID) |> as.numeric())

# create raster of archetype IDs
archetypes_full = rasterDT::subsDT(x = raster(here("data/MAP_prototypes.tif")),
                                   dict = data.frame(from = rcl.m$pID, 
                                                     to   = rcl.m$aID),
                                   filename = here("data/MAP_archetypes.tif"),
                                   overwrite = TRUE)
plot(archetypes_full)

# clear old files from previous runs
do.call(file.remove, list(list.files(here("data/"), pattern = "protMAP_in_arch_", full.names = T)))
do.call(file.remove, list(list.files(here("data/"), pattern = "ARCH_extent_", full.names = T)))

# create raster of prototypes masked by individual archetype extents (for later plotting)
for (i in 1:10) {
  # i = 6
  prot_iter = prototype_mosaic_full
  prot_iter[rast(archetypes_full) != i] = NA
  # plot(prot_iter)
  
  writeRaster(prot_iter, 
              filename = paste0(here("data/protMAP_in_arch_"), i, ".tif"),
              overwrite = T)
  
  message("arch ", i, " is done")
}

# create raster of each archetype's extent
for (i in 1:10) {
  arch_iter = archetypes_full
  arch_iter[arch_iter != i] = NA
  arch_iter[arch_iter >= 1] = 1  
  
  writeRaster(arch_iter, 
              filename = paste0(here("data/ARCH_extent_"), i, ".tif"),
              overwrite = T)
  
  message("arch ", i, " is done")
}
