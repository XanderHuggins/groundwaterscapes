# Name: p2-core-input-stack.R
#   description:  

# id ras
id_ras = terra::rast(here("data/earth_mask_5arcmin.tif"))
id_ras[] = 1:ncell(id_ras)

# Create raster stack of all inputs and mask to land areas
data_stack = c(
  terra::rast(here("data/input/wtr_5arcmin_aridmask.tif")),
  terra::rast(here("data/input/porosity_5arcmin.tif")),
  terra::rast(here("data/input/gde-density.tif")) |> na_cells_to_val(value = 0), # where there are no GDEs, set to 0
  terra::rast(here("data/input/field_size_modal_rcl_5arcmin.tif")) |> na_cells_to_val(value = 0), # where there are no fields, set to 0
  terra::rast(here("data/input/gmia_aeigw_pct_aei.tif")) |> na_cells_to_val(value = 0), # where there is no gw irr, set to 0
  # terra::rast(here("data/input/value_of_wat_in_ag.tif")) |> na_cells_to_zero(), # where there is no econ value data, set to 0
  # terra::rast(here("data/input/iwrm-gw-layers-norm.tif")), # old analysis used IWRM 
  terra::rast(here("data/input/wgi_ge_2020.tif")), # new version uses WGI GovEff
  terra::rast(here("data/input/udw_norm.tif")),
  id_ras) |> 
  terra::mask(mask = terra::rast(here("data/earth_mask_5arcmin.tif")),
              maskvalues = c(0,2,3))
# |> 
#   terra::mask(mask = terra::rast(here("data/input_raster_coverage.tif")),
#               maskvalues = c(1:5))

names(data_stack) = c('wtr', 'por', 'gde_t', 'gde_a', 'fsize', 'aeigw', 'wgi_ge', 'udw', 'id')

# mask-out areas below 60S from analysis
Sof60r = rast(x = rast(WGS84_areaRaster(5/60)), vals = 0)
Sof60r[1800:2160,] = 1 # rows that correspond with areas south of 60S
data_stack[Sof60r == 1] = NA

# import greenland and rasterize to mask out
nat_vect = rnaturalearth::ne_countries(scale = 10, returnclass = "sf") |> 
  terra::vect()
greenland = nat_vect[nat_vect$admin == "Greenland"]
greenland = terra::buffer(greenland, width = 10e3) # 10 km buffer
greenland_r = terra::rasterize(x = greenland, y = data_stack, touches = T)

# check for complete data coverage across all inputs 
complete_coverage = rast(Sof60r)
complete_coverage[] = 1

for (i in 1:nlyr(data_stack)) {
  complete_coverage[is.na(data_stack[[i]])] = 0
}
complete_coverage[greenland_r == 1 | complete_coverage != 1 | is.na(complete_coverage)] = 0
plot(complete_coverage)

writeRaster(complete_coverage,
            filename = here("data/complete_coverage.tif"),
            overwrite = T)

data_stack[complete_coverage == 0] = NA

terra::writeRaster(x = data_stack, 
                   filename = here("data/input-data-stack-originalvals.tif"),
                   overwrite = TRUE)

data_stack_df = rast(here("data/input-data-stack-originalvals.tif"))
data_stack_df = data_stack_df |> as_tibble()
hist(data_stack_df$udw)
data_stack_df |> filter(wtr != -9) |> summary()
readr::write_rds(x = data_stack_df, file = here("data/ds_df_full_originalvals.rds"))

# create constant mask for project
mask_proj = terra::rast(here("data/earth_mask_5arcmin.tif"))
mask_proj[mask_proj != 1] = NA
mask_proj[Sof60r == 1] = NA
mask_proj[complete_coverage == 0] = NA

terra::writeRaster(x = mask_proj, 
                   filename = here("data/project_mask.tif"),
                   overwrite = TRUE)

# now normalize the data that isn't already normalized

# wtr
data_stack[[1]] = raster_scale(rast.in = data_stack[[1]], exception.val = -9)

# por
data_stack[[2]] = raster_scale(rast.in = data_stack[[2]], exception.val = -1) # no exception val, so value is out of data range

# gde_t
data_stack[[3]] = raster_scale(rast.in = data_stack[[3]], exception.val = 0)

# gde_a
data_stack[[4]] = raster_scale(rast.in = data_stack[[4]], exception.val = 0)

# fsize
data_stack[[5]] = raster_scale(rast.in = data_stack[[5]], exception.val = 0)

# aeigw
data_stack[[6]] = raster_scale(rast.in = data_stack[[6]], exception.val = 0)

# integrated management and unimproved drinking water are already scaled

# see scaled maps
plot(data_stack)
summary(data_stack)

# write to file
terra::writeRaster(x = data_stack, 
                   filename = here("data/input-data-stack-norm.tif"),
                   overwrite = TRUE)

# Clean to include only grid cells with complete data availability
ds_df = data_stack |> 
  as.data.frame() |> 
  drop_na() 

nrow(ds_df)/1e6 #2.065593 million rows

readr::write_rds(x = ds_df, file = here("data/ds_df_full.rds"))
