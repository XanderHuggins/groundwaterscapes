
# import unique function combination raster and calculate area distribution of each unique id
id_area_dist = c(rast(here("data/matrix_stack.tif"))[[5]],
                 rast(here("data/ggrid_5arcmin.tif"))) |> 
  as.data.frame() |> 
  set_colnames(c('id', 'area')) |> 
  group_by(id) |> 
  drop_na() |> 
  summarise(
    area= sum(area, na.rm = T)
  ) |> 
  mutate(
    area = area/sum(area),
    id = as.character(id)
  ) 


# import raster stack and also write to file (.rds) for later use
data_stack = c(terra::rast(here("data/input-data-stack-norm.tif")), rast(here("data/matrix_stack.tif"))[[5]])
# data_stack_df = data_stack |> as.data.frame() |> dplyr::select(!all)
# write_rds(data_stack_df, file = here("data/input_features_full_set_norm.rds"))

data_stack$all |> unique() |> nrow()
unique_patterns = data_stack$all |> as.vector() |> table() |> as.data.frame()
unique_patterns |> filter(Freq >= 1) |> nrow() #> 79177
unique_patterns |> filter(Freq == 1) |> nrow() #> 26729
unique_patterns |> filter(Freq > 1) |> nrow()  #> 52448

# determine how large the sample size needs to be to cover 95% of these unique function configurations by area 
configs_list = data_stack |> as.data.frame() |> drop_na()

sample_size = 350e3
#### function to check area coverage
configs_sample = configs_list |> slice_sample(n = sample_size)
round(id_area_dist |> filter(id %in% configs_sample$all) |> pull(area) |> sum()*100, 2)

id_rast_in_sample = rasterDT::subsDT(x = raster(terra::rast(here("data/input-data-stack-norm.tif"))[[9]]),
                                     dict = data.frame(from = configs_sample$id,
                                                       to   = rep(1, nrow(configs_sample)))) # prototype ID
plot(id_rast_in_sample)
id_rast_in_sample_v = terra::as.points(rast(id_rast_in_sample))
nrow(id_rast_in_sample_v)
plot(id_rast_in_sample_v)


# create 10 alternative data samples, writing out each
# this enables us to test the robustness of archetypes to
# the sampling process

data_stack_df = data_stack |> as.data.frame() |> drop_na()
nrow(data_stack_df)/1e6 #> 2.04036 million rows (i.e. grid cells)

for (set_i in 1:10) {
  # set_i = 1
  data_stack_df_sample = data_stack_df |> slice_sample(n = sample_size)
  # # see how many overspecified cell IDs are included here... 
  # id_in_sample = data_stack_ss$all |> unique() 
  # data_stack_ss$all |> unique() |> length()
  # 
  readr::write_rds(x = data_stack_df_sample, 
                   file = paste0(here("data/ds_df_sampled_for_SOM_"), 
                                      set_i, ".rds"))
  
  npat = data_stack_df_sample$all |> unique() |> length()
  message(sqrt(2 * (npat^0.4))) # message out; to give insight into size of first-stage SOM
}


data_sample = read_rds(here("data/ds_df_sampled_for_SOM_3.rds")) 
nrow(data_sample)  
data_sample |> slice_sample(n = 1e3) |> plot()