################################-
# Calculate area-distribution of sub-domains
################################-
area_r = rast(here("data/ggrid_5arcmin.tif"))
matrix_stack = rast(here("data/unique_combinations_all_functions.tif"))

## earth
earth_ar_dist = c(matrix_stack[[1]], area_r) |> 
  as.data.frame() |> set_colnames(c('id', 'area')) |>  
  group_by(id) |> 
  summarise(
    area = sum(area, na.rm = T)
  ) |> 
  drop_na() |> 
  mutate(
    area = round(100*area/sum(area), 1),
    yval = trunc(id/10),
    xval = id - (yval*10)
  ) 

earth_ar_dist |> #dplyr::filter(area>0.1) |> 
  ggplot(aes(x = xval, y = yval, fill = area)) + 
  geom_tile() +
  scale_fill_gradientn(colours = met.brewer("Hokusai2", n = 20), limits = c(0,10), oob = scales::squish) +
  theme_void() +
  theme(legend.position = "None") 
ggsave(plot = last_plot(),
       file= here("plots/earth_matrix_area_distribution.png"), bg = "transparent",
       dpi= 400, width = 5, height = 5, units = "cm")


## eco
eco_ar_dist = c(matrix_stack[[2]], area_r) |> 
  as.data.frame() |> set_colnames(c('id', 'area')) |>  
  group_by(id) |> 
  summarise(
    area = sum(area, na.rm = T)
  ) |> 
  drop_na() |> 
  mutate(
    area = round(100*area/sum(area), 1),
    yval = trunc(id/10),
    xval = id - (yval*10)
  )

eco_ar_dist |> # dplyr::filter(area>0.1) |> 
  ggplot(aes(x = xval, y = yval, fill = area)) + 
  geom_tile() +
  scale_fill_gradientn(colours = met.brewer("Hokusai2", n = 20), limits = c(0,10), oob = scales::squish) +
  theme_void() +
  theme(legend.position = "None") 
ggsave(plot = last_plot(),
       file= here("plots/eco_matrix_area_distribution.png"), bg = "transparent",
       dpi= 400, width = 5, height = 5, units = "cm")


## food
food_ar_dist = c(matrix_stack[[3]], area_r) |> 
  as.data.frame() |> set_colnames(c('id', 'area')) |>  
  group_by(id) |> 
  summarise(
    area = sum(area, na.rm = T)
  ) |> 
  drop_na() |> 
  mutate(
    yval = trunc(id/10),
    xval = id - (yval*10)
  ) |> 
  dplyr::filter(xval > 1) |> 
  mutate(
    area = round(100*area/sum(area), 1),
  )

food_ar_dist |> # dplyr::filter(area>0.1) |> 
  ggplot(aes(x = xval, y = yval, fill = area)) + 
  geom_tile() +
  scale_fill_gradientn(colours = met.brewer("Hokusai2", n = 20), limits = c(0,10), oob = scales::squish) +
  theme_void() +
  theme(legend.position = "None") + xlim(c(0.5,6.5)) 
ggsave(plot = last_plot(),
       file= here("plots/food_matrix_area_ag_fields_distribution.png"), bg = "transparent",
       dpi= 400, width = 5, height = 5, units = "cm")

## gov
gov_ar_dist = c(matrix_stack[[4]], area_r) |> 
  as.data.frame() |> set_colnames(c('id', 'area')) |>  
  group_by(id) |> 
  summarise(
    area = sum(area, na.rm = T)
  ) |> 
  drop_na() |> 
  mutate(
    area = round(100*area/sum(area), 1),
    yval = trunc(id/10),
    xval = id - (yval*10)
  )
gov_ar_dist |> # dplyr::filter(area>0.1) |> 
  ggplot(aes(x = xval, y = yval, fill = area)) + 
  geom_tile() +
  scale_fill_gradientn(colours = met.brewer("Hokusai2", n = 20), limits = c(0,10), oob = scales::squish) +
  theme_void() +
  theme(legend.position = "None")
ggsave(plot = last_plot(),
       file= here("plots/gov_matrix_area_distribution.png"), bg = "transparent",
       dpi= 400, width = 5, height = 5, units = "cm")
