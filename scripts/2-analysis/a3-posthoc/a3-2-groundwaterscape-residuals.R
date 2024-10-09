### ---------------------\\ 
# Script objective:
# Calculate residual from local groundwaterscape model per grid cell
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

# Import final archetypes map
atypes = terra::rast(here("data/groundwaterscapes-currentiter"))

# Import archetypes codebook vector
cbv = readr::read_rds(here("data/som_files/som_selections/som2_selection.rds"))
cbv = cbv$codes[[1]] |> as_tibble()
cbv$ID = seq(1, 15)

atype_wtr   = rasterDT::subsDT(x = raster(atypes), dict = data.frame(from = cbv$ID, to = cbv$wtr)) |> rast()
atype_por   = rasterDT::subsDT(x = raster(atypes), dict = data.frame(from = cbv$ID, to = cbv$por)) |> rast()
atype_gdet  = rasterDT::subsDT(x = raster(atypes), dict = data.frame(from = cbv$ID, to = cbv$gde_t)) |> rast()
atype_gdea  = rasterDT::subsDT(x = raster(atypes), dict = data.frame(from = cbv$ID, to = cbv$gde_a)) |> rast()
atype_fsize = rasterDT::subsDT(x = raster(atypes), dict = data.frame(from = cbv$ID, to = cbv$fsize)) |> rast()
atype_aeigw = rasterDT::subsDT(x = raster(atypes), dict = data.frame(from = cbv$ID, to = cbv$aeigw)) |> rast()
atype_gwmgm = rasterDT::subsDT(x = raster(atypes), dict = data.frame(from = cbv$ID, to = cbv$wgi_ge)) |> rast() 
atype_udw   = rasterDT::subsDT(x = raster(atypes), dict = data.frame(from = cbv$ID, to = cbv$udw))|> rast()


# identify distance between each grid cell and its archetype's codebook vector:
data_stack = rast(here("data/input-data-stack-norm.tif"))

dist_rast = abs(atype_wtr - data_stack[[1]])^2 +
  abs(atype_por - data_stack[[2]])^2 +
  abs(atype_gdet - data_stack[[3]])^2 +
  abs(atype_gdea - data_stack[[4]])^2 +
  abs(atype_fsize - data_stack[[5]])^2 +
  abs(atype_aeigw - data_stack[[6]])^2 +
  abs(atype_gwmgm - data_stack[[7]])^2 +
  abs(atype_udw - data_stack[[8]])^2

dist_rast = sqrt(dist_rast)
plot(dist_rast)

# calculate sd and mean
stat_df = c(atypes, dist_rast, rast(WGS84_areaRaster(5/60))) |> 
  as.data.frame() |> 
  set_colnames(c('id','resid', 'area')) |>
  group_by(id) |> 
  summarise(
    sd = Hmisc::wtd.var(x = resid, weights = area, na.rm = T) |> sqrt(),
    mean = Hmisc::wtd.mean(x = resid, weights = area, na.rm = T)
  )
stat_df
dist_rast_z = (dist_rast - stat_df$mean) / stat_df$sd

# appraoch that performs z score normalization per archetype
dist_rast_z = rast(atypes)

for (jj in 1:15) {
  # jj = 1
  dist_rast_z[atypes == jj] = (dist_rast[atypes == jj] - stat_df$mean[jj]) / stat_df$sd[jj]
  
}

# plot world map of this residual
outline = terra::vect(here("data/input/land_mask_polygon.sqlite")) |> st_as_sf()
dist_rast_z_proj = terra::project(x = dist_rast_z, y = "+proj=robin", method = "near")

map =  
  tm_shape(outline, projection = "+proj=robin") +
  tm_fill(col = "grey", lwd = 0, border.col = NA) +
  tm_shape(dist_rast_z_proj) +
  tm_raster(palette = met.brewer(name = "Hiroshige", direction = -1,
                                 n = 100, type = "continuous"),
            breaks = seq(-2, 2, by = 0.1), midpoint = 0) +
  tm_shape(outline) + 
  tm_borders(col = "black", lwd = 1) +
  tm_layout(legend.show = F, legend.frame = F, frame = F)
map
tmap_save(map, here("plots/global-maps/residual_map.pdf"), dpi = 400, units = "in")  

# now plot ridgeline plot of residual z-scores per archetype
ridge_df = c(dist_rast_z, atypes) |> 
  as.data.frame() |> 
  drop_na() |> 
  set_colnames(c('dist', 'arch'))

ggplot(ridge_df, aes(x = dist, y = as.factor(arch), fill = as.factor(arch))) +
  geom_density_ridges(scale = 2, bandwidth = 0.1) +
  theme_minimal() +
  coord_cartesian(xlim = c(-3, 3), expand = 0) +
  scale_fill_manual(values = pal_arch) +
  theme(legend.position = "none") +
  xlab('Residual between grid cell data and archetype median') + ylab('Archetype')
ggsave(plot = last_plot(), filename = here("plots/residual_ridgeline_archetypes.png"),
       height = 12.5, width = 15, units = "cm", dpi = 400)  