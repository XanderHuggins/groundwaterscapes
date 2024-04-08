
ggmn = terra::vect("C:/Users/xande/Desktop/ggmn_geometry_corrected.gpkg")
# ggmn = terra::as.points(ggmn)

landscapes = terra::rast(here("data/groundwater-SYSTEM-archetypes_3x3_currentiter.tif"))


distr = terra::extract(x = landscapes, y = ggmn)
hist(distr$focal_modal)

dist_df = data.frame(id = 1:18,
                     count = distr$focal_modal |> table() |> as.vector() |> unlist())
dist_df$freq = 100*dist_df$count / sum(dist_df$count)

ggplot(data = dist_df) +
  geom_bar(aes(x = id, y = freq, fill = as.factor(id)), stat = "identity") +
  scale_fill_manual(values = pal_arch) +
  scale_x_continuous(breaks = 1:18) +
  scale_y_continuous(limits = c(0, 0.35)) +
  coord_cartesian(expand = 0) +
  cowplot::theme_half_open() 

ggsave(plot = last_plot(),
       file= here("plots/ggmn_across_functional_landscapes.png"), bg = "transparent",
       dpi= 400, width = 14, height = 7, units = "cm")


# and compare to area 
rast_stack = c(terra::rast(here("data/groundwater-SYSTEM-archetypes_3x3_currentiter.tif")),
               terra::rast(here("data/ggrid_5arcmin.tif")))
names(rast_stack) = c('arche', 'area')

rast_stack = rast_stack |> as.data.frame() |> drop_na()

area_df = rast_stack |>
  group_by(arche) |> 
  summarise(area = sum(area, na.rm = T)) |> 
  mutate(areaf = round(100*area/sum(area), 2)) |> 
  as.data.frame()

dist_dfm = merge(x = dist_df, y = area_df, by.x = "id", by.y = "arche")
dist_dfm$wellDENS = 10000 * dist_dfm$count / dist_dfm$area # wells per 10,000 km2
dist_dfm$km2perwell = dist_dfm$area / dist_dfm$count
dist_dfm$km_linear_perwell = sqrt(dist_dfm$km2perwell)

ggplot(data = dist_dfm) +
  geom_bar(aes(x = id, y = wellDENS, fill = as.factor(id)), stat = "identity") +
  scale_fill_manual(values = pal_arch) +
  scale_x_continuous(breaks = 1:18) +
  scale_y_continuous(limits = c(0, 30)) +
  coord_cartesian(expand = 0) +
  cowplot::theme_half_open() 

ggsave(plot = last_plot(),
       file= here("plots/ggmn_density_per_100x100km_per_groundwaterscape.png"), bg = "transparent",
       dpi= 400, width = 14, height = 7, units = "cm")


## make a world plot using just points from the GGMN
ggmn$id = c(1:nrow(ggmn))

ggmn_gwscape = merge(x = ggmn, y = distr, by.x = "id", by.y = "ID")

ggmn = tm_shape(rnaturalearth::ne_countries(), projection = "+proj=robin") +
  tm_polygons(border.col = "grey", col = "grey") +
  tm_shape(st_as_sf(ggmn_gwscape)) +
  tm_symbols(col = "focal_modal", # RGS not log10(RGS)
             shape = 21,
             size = 0.1,
             border.alpha = 0,
             alpha = 1,
             palette = pal_arch,
             breaks = seq(0.5,18.5),
             style = "cat",
             colorNA = NULL) +
  tm_layout(legend.show = F, 
            earth.boundary = c(-179, -60, 179, 88),
            earth.boundary.color = "white", space.color = "white",
            legend.frame = F, frame = F)
ggmn

tmap_save(ggmn, here("plots/ggmn_map_gwscape.png"), dpi = 400, units = "in")

dist_dfm$wellDENS[8] / dist_dfm$wellDENS[9]