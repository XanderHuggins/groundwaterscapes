
# import matrix raster
matrix_r = terra::rast(here("data/matrix_stack_altGDEclass.tif")) 

# 
bivir_pal = c(
  "#E8E8E8", "#CEE2E2", "#CEE2E2", "#97D6D6", "#7ACFCF", "#5AC8C8", #11 to 16
  "#E0CEDC", "#CECEDC", "#B3CEDC", "#97CED6", "#7ACECF", "#5AC8C8", #21 to 26
  "#D8B4D1", "#CEB4D1", "#B3B4D1", "#97B4D1", "#7AB4CF", "#5AB4C8", #31 to 36
  "#CF9AC5", "#CE9AC5", "#B39AC5", "#979AC5", "#7A9AC5", "#5A9AC5", #41 to 46
  "#C780B9", "#C780B9", "#B380B9", "#9780B9", "#7A80B9", "#5A80B9", #51 to 56
  "#BE64AC", "#BE64AC", "#B364AC", "#9764AC", "#7A64AC", "#5A64AC") #61 to 66

# seed at top of each raster to ensure each ID is in plot
# seed.in = c(seq(11,16), seq(21,26), seq(31,36), seq(41,46), seq(51,56), seq(61,66))

# import outline
outline = terra::vect(here("data/input/land_mask_polygon.sqlite")) |> st_as_sf()

for (i in 1:4) {
  # i = 2
  matrix_i = matrix_r[[i]]
  # matrix_i[1,1:36] = seed.in
  
  matrix_i_projected = terra::project(x = matrix_i, y = "+proj=robin", method = "near")
  
  map =  
    tm_shape(outline, projection = "+proj=robin") +
    tm_fill(col = "grey", lwd = 0, border.col = NA) +
    tm_shape(matrix_i_projected) +
    tm_raster(palette = bivir_pal,
              breaks = c(
                seq(10.5, 16.5, by = 1),
                seq(21.5, 26.5, by = 1),
                seq(31.5, 36.5, by = 1),
                seq(41.5, 46.5, by = 1),
                seq(51.5, 56.5, by = 1),
                seq(61.5, 66.5, by = 1))) +
    # tm_raster(palette = bivir_pal,
    #           style = "cat",
    #           colorNA = NULL) +
    tm_shape(outline) + 
    tm_borders(col = "black", lwd = 1) +
    tm_layout(legend.show = F, legend.frame = F, frame = F)
  map
  
  names_out = c('earth', 'eco', 'food', 'mgmt')
 
  tmap_save(map, paste0(here("plots/global-maps/matrix_map_"), names_out[i], ".pdf"), dpi = 400, units = "in")  
  
}


# create barplot of area distribution of individual matrix values
matrix_r = c(terra::rast(here("data/matrix_stack_altGDEclass.tif"))[[5]], rast(WGS84_areaRaster(5/60)))
names(matrix_r) = c('id', 'area')

matrix_df = matrix_r |> 
  as.data.frame() |> 
  drop_na() |> 
  group_by(id) |> 
  summarise(
    area = sum(area, na.rm = T)
  ) |> 
  mutate(
    area_f = area/sum(area)
  )

matrix_df = matrix_df[order(-matrix_df$area),]
head(matrix_df)
matrix_df$rank = seq(1, nrow(matrix_df))
nrow(matrix_df)

matrix_df$cumsum = cumsum(matrix_df$area_f)

ggplot() +
  # explained variance
  # geom_line(aes(y = rank_s), col = "#EF440C", linewidth = 2) +
  geom_bar(data = matrix_df[1:1000,], stat = "identity",  aes(x= rank, y = 100*area_f), col = "black") +
  geom_line(data = matrix_df[1:1000,], aes(x= rank, y = cumsum*1.5), col = "red", linewidth = 1.5) +
  # geom_point(data = best_at_size, aes(x= nrc, y = perf), col = "black", size = 5) +
  coord_cartesian(ylim=c(0, 1.5), xlim = c(0.5, 1000.5), expand = 0, clip = "off") +
  # scale_x_continuous(breaks = seq(4, 30, by = 2)) +
  scale_y_continuous(breaks = seq(0, 2, by = 0.5)) +
  my_theme + 
  theme(axis.line = element_line(size = 1), 
        panel.grid.major = element_line(),
        axis.text = element_text(size=13),
        axis.title = element_blank()) 

ggsave(plot = last_plot(),
       filename = here("plots/marix_area_distribution.png"),
       height = 10,
       width = 18,
       units = "cm",
       dpi = 400)
