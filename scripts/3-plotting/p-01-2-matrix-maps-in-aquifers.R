
matrix_r = terra::rast(here("data/matrix_stack_altGDEclass.tif")) 
arch_map = terra::rast(here("data/groundwater-SYSTEM-archetypes_3x3_currentiter.tif"))

bivir_pal = c(
  "#E8E8E8", "#CEE2E2", "#CEE2E2", "#97D6D6", "#7ACFCF", "#5AC8C8", #11 to 16
  "#E0CEDC", "#CECEDC", "#B3CEDC", "#97CED6", "#7ACECF", "#5AC8C8", #21 to 26
  "#D8B4D1", "#CEB4D1", "#B3B4D1", "#97B4D1", "#7AB4CF", "#5AB4C8", #31 to 36
  "#CF9AC5", "#CE9AC5", "#B39AC5", "#979AC5", "#7A9AC5", "#5A9AC5", #41 to 46
  "#C780B9", "#C780B9", "#B380B9", "#9780B9", "#7A80B9", "#5A80B9", #51 to 56
  "#BE64AC", "#BE64AC", "#B364AC", "#9764AC", "#7A64AC", "#5A64AC") #61 to 66


whymap37 = terra::vect(here("data/input/major_aquifers_sorted.sqlite"))

for (i in 1:nrow(whymap37)) {
  # i = 22
  whymap_i = whymap37[i,]
  whymap_i$flag = 1

  matrix_aqui = terra::mask(x = matrix_r, mask = whymap_i, touches = T) |> trim()
  archty_aqui = terra::mask(x = arch_map, mask = whymap_i, touches = T) |> trim()
  archty_vect = terra::as.polygons(x = archty_aqui, aggergate = TRUE)
  
  
  for (ly in 1:4) {
    # ly = 4
    tm_obj = tm_shape(st_as_sf(whymap_i)) + tm_polygons(col = "grey")+ 
      tm_shape(raster(matrix_aqui[[ly]]), raster.warp = FALSE) + 
      tm_raster(palette = bivir_pal,
                breaks = c(
                  seq(10.5, 16.5, by = 1),
                  seq(21.5, 26.5, by = 1),
                  seq(31.5, 36.5, by = 1),
                  seq(41.5, 46.5, by = 1),
                  seq(51.5, 56.5, by = 1),
                  seq(61.5, 66.5, by = 1))) +
      tm_shape(st_as_sf(archty_vect)) +
      tm_borders(col = "grey30", lwd = 1, lty = 1) +
      tm_shape(st_as_sf(whymap_i)) +
      tm_borders(col = "black", lwd = 2) +
      tm_layout(legend.show = F, 
                earth.boundary.color = "white", space.color = "white",
                legend.frame = F, frame = F, bg.color = "transparent")
    tm_obj # preview plot
    
    if (ly == 1) {folder_name = "1_earth"}
    if (ly == 2) {folder_name = "2_eco"}
    if (ly == 3) {folder_name = "3_food"}
    if (ly == 4) {folder_name = "4_mgmt"}
     
    tmap_save(tm = tm_obj,
              filename = paste0(here("plots/matrix_maps_in_aquifers"), "/", folder_name, 
                                "/aquifer_", whymap_i$idalph, "_", whymap_i$aquifer_sy, ".png"),
              height = 10, units = "cm", dpi = 500,
              device = png, bg = "transparent", type = 'cairo')
  }

}
