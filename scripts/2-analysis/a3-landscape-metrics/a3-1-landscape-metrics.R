##################################################### --
## Calculate landscape metrics for the 37 whymap aquifers
##################################################### --

# split this script into the individual analyses and print out results below each

# Import raster of archetypes, area raster, and polygons of the whymap aquifers
archs = terra::rast(here("data/MAP_archetypes.tif"))
whymap = terra::vect(here("data/input/major_aquifers_sorted.sqlite"))
whymap_r = terra::rasterize(x = whymap, y = archs, field = "idalph", touches = TRUE) # rasterize ALPHebetized whymap order
area = terra::rast(here("data/ggrid_5arcmin.tif"))

# get cell adjacencies
adj_df = landscapemetrics::get_adjacencies(landscape = archs, neighbourhood = 4)

for (i in 1:nrow(adj_df$layer_1)) {
  # i = 1
  temp_df = data.frame(id = seq(1:nrow(adj_df$layer_1)), count = adj_df$layer_1[,i])
  temp_df$relc = temp_df$count / max(temp_df$count)
  
  ggplot(data = temp_df) +
    geom_bar(aes(x = id, y = relc, fill = as.factor(id)), stat = "identity") +
    scale_fill_manual(values = pal_arch) +
    theme_void() +
    coord_cartesian(expand = c(0), ylim=c(0, 1)) +
    theme(legend.position = "none")
  
  ggsave(plot = last_plot(), filename = paste0(here("plots/adjacencies-for-arch-"), i, ".png"),
         height = 10, width = 15, units = "cm", dpi = 400)
  
}

adj_df = landscapemetrics::get_adjacencies(landscape = archs, neighbourhood = 4)
adj_df = adj_df$layer_1 
for (i in 1:nrow(adj_df)) {
  adj_df[i,] = adj_df[i,] / sum(adj_df[i,])
}
round(adj_df, 3)

###############
# All landscape metrics
###############

# data frame to hold results
res_df = data.frame(
  id = whymap$idalph,
  sidi = rep(NA),
  siei = rep(NA),
  cntg = rep(NA),
  cohn = rep(NA),
  entr = rep(NA),
  rmif = rep(NA)
)

for (i in 1:nrow(res_df)) {
  # i = 1
  temp_arch = archs
  temp_arch[whymap_r != res_df$id[i]] = NA
  temp_arch[is.na(whymap_r)] = NA
  
  res_df$sidi[i] = lsm_l_sidi(landscape = temp_arch, directions = 8) |> pull(value)
  res_df$siei[i] = lsm_l_siei(landscape = temp_arch, directions = 8) |> pull(value)
  res_df$cntg[i] = lsm_l_contag(landscape = temp_arch) |> pull(value)
  res_df$cohn[i] = lsm_l_cohesion(landscape = temp_arch, directions = 8) |> pull(value)
  res_df$entr[i] = lsm_l_ent(landscape = temp_arch, neighbourhood = 4, base = "log2") |> pull(value)
  res_df$rmif[i] = lsm_l_relmutinf(landscape = temp_arch, neighbourhood = 4, ordered = FALSE, base = "log2") |> pull(value)
  
  message("aquifer ", i, " done")
  
}

ggplot(data = res_df) + geom_point(aes(x = siei, y = cntg/100), shape = 21, size = 8, fill = "black", alpha = 0.8) +
  geom_text(aes(x = siei, y = cntg/100, label = id), colour = "white", size = 4)+
  # coord_cartesian(xlim=c(0, 1), ylim = c(0.25,1), expand = 0) +
  my_theme + 
  # scale_x_continuous(breaks = seq(1, 11, by = 1)) +
  # coord_cartesian(xlim = c(1, 11), ylim = c(0, 0.9)) +
  theme(axis.line = element_line(size = 1), 
        panel.grid.major = element_line(),
        axis.text = element_text(size=16),
        axis.title = element_blank()) 
ggsave(plot = last_plot(), filename = here("plots/aquifer-evenness-contagion.png"),
       height = 10, width = 15, units = "cm", dpi = 400)

# mutual info 
ggplot(data = res_df) + geom_point(aes(x = entr, y = rmif), shape = 21, size = 8, fill = "black", alpha = 0.8) +
  geom_text(aes(x = entr, y = rmif, label = id), colour = "white", size = 4)+
  # coord_cartesian(xlim=c(0, 1), ylim = c(0.25,1), expand = 0) +
  my_theme + 
  # scale_x_continuous(breaks = seq(1, 11, by = 1)) +
  # coord_cartesian(xlim = c(1, 11), ylim = c(0, 0.9)) +
  theme(axis.line = element_line(size = 1), 
        panel.grid.major = element_line(),
        axis.text = element_text(size=16),
        axis.title = element_blank()) 
ggsave(plot = last_plot(), filename = here("plots/aquifer-mutual-info.png"),
       height = 10, width = 15, units = "cm", dpi = 400)


# plot area distribution of each archetype within each whymap aquifer
stack_df = c(archs, area, whymap_r) |> 
  as.data.frame() |> 
  set_colnames(c('arch', 'area', 'aquifer')) |> 
  drop_na() 

summary_df = data.frame(aquiferID = whymap$idalph, 
                        arch_n = rep(NA),
                        archs = rep(NA))

base_area_df = data.frame(arch = seq(1, 10))

for (i in 1:nrow(res_df)) {
  # i = 20
  
  area_temp_df = stack_df |> 
    filter(aquifer == i) |> 
    group_by(arch) |> 
    summarise(
      area = sum(area, na.rm = T)
    )
  
  temp_base_area = merge(x = base_area_df, y = area_temp_df, by = "arch", all = T)
  temp_base_area$area[is.na(temp_base_area$area)] = 0
  
  ggplot(data = temp_base_area, aes(y = area, x = 1, fill = as.factor(arch))) +
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(values = pal_arch) +
    theme_void() +
    coord_cartesian(expand = c(0)) +
    theme(legend.position = "none")
  
  ggsave(plot = last_plot(), filename = paste0(here("plots/arch-DISTR-in-aq-"), i, ".png"),
         height = 10, width = 3, units = "cm", dpi = 400)
  
  summary_df$arch_n[i] = area_temp_df |> filter(area > 0.01*sum(area_temp_df$area, na.rm = T)) |> nrow()
  summary_df$archs[i] = area_temp_df  |> filter(area > 0.001*sum(area_temp_df$area, na.rm = T)) |> nrow()
}

whymap_df = merge(x = whymap, y = summary_df, by.x = "idalph", by.y = "aquiferID") |> as.data.frame() |> 
  dplyr::select(c('idalph', 'aquifer_sy', 'arch_n', 'archs'))
whymap_df
