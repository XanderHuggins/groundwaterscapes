radial_plot = function(dataframe.in, cluster.no, clust_length, colour.in, tier) {
  
  library(ggradar)
  # cluster.no = 1
  # dataframe.in = arche_codes
  # clust_length = arche_codes$ptypeID |> unique() |> length()
  # colour.in = metbrew_pal(clust_length, tier = 1)[cluster.no]

  # dataframe.in = arche_codes
  # cluster.no = 1
  # i = 1
  # clust_length = nclust
  # tier = 1
  # colour.in = metbrew_pal(nclust, tier = tier)[i]
  
  # radial_plot(dataframe.in = arche_codes,
  #             cluster.no = i,
  #             clust_length = nclust,
  #             colour.in = metbrew_pal(nclust, tier = 1)[i],
  #             tier = 1) 
  
  radar_data = dataframe.in |> 
    as_tibble() |> 
    filter(ptypeID == cluster.no) |> 
    mutate(ptypeID = as.numeric(ptypeID)) |> 
    # dplyr::select(-clust) |>
    set_colnames(c('Cluster', 'wtr', 'por', 'gde_t', 'gde_a', 'fsz', 'gwi', 'mgmt', 'udw'))
  
  mean_values = dataframe.in |> 
    mutate(ptypeID = as.numeric(ptypeID)) |> 
    colMeans(na.rm = TRUE)
  
  mean_values[1] = 0
  
  radar_data = rbind(mean_values, radar_data)
  
  ggradar(radar_data,
          legend.title = "",
          legend.position = "none",
          grid.label.size = 0,
          gridline.mid.colour = "grey",
          x.centre.range = 0.001,
          background.circle.colour = "transparent",
          background.circle.transparency = 0,
          grid.line.width = 1,
          gridline.min.linetype = "solid",
          gridline.mid.linetype = "blank",
          gridline.max.linetype = "solid",
          group.colours = c("grey10", colour.in),
          group.line.width = c(rep(NA, ncol(dataframe.in)), rep(3, ncol(dataframe.in))),
          group.point.size = 0,
          axis.label.size =  5, 
          fill = T,
          fill.alpha = 0.5,
          grid.min = 0,
          grid.mid = 2,
          grid.max = 4)
  
  if (tier == 1) {ggsave(plot = last_plot(), paste0(here("plots/radial_"), cluster.no, ".png"),
                         dpi = 300, width = 5, height = 5, units = "in")
                 }
  
  if (tier == 2) {ggsave(plot = last_plot(), paste0(here("plots/tier2_radial_"), cluster.no, ".png"),
                         dpi = 300, width = 5, height = 5, units = "in")
                 }
  
}
