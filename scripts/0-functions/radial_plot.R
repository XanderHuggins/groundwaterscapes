radial_plot = function(dataframe.in, cluster.no, clust_length, colour.in, labs = TRUE, 
                       name.cust, min.bot, max.top, base.rad.in) {
  
  library(ggradar)
  
  # i = 1
  # dataframe.in = archetypes,
  # cluster.no = i,
  # clust_length = nclust,
  # colour.in = pal_arch[i],
  # labs = T,
  # name.cust = "risk",
  # min.bot = 0.5,
  # max.top = 3.5,
  # base.rad.in = base_radial
  
  
  radar_data = dataframe.in[i,] |> 
    as_tibble()
  
  mean_values = base.rad.in
  names(mean_values) = names(dataframe.in)[1:9]
  mean_values$ID = 0
  
  radar_data = rbind(mean_values, radar_data)
  radar_data = radar_data[,c(9, 1:8)]
  
  radar_id = radar_data[,1]
  radar_data = radar_data[,2:9]
  
  radar_data[radar_data < min.bot] = min.bot
  radar_data[radar_data > max.top] = max.top
  
  radar_data = cbind(radar_id, radar_data)
  
  if (labs == FALSE) {labels = rep('', ncol(radar_data)-1)}
  if (labs == TRUE) {labels = colnames(radar_data)[-1]}
  
  plt = ggradar(radar_data,
                legend.title = "",
                legend.position = "none",
                grid.label.size = 0,
                axis.labels = labels,
                gridline.mid.colour = "grey",
                x.centre.range = 0.001,
                background.circle.colour = "grey90",
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
                grid.min = min.bot,
                grid.mid = mean(c(min.bot, max.top)),
                grid.max = max.top)
  # plt
  plt = plt +
    theme(plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white")) # #F2F2F2
  # plt
  
  if (labs == TRUE) {
    ggsave(plot = plt, paste0(here("plots/radial_L_"), name.cust, "_", cluster.no, ".png"),
           dpi = 300, width = 5, height = 5, units = "in", bg = "transparent")  
  }
  if (labs == FALSE) {
    ggsave(plot = plt, paste0(here("plots/radial_B_"), name.cust, "_", cluster.no, ".png"),
           dpi = 300, width = 5, height = 5, units = "in", bg = "transparent")  
  }
  
}
