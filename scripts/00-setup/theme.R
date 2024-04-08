my_theme = theme_minimal()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))


met.pal.use = "Cross"

# pal_arch = met.brewer(name = "Cross", n = 11, type = "continuous")[c(1:3, 5:11)]
pal_arch = met.brewer(name = "Cross", n = 18, type = "continuous", dir = -1) # [c(1:3, 5:11)]
pal_arch


# 1-4
gde_blues = met.brewer(name = "Cross", n = 18, type = "continuous", dir = -1)[1:4]

# 5 - 7
large_ag = met.brewer(name = "Nattier", n = 9, type = "continuous", dir = 1)[5:7]

# 8 - 9
gw_ag = met.brewer(name = "Tam", n = 10, type = "continuous", dir = 1)[c(5,8)]

# 10 - 13
climate = met.brewer(name = "Hiroshige", n = 10, type = "continuous", dir = 1)[6:9]

# 14
minimal_all = met.brewer(name = "OKeeffe2", n = 8, type = "continuous", dir = 1)[6]

# 15 - 18
minimal_ef = met.brewer(name = "OKeeffe2", n = 8, type = "continuous", dir = 1)[2:5]

pal_arch = c(gde_blues, large_ag, gw_ag, climate, minimal_all, minimal_ef)
pal_arch