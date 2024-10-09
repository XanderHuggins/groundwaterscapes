### ---------------------\\ 
# Script objective:
# Set ggplot theme and groundwaterscape colour palette 
### ---------------------\\ 

my_theme = theme_minimal()+
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))


# Generate groundwaterscape palette 

# 
#1-3
pal_1_3 = met.brewer(name = "OKeeffe2", n = 8, type = "continuous", dir = 1)[2:4]

#4-5
pal_4_5 = met.brewer(name = "Isfahan1", n = 8, type = "continuous", dir = 1)[c(1,2)]

#6-7
pal_6_7 = met.brewer(name = "Hiroshige", n = 10, type = "continuous", dir = 1)[7:8]

#8-9
pal_8_9 = met.brewer(name = "Cross", n = 18, type = "continuous", dir = -1)[c(3,2)]
# pal_8_9 = met.brewer(name = "Cassatt2", n = 10, type = "continuous", dir = 1)[c(3,5)]

#10-11
pal_10_11 = met.brewer(name = "Tam", n = 10, type = "continuous", dir = 1)[c(6,9)]

#12-13
pal_12_13 = met.brewer(name = "Nattier", n = 9, type = "continuous", dir = 1)[c(5,7)]

#14-15
pal_14_15 = met.brewer(name = "Cross", n = 18, type = "continuous", dir = -1)[1:2]

pal_arch = c(pal_1_3, pal_4_5, pal_6_7, pal_8_9, pal_10_11, pal_12_13, pal_14_15)
pal_arch

pal_arch = data.frame(
  GWscape_ID = seq(1:15),
  HEXCODE = pal_arch
)

write_csv(pal_arch, here("data/!Borealis/groundwaterscape-colour-scheme.csv"))