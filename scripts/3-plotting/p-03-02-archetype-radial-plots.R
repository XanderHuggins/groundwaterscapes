

# import archetype codebooks and plot radial plots

# arche_codes = readr::read_rds(here("data/som_cluster_codebook.rds"))

archetypes = read_rds(paste0(here("data/som_files/som_selections/som2_selection.rds")))
archetypes = archetypes$codes[[1]] |> as_tibble()                      
archetypes$udw = -archetypes$udw # so that more inequality appears as bigger risk
# archetypes$gws = -archetypes$gws # so that groundwater depletion appears as bigger risk
# archetypes$conspri = -archetypes$conspri # because lower values have greater priority
archetypes = round(archetypes + 2, 2) # 2 sds shifts up for radial plotting

nclust = archetypes |> nrow()
archetypes$ID = as.numeric(1:nclust)

# ### ----- sidebar -- identify median value across archetype values 
# arch_ras = rast(here("data/groundwater-SYSTEM-archetypes_3x3_currentiter.tif"))
# dat_stack = rast(here("data/input-data-stack-norm.tif"))
# 
# base_radial = matrix(nrow = 1, ncol = 9) |> as_tibble()
# names(base_radial) = names(archetypes)[1:9]
# 
# for (i in 1:ncol(base_radial)) {
#   # i = 1
#   temp_extent = arch_ras[[i]]
#   temp_ras = dat_stack[[i]]
#   temp_ras[temp_extent != i] = NA
#   
#   if (i %in% c(8)) {
#     temp_ras = -temp_ras
#   }
#   base_radial[1,i] = temp_ras[] |> min(na.rm = T)
# }
# 
base_radial = matrix(nrow = 1, ncol = 9) |> as_tibble()
base_radial[1,] = rep(2)

for (i in 1:nclust) { 
  # i = 1
  
  for (j in c(T,F)) {
    radial_plot(dataframe.in = archetypes,
                cluster.no = i,
                clust_length = nclust,
                colour.in = pal_arch[i],
                labs = j,
                name.cust = "risk",
                min.bot = 0.5,
                max.top = 3.5,
                base.rad.in = base_radial) 
  }
  
}

# # and now for the system archetypes (from WRR paper in review)
# archetypes = read_rds("C:/Users/xande/Documents/2.scripts/gcs-archetypes/data/som-iter/final-kohonen-objs/som2_archetypes_reclassed.rds")
# 
# arche_codes = archetypes$codes[[1]] |> as_tibble()
# arche_codes = round(arche_codes + 2, 2) 
# # arche_codes[arche_codes>4] = 4
# 
# nclust = arche_codes |> nrow()
# arche_codes$ID = as.numeric(nclust:1)
# arche_codes = arche_codes[order(arche_codes$ID),]
# 
# pal_sys = met.brewer(name = "Cross", n = 11, type = "continuous")[c(1:3, 5:11)]
# 
# for (i in 1:nclust) { 
#   # i = 8
#   for (j in c(T,F)) {
#     radial_plot(dataframe.in = arche_codes,
#                 cluster.no = i,
#                 clust_length = nclust,
#                 colour.in = pal_sys[i],
#                 labs = j,
#                 name.cust = "sys",
#                 min.bot = 0.5,
#                 max.top = 3.5) 
#   }
#   
#   # radial_plot(dataframe.in = arche_codes,
#   #             cluster.no = i,
#   #             clust_length = nclust,
#   #             colour.in = pal_sys[i],
#   #             labs = T,
#   #             name.cust = "sys") 
#   message(i)
# }
