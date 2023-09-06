
# import winning first-stage SOM
prototypes = readr::read_rds(here("data/som-iter/final-kohonen-objs/som1_prototypes.rds"))
prototypes = prototypes$codes[[1]] |> as.data.frame()

# set range of second-stage som
r_min = 4
r_max = 30

# convert prototypes to matrix to feed to som function
som_input = prototypes |> as.matrix()

for (n_clust in seq(r_min, r_max, by = 1)) {
  # n_clust = 4
  message("starting SOM size ", n_clust)
  
  quality_df = data.frame(
    iter = seq(1:100),
    quant = rep(NA),
    varra = rep(NA),
    k_l  = rep(NA),
    topo = rep(NA),
    nrc = rep(n_clust)
  )
  
  for (i in 1:(quality_df$iter |> max())) {
    # i = 1

    som_iter = supersom(som_input, 
                        grid = somgrid(xdim = n_clust, ydim = 1, topo="hexagonal"), 
                        rlen = 500, 
                        alpha = c(0.05, 0.01),
                        keep.data = TRUE,
                        cores = 8)
   
    ## write out SOM data to recall if best performing SOM
    write_rds(x = som_iter,
              file = paste0(here("data/som-iter/kohonen-objs/som2"), "/som2_nclust_", n_clust, "_iter_", i, ".rds")) 
    
    som_quality = aweSOM::somQuality(som = som_iter, traindat = som_input)
    
    quality_df$quant[i] = som_quality$err.quant[1] |> as.numeric() |> round(2)
    quality_df$varra[i] = som_quality$err.varratio[1] |> as.numeric() |> round(2)
    quality_df$k_l[i] = som_quality$err.kaski[1] |> as.numeric() |> round(2)
    quality_df$topo[i] = som_quality$err.topo[1] |> as.numeric() |> round(2)
    message("... ", i, "/", n_clust)
  }
  
  write_rds(x = quality_df,
            file = paste0(here("data/som-iter/som2/som2_nclust_"), n_clust, ".rds"))
  message("SOM iterations for nclust=", n_clust, " has completed")
  
}

########################-
## Assess performance of all SOM iterations and select best performing dimension
########################-

# Import performance metrics of each SOM2 architecture size
df = list.files(here("data/som-iter/som2"), pattern = ".rds", full.names = T) 

# need to sort as 4-9 are at end of ordered list, instead of at top
df = c(df[22:27], df[1:21]) |> 
  map_dfr(readRDS) |> 
  mutate(
    rowID = seq(1, 27e2)
  ) 

# unexplained variance
df$unexv = (100 - df$varra)/100

# scale number of clusters based on rule of human perception of scale https://en.wikipedia.org/wiki/Weber%E2%80%93Fechner_law
df$sizeperception = log(df$nrc)

# plot(df$k_l ~ df$nrc)
plot(df$topo ~ df$nrc)
plot(df$unexv ~ df$nrc)
plot(df$sizeperception ~ df$nrc)

# to select these archetypes, enable explicit consideration of (1) topo error, (2) explained variance, and (3) number of clusters
df$topo_scaled = df$topo / sd(df$topo)
df$unex_scaled = df$unexv / sd(df$unexv)
df$size_scaled = df$sizeperception / sd(df$sizeperception) #

plot(df$topo_scaled ~ df$nrc)
plot(df$unex_scaled ~ df$nrc)
plot(df$size_scaled ~ df$nrc)

df$perf = minmaxnorm(df$topo_scaled + df$unex_scaled + df$size_scaled)
# df$perf = minmaxnorm(df$topo_scaled + df$unex_scaled)

plot(df$perf ~ df$nrc) 
rowno = which.min(df$perf)
winning_size = df[rowno,]
winning_size

# Like in first-stage SOM, remove outliers to improve robustness and reproducibility
df_keep = matrix(nrow = 1, ncol = ncol(df)) |>  as.data.frame()
names(df_keep) = names(df)
mad_x = 1 # set narrower band for greater reproducibility + robustness
for (i in unique(df$nrc)) {
  # i = 4
  iter_df = df |> dplyr::filter(nrc == i)
  iter_perf = iter_df$perf
  min_lim = median(iter_perf) - mad_x*mad(iter_perf)
  max_lim = median(iter_perf) + mad_x*mad(iter_perf)
  iter_df = iter_df |> dplyr::filter(perf >= min_lim) |> dplyr::filter(perf <= max_lim)
  df_keep = rbind(df_keep, iter_df)
}

# % of entries removed as outleirs
(nrow(df) - nrow(df_keep) )/ nrow(df) # 31%

plot(df_keep$perf ~ df_keep$nrc, main = "less outliers") 
rowno = which.min(df_keep$perf)
winning_size = df_keep[rowno,]
winning_size
write_rds(df_keep, file = here("data/som_2_performance.rds"))

### From the above plotting, we can see that n_clust = 10 is the best second-stage SOM size, 
### and iter = 52 is SOM iteration that produced best result
som_objs = list.files(here("data/som-iter/kohonen-objs/som2"), pattern = ".rds", full.names = T)

winning_som = readRDS(paste0(here("data/som-iter/kohonen-objs/som2"), 
                             "/som2_nclust_", winning_size$nrc, 
                             "_iter_", winning_size$iter, ".rds"))

write_rds(x = winning_som,
          file = here("data/som-iter/final-kohonen-objs/som2_archetypes.rds"))
