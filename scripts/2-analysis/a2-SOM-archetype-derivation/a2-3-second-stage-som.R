## --------------------------- \
# import best-performing first-stage SOM and extract codebook vectors
prototypes = readr::read_rds(here("data/som-iter/final-kohonen-objs/som1_prototypes.rds"))
prototypes = prototypes$codes[[1]] |> as.data.frame()

# set range of second-stage SOM following range suggested by Eisenack et al. 2021
r_min = 4
r_max = 30

# convert prototypes to matrix to feed to som function
som_input = prototypes |> as.matrix()

## --------------------------- \
# Loop through second-stage SOM iterations

for (n_clust in seq(r_min, r_max, by = 1)) {
  # n_clust = 4
  message("starting SOM size ", n_clust)
  
  # data frame to store SOM quality metrics for each lattice size
  quality_df = data.frame(
    iter = seq(1:100), # 100 iterations per SOM lattice
    quant = rep(NA),   # all other parts of this df as same as in first-stage SOM 
    varra = rep(NA),
    k_l  = rep(NA),
    topo = rep(NA),
    nrc = rep(n_clust)
  )
  
  # loop through each SOM iteration per lattice size
  for (i in 1:(quality_df$iter |> max())) {
    # i = 1
    
    # create the SOM 
    som_iter = supersom(som_input, 
                        grid = somgrid(xdim = n_clust, ydim = 1, topo="hexagonal"), 
                        rlen = 500, 
                        alpha = c(0.05, 0.01),
                        keep.data = TRUE,
                        cores = 8)
   
    ## write out SOM data to recall in case the iteration is thebest performing SOM
    write_rds(x = som_iter,
              file = paste0(here("data/som-iter/kohonen-objs/som2"), "/som2_nclust_", n_clust, "_iter_", i, ".rds")) 
    
    # calculate SOM quality metrics
    som_quality = aweSOM::somQuality(som = som_iter, traindat = som_input)
    
    # assign SOM quality metrics to data frame  
    quality_df$quant[i] = som_quality$err.quant[1] |> as.numeric() |> round(2)
    quality_df$varra[i] = som_quality$err.varratio[1] |> as.numeric() |> round(2)
    quality_df$k_l[i] = som_quality$err.kaski[1] |> as.numeric() |> round(2)
    quality_df$topo[i] = som_quality$err.topo[1] |> as.numeric() |> round(2)
    message("... ", i, "/", n_clust)
  }
  
  # write the full SOM quality data frame to file for the SOM lattice size
  write_rds(x = quality_df,
            file = paste0(here("data/som-iter/som2/som2_nclust_"), n_clust, ".rds"))
  message("SOM iterations for nclust=", n_clust, " has completed")
  
}

## --------------------------- \
## Assess performance of all SOM iterations and select best performing dimension

# Import performance metrics of each SOM2 architecture size
df = list.files(here("data/som-iter/som2"), pattern = ".rds", full.names = T) 

df = c(df[22:27], df[1:21]) |> # need to sort in this way as nrc = 4-9 are at end of file list but need to be in sequantial order for rowID to correspond to correct iteration
  map_dfr(readRDS) |> 
  mutate(
    rowID = seq(1, 27e2)
  ) 

# convert explained variance to unexplained variance
df$unexv = (100 - df$varra)/100

# add performance metric based on number of clusters (fewer clusters is better for Archetyping)
# log-scale performance metric based on https://doi.org/10.1111/j.1740-9713.2013.00636.x
df$sizeperception = log(df$nrc)

## exploratory plotting of results (commented-out)
# plot(df$k_l ~ df$nrc)
# plot(df$topo ~ df$nrc)
# plot(df$unexv ~ df$nrc)
# plot(df$sizeperception ~ df$nrc)

## --------------------------- \
# scale performance metrics so each has unit variance (equal contribution to additive performance metric)
df$topo_scaled = df$topo / sd(df$topo)
df$unex_scaled = df$unexv / sd(df$unexv)
df$size_scaled = df$sizeperception / sd(df$sizeperception) 

## --------------------------- \
# calculate performance metric of second-stage SOM 
df$perf = minmaxnorm(df$topo_scaled + df$unex_scaled + df$size_scaled)

# plot performance outcomes of all first-stage SOM iterations
plot(df$perf ~ df$nrc, main = "all") 
rowno = which.min(df$perf)
winning_size = df[rowno,]
winning_size
# like in first-stage SOM, remove outliers to improve robustness and reproducibility

## --------------------------- \
# initialize a data frame to hold iterations that are identified as NOT outliers 
df_keep = matrix(nrow = 1, ncol = ncol(df)) |>  as.data.frame()
names(df_keep) = names(df)
mad_x = 1 

for (i in unique(df$nrc)) {
  # i = 4
  iter_df = df |> dplyr::filter(nrc == i)
  iter_perf = iter_df$perf
  min_lim = median(iter_perf) - mad_x*mad(iter_perf) # lower limit 
  max_lim = median(iter_perf) + mad_x*mad(iter_perf) # upper limit
  
  iter_df = iter_df |> dplyr::filter(perf >= min_lim) |> dplyr::filter(perf <= max_lim) # keep if within limits
  df_keep = rbind(df_keep, iter_df) # bind non-outlier iterations
}
write_rds(df_keep, file = here("data/som_2_performance.rds"))


# plot iteration performances that are retained after outlier removal
plot(df_keep$perf ~ df_keep$nrc, main = "less outliers") 

# determine best-performing SOM among non-outlier iterations
rowno = which.min(df_keep$perf)
winning_size = df_keep[rowno,]
winning_size # this is best-performing SOM from first-stage 

### we see that iter 52 of nrc = 10 is the best second-stage SOM size, so import below to rewrite as the second-stage SOM results
som_objs = list.files(here("data/som-iter/kohonen-objs/som2"), pattern = ".rds", full.names = T)

winning_som = readRDS(paste0(here("data/som-iter/kohonen-objs/som2"), 
                             "/som2_nclust_", winning_size$nrc, 
                             "_iter_", winning_size$iter, ".rds"))

write_rds(x = winning_som,
          file = here("data/som-iter/final-kohonen-objs/som2_archetypes.rds"))