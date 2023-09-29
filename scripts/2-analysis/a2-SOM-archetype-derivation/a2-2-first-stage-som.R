## --------------------------- \
## Import data
input_df = readr::read_rds(here("data/ds_df_sampled_for_SOM_1.rds")) |> as_tibble()

## --------------------------- \
## Set parameters for iterative SOM mapping of entire data space

# parameters based on Delgado et al. 2017
n_patterns = input_df$all |> unique() |> length() # 38,880
k_max = n_patterns ^ (0.4)  # 68.53...
P_min = 2*k_max # 137.06...
P_max = 0.15 * n_patterns # 5832 
r_min = sqrt(P_min)  # 11.71...
r_max = sqrt(P_max) # 76.37...
no_cores = 8

# overwrite r_min and r_max parameters based on justification provided in SI 
r_min = 10
r_max = 30

# create vector of all SOM size parameters to try:
som_sizes = seq(r_min, r_max,by=2)

## --------------------------- \
# Loop through first-stage SOM iterations 

som_input = input_df |> dplyr::select(!c(id, all)) |> as.matrix()

# done up to nrc = 30
for (nrc in som_sizes) {
  
  # nrc = 10 # to test individual run works
  message("starting SOM size ", nrc)
  
  # data frame to store SOM quality metrics for each lattice size
  quality_df = data.frame(
    iter = seq(1:20), # 20 iterations per SOM lattice size
    quant = rep(NA), # quantization error 
    varra = rep(NA), # explained variance
    k_l  = rep(NA),  # Kaski-Lagus error
    topo = rep(NA),  # topographic error
    nrc = rep(nrc)   # number of rows and columns (the same as square lattice)
  )
  
  # loop through each SOM iteration per lattice size
  for (i in 1:(quality_df$iter |> max())) {
    # i = 1
    st = Sys.time()
    
    # create the SOM
    som_iter = supersom(som_input, 
                        grid = somgrid(xdim = nrc, ydim = nrc, topo="hexagonal"), 
                        rlen = 500, 
                        alpha = c(0.05, 0.01),
                        keep.data = TRUE,
                        cores = no_cores,
                        mode = "pbatch")
    et = Sys.time()
    message(et-st)
    
    ## write out SOM data to recall in case the iteration is best performing SOM
    write_rds(x = som_iter,
              file = paste0(here("data/som-iter/kohonen-objs"), "/som1_nrc_", nrc, "_iter_", i, ".rds")) 
    
    # calculate SOM quality metrics
    som_quality = aweSOM::somQuality(som = som_iter, traindat = som_input)
    
    # assign SOM quality metrics to data frame  
    quality_df$quant[i] = som_quality$err.quant[1] |> as.numeric() |> round(2)
    quality_df$varra[i] = som_quality$err.varratio[1] |> as.numeric() |> round(2)
    quality_df$k_l[i] = som_quality$err.kaski[1] |> as.numeric() |> round(2)
    quality_df$topo[i] = som_quality$err.topo[1] |> as.numeric() |> round(2)
    message("... iteration ", i, " is done for given SOM size")
  }
  
  # write the full SOM quality data frame to file for the SOM lattice size  
  write_rds(x = quality_df,
            file = paste0(here("data/som-iter/som1_nrc_"), nrc, ".rds"))
  message("SOM iterations for dims=", nrc, " has completed")
  
}

## --------------------------- \
## Assess performance of all SOM iterations and select best performing dimension

# Import performance metrics of each SOM1 architecture size
df = list.files(here("data/som-iter"), pattern = ".rds", full.names = T) 

# read and bind the SOM quality data 
df = df |> 
  map_dfr(readRDS) |> 
  dplyr::filter(nrc <= 30) # need to filter here as I developed some larger SOMs out of curiosity  
df$rowID = seq(1, nrow(df))

# convert explained variance to unexplained variance
df$unexv = (100 - df$varra)/100

## exploratory plotting of results (commented-out)
# # kaski-lagus error
# plot(df$k_l ~ df$nrc) # all iters
# df |> group_by(nrc) |> summarise(k_l = median(k_l)) |> ggplot() + geom_point(aes(x = nrc, y= k_l)) # median per size
# 
# # topo error 
# plot(df$topo ~ df$nrc) # all
# df |> group_by(nrc) |> summarise(topo = median(topo)) |> ggplot() + geom_point(aes(x = nrc, y= topo)) # median per size
# df$nnode = df$nrc^2
# plot(df$varra ~ df$nrc)
# df |> group_by(nnode) |> summarise(topo = median(topo)) |> plot(topo ~ nnode)
# df |> group_by(nnode) |> summarise(varra = median(varra)) |> plot(varra ~ nnode)
# df |> group_by(nnode) |> summarise(k_l = median(k_l)) |> plot(k_l ~ nnode)

## --------------------------- \
# scale performance metrics by variance so each metric has same sd (sd = 1)
df$topo_scaled = df$topo / sd(df$topo)
df$unex_scaled = df$unexv / sd(df$unexv)

## --------------------------- \
# calculate performance metric of first-stage SOM 
df$perf = minmaxnorm(df$topo_scaled + df$unex_scaled)

# plot performance outcomes of all first-stage SOM iterations
plot(df$perf ~ df$nrc, main = "all")
rowno = which.min(df$perf)
winning_size = df[rowno,]
winning_size 
# Can observe considerable variability in these performance results 
# (i.e. SOM size 20x20 has both the best performing SOM  BUT also the greatest variation in SOM performance across all sizes)
# So, need to remove outliers to improve robustness so this first-stage SOM reflects general performance trends when looking at all iterations 

## --------------------------- \
# initialize a data frame to hold iterations that are identified as NOT outliers 
df_keep = matrix(nrow = 1, ncol = ncol(df)) |>  as.data.frame()
names(df_keep) = names(df)

# set median absolute deviation factor to detect outliers +/- this MAD around the median per SOM size
mad_x = 1 # set narrower band for high robustness (i.e., consistent determination of best-performing SOM size between script re-runs)

for (i in unique(df$nrc)) {
  
  # i = 12
  iter_df = df |> dplyr::filter(nrc == i)
  iter_perf = iter_df$perf
  min_lim = median(iter_perf) - mad_x*mad(iter_perf) # identify lower limit of non-outliers  
  max_lim = median(iter_perf) + mad_x*mad(iter_perf) # identify upper limit of non-outliers
  
  iter_df = iter_df |> dplyr::filter(perf >= min_lim) |> dplyr::filter(perf <= max_lim) # keep only iterations within these limits
  df_keep = rbind(df_keep, iter_df) # bind non-outlier iterations
}
write_rds(df_keep, file = here("data/som_1_performance.rds")) # write this outlier-removed df to file

# plot all rows that are retained after outlier removal
plot(df_keep$perf ~ df_keep$nrc, main = "less outliers") 

# determine best-performing SOM among non-outlier iterations
rowno = which.min(df_keep$perf)
winning_size = df_keep[rowno,]
winning_size # this is best-performing SOM from first-stage 

### we see that iter 17 of nrc = 26 is the best first-stage SOM size, so import below to rewrite as the first-stage SOM results
som_objs = list.files(here("data/som-iter/kohonen-objs"), pattern = ".rds", full.names = T)

winning_som = readRDS(paste0(here("data/som-iter/kohonen-objs"), 
                             "/som1_nrc_", winning_size$nrc, 
                             "_iter_", winning_size$iter, ".rds"))
 
write_rds(x = winning_som,
          file = here("data/som-iter/final-kohonen-objs/som1_prototypes.rds"))


## --------------------------- \
## old scripts below -- ignore

# # remove best-performing iter per size for reproducibility
# for (i in 1:nrow(df_win)) {
#   # i = 2
#   df_temp = df |> dplyr::filter(nrc == df_win$nrc[i])
#   rowno = which.min(df_temp$perf)
#   winning_iter = df_temp[rowno,]
#   df$iter[winning_iter$rowID] = NA
#   message(winning_iter$rowID)
# }
# df = df[complete.cases(df$iter),]