

##############################-
## Import data and set seed ##
input_df = readr::read_rds(here("data/ds_df_sampled_for_SOM_1.rds")) |> as_tibble()

######################################################################
## Set parameters for iterative SOM mapping of entire data space    ##
######################################################################

# parameters based on Delgado et al. 2017
n_patterns = input_df$all |> unique() |> length() # 38,880
k_max = n_patterns ^ (0.4)  # 68
P_min = 2*k_max # 137
P_max = 0.15 * n_patterns # 5832 
r_min = sqrt(P_min)  # 11.707
r_max = sqrt(P_max) # 76.368
no_cores = 8

# overwrite parameters 
r_min = 10
r_max = 30

# create vector of all SOM size parameters to try:
som_sizes = c(seq(10,30,by=2), 35, 40, 45, 50, 55, 65, 75)

###################-
# Loop through first-stage SOMs 
###################-

som_input = input_df |> dplyr::select(!c(id, all)) |> as.matrix()

# done up to nrc = 30
for (nrc in som_sizes[8:11]) { #r_min
  
  # nrc = 10
  message("starting SOM size ", nrc)
  
  quality_df = data.frame(
    iter = seq(1:20),
    quant = rep(NA),
    varra = rep(NA),
    k_l  = rep(NA),
    topo = rep(NA),
    nrc = rep(nrc)
  )
  
  for (i in 1:(quality_df$iter |> max())) {
    # i = 1
    st = Sys.time()
    som_iter = supersom(som_input, 
                        grid = somgrid(xdim = nrc, ydim = nrc, topo="hexagonal"), 
                        rlen = 500, 
                        alpha = c(0.05, 0.01),
                        keep.data = TRUE,
                        cores = no_cores,
                        mode = "pbatch")
    et = Sys.time()
    message(et-st)
    
    ## write out SOM data to recall if best performing SOM
    write_rds(x = som_iter,
              file = paste0(here("data/som-iter/kohonen-objs"), "/som1_nrc_", nrc, "_iter_", i, ".rds")) 
    
    som_quality = aweSOM::somQuality(som = som_iter, traindat = som_input)
    
    quality_df$quant[i] = som_quality$err.quant[1] |> as.numeric() |> round(2)
    quality_df$varra[i] = som_quality$err.varratio[1] |> as.numeric() |> round(2)
    quality_df$k_l[i] = som_quality$err.kaski[1] |> as.numeric() |> round(2)
    quality_df$topo[i] = som_quality$err.topo[1] |> as.numeric() |> round(2)
    message("... iteration ", i, " is done for given SOM size")
  }
  
  write_rds(x = quality_df,
            file = paste0(here("data/som-iter/som1_nrc_"), nrc, ".rds"))
  message("SOM iterations for dims=", nrc, " has completed")
  
}

########################-
## Assess performance of all SOM iterations and select best performing dimension
########################-

# Import performance metrics of each SOM1 architecture size
df = list.files(here("data/som-iter"), pattern = ".rds", full.names = T) 

# need to sort 
df = df |> # c(df[22:27], df[1:21]) |>
  map_dfr(readRDS) |> 
  dplyr::filter(nrc <= 30)
df$rowID = seq(1, nrow(df))

# exploratory plotting of results
df |> group_by(nrc) |> summarise(k_l = median(k_l)) |> ggplot() + geom_point(aes(x = nrc, y= k_l))
df |> group_by(nrc) |> summarise(topo = median(topo)) |> ggplot() + geom_point(aes(x = nrc, y= topo))
plot(df$k_l ~ df$nrc)
plot(df$topo ~ df$nrc)
df$nnode = df$nrc^2
plot(df$varra ~ df$nrc)
df |> group_by(nnode) |> summarise(topo = median(topo)) |> plot(topo ~ nnode)
df |> group_by(nnode) |> summarise(varra = median(varra)) |> plot(varra ~ nnode)
df |> group_by(nnode) |> summarise(k_l = median(k_l)) |> plot(k_l ~ nnode)

# unexplained variance
df$unexv = (100 - df$varra)/100

df$topo_scaled = df$topo / sd(df$topo)
df$unex_scaled = df$unexv / sd(df$unexv)
df$perf = minmaxnorm(df$topo_scaled + df$unex_scaled)
plot(df$perf ~ df$nrc, main = "all")
rowno = which.min(df$perf)
winning_size = df[rowno,]
winning_size 

# Can observe considerable variability in these performance results 
# (i.e. SOM size 20x20 has both the 2nd best and very worst performing SOM of all 220 trained SOMs)
# So remove outliers to improve robustness and reproducibility

df_keep = matrix(nrow = 1, ncol = ncol(df)) |>  as.data.frame()
names(df_keep) = names(df)

mad_x = 1 # set narrower band for greater reproducibility + robustness
for (i in unique(df$nrc)) {
  
  # i = 12
  iter_df = df |> dplyr::filter(nrc == i)
  iter_perf = iter_df$perf
  min_lim = median(iter_perf) - mad_x*mad(iter_perf)
  max_lim = median(iter_perf) + mad_x*mad(iter_perf)
  
  iter_df = iter_df |> dplyr::filter(perf >= min_lim) |> dplyr::filter(perf <= max_lim)
  df_keep = rbind(df_keep, iter_df)
}

plot(df_keep$perf ~ df_keep$nrc, main = "less outliers") 
rowno = which.min(df_keep$perf)
winning_size = df_keep[rowno,]
winning_size
write_rds(df_keep, file = here("data/som_1_performance.rds"))

### From the above plotting, we can see that nrc = 26 is the best first-stage SOM size, 
### and iter = 17 is SOM iteration that produced best result after filtering out outliers (n = 66/220)
som_objs = list.files(here("data/som-iter/kohonen-objs"), pattern = ".rds", full.names = T)

winning_som = readRDS(paste0(here("data/som-iter/kohonen-objs"), 
                             "/som1_nrc_", winning_size$nrc, 
                             "_iter_", winning_size$iter, ".rds"))

write_rds(x = winning_som,
          file = here("data/som-iter/final-kohonen-objs/som1_prototypes.rds"))


##### old stuff below... 
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