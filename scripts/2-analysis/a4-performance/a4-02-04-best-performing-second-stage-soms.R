
# # Import the original second-stage SOMs
df = list.files(here("data/som-iter/som2"), pattern = ".rds", full.names = T)

# need to sort as 4-9 are at end of ordered list, instead of at top
df = c(df[22:27], df[1:21]) |>
  map_dfr(readRDS) |>
  mutate(
    rowID = seq(1, 27e2),
    unexv = (100 - varra)/100,
    sizeperception = log(nrc)
  ) |>
  dplyr::filter(nrc != 10)

# start loop of the five alternatives
for (k in 2:6) {
  k = 3
  # Import the quality data frame of the SOM2 alternatives at 
  iter_df = read_rds(paste0(here("data/som-iter/alternative-samples/som2"), "/som2_alt_", k ,"_nclust_10.rds")) |> 
    as.data.frame() |> 
    dplyr::select(-alt) |> 
    mutate(rowID = seq(1, 100),
           unexv = (100 - varra)/100,
           sizeperception = log(nrc))
  
  # join to full dataframe and calculate performance
  iter_df = rbind(iter_df, df)
  
  iter_df$topo_scaled = iter_df$topo / sd(iter_df$topo)
  iter_df$unex_scaled = iter_df$unexv / sd(iter_df$unexv)
  iter_df$size_scaled = iter_df$sizeperception / sd(iter_df$sizeperception) 
  iter_df$perf = minmaxnorm(iter_df$topo_scaled + iter_df$unex_scaled + iter_df$size_scaled)
  
  # keep just n_clust = 10 
  iter_df = iter_df |> dplyr::filter(nrc == 10)
  
  # filter out outliers
  iter_perf = iter_df$perf
  min_lim = median(iter_perf) - 1*mad(iter_perf)
  max_lim = median(iter_perf) + 1*mad(iter_perf)
  iter_df = iter_df |> dplyr::filter(perf >= min_lim) |> dplyr::filter(perf <= max_lim)
  
  rowno = which.min(iter_df$perf)
  winning_size = iter_df[rowno,]
  winning_size
  
  winning_som = readRDS(paste0(here("data/som-iter/alternative-samples/kohonen-objs/som2"), 
                               "/som2_alt_", k, "_iter_",winning_size$iter, "_nclust_10.rds"))
  
  write_rds(x = winning_som,
            file = paste0(here("data/som-iter/alternative-samples/kohonen-objs/winning_som2/som2_archetypes_iter_"), k, ".rds"))
  
}

# Import the 