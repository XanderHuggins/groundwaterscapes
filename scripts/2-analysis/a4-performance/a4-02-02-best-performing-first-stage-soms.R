
# import all som1 sizes 

# list.files(path = here("data/som-iter/alternative-samples/"), pattern = 'som1_alt_')
df = list.files(here("data/som-iter/alternative-samples"), pattern = ".rds", full.names = T) 

# need to sort 
df = df |> # c(df[22:27], df[1:21]) |>
  map_dfr(readRDS) 
df$unexv = (100 - df$varra)/100
df$repno = c(rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 20), rep(6,20)) 
df$iter = rep(seq(1,20), 5)
df$rowID = seq(1,nrow(df))

# import the performance of the original first-stage SOM to include variance of other SOM dimensions
som_orig_df = list.files(here("data/som-iter"), pattern = ".rds", full.names = T) 

# need to sort 
som_orig_df = som_orig_df |> # c(df[22:27], df[1:21]) |>
  map_dfr(readRDS) |> 
  dplyr::filter(nrc <= 30)
som_orig_df$unexv = (100 - som_orig_df$varra)/100
som_orig_df$rowID = seq(1, nrow(som_orig_df))
som_orig_df = som_orig_df |> dplyr::filter(nrc != 26)

# now loop through each alternative SOM iteration
for (i in 2:6) {
  # i = 2
  iter_som_df = df |> dplyr::filter(repno == i) |> dplyr::select(!repno)
  comb_df = rbind(iter_som_df, som_orig_df)
  
  # scale using the combined dataset
  comb_df$topo_scaled = comb_df$topo / sd(comb_df$topo)
  comb_df$unex_scaled = comb_df$unexv / sd(comb_df$unexv)
  comb_df$perf = minmaxnorm(comb_df$topo_scaled + comb_df$unex_scaled)
  
  # filter back to just the nrc - 26 size
  iter_som_df = comb_df |> dplyr::filter(nrc == 26)
  
  # filter out outliers
  iter_perf = iter_som_df$perf
  min_lim = median(iter_perf) - 1*mad(iter_perf)
  max_lim = median(iter_perf) + 1*mad(iter_perf)
  
  iter_som_df = iter_som_df |> dplyr::filter(perf >= min_lim) |> dplyr::filter(perf <= max_lim)
  rowno = which.min(iter_som_df$perf)
  winning_size = iter_som_df[rowno,]
  winning_size 
  
  # Write winning SOM to file
  som_objs = list.files(here("data/som-iter/alternative-samples/kohonen-objs/"), pattern = ".rds", full.names = T)
  
  winning_som = readRDS(paste0(here("data/som-iter/alternative-samples/kohonen-objs/"), 
                               "/som1_alt_", i, "_iter_", winning_size$iter, ".rds"))
  
  write_rds(x = winning_som,
            file = paste0(here("data/som-iter/alternative-samples/kohonen-objs/winning_som1"),
                          "/winning_som1_alt_", i, ".rds"))
}