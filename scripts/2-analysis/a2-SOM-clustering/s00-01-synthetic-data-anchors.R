library(here); source(here(("scripts/on_button.R")))

### ---------------------\\ 
# Script objective:
# Create a synthetic representation of input data using k-means cluster centers
### ---------------------\\ 

data_stack = c(terra::rast(here("data/input-data-stack-norm.tif")))
write_rds(data_stack |> as_tibble() |> drop_na(), file = here("data/input_features_full_set_norm.rds"))

data_all = readr::read_rds(here("data/input_features_full_set_norm.rds"))
data_all = data_all |> dplyr::select(-id)
data_all[data_all > 2] = 2; data_all[data_all < -2] = -2
write_rds(data_all, file = here("data/som_files/som_derivation_data/02_full_input_data_norm.rds"))

## identify the size of kmeans cluster centers to retain 99% of data
n.sample = 1.2e4

synthetic_anchors = kmeans(x = data_all, centers = n.sample, iter.max= 100)

round(synthetic_anchors$betweenss/synthetic_anchors$totss, 2) # ~99%  
n.sample/nrow(data_all) # 0.5% of data size

##
##
## 
anchor_codes = synthetic_anchors$centers |> as_tibble()

write_rds(anchor_codes, 
          file = here("data/som_files/som_derivation_data/01_synthetic_input_data.rds"))

write_rds(synthetic_anchors, 
          file = here("data/som_files/som_derivation_data/01_synthetic_kmeans_all_data.rds"))

##
# 
# ## identify the size of kmeans cluster centers to retain 99% of data
# # n.sample = 1.1e4
# n.sample = 250
# 
# synthetic_anchors = kmeans(x = data_all, centers = n.sample, iter.max= 100)
# synthetic_anchors$betweenss/synthetic_anchors$totss # 84%
# 
# data_all$batchID = synthetic_anchors$cluster
# 
# # within each of 100 first-batch k-means, calculate XX sub-clusters
# 
# df_batch_all = matrix(nrow = 0, ncol = ncol(data_all)) |>  as.data.frame()
# names(df_batch_all) = names(data_all)
# 
# centers_all = matrix(nrow = 0, ncol = ncol(synthetic_anchors$centers)) |>  as.data.frame()
# names(centers_all) = names(synthetic_anchors$centers |> as_tibble())
# 
# n.total = 1.2e4
# 
# for (i in 1:n.sample) {
#   # i = 1
#   # create batch df
#   batch_stack = data_all |> filter(batchID == i)
#   
#   batch_kmeans = kmeans(x = batch_stack[1:8], centers = (n.total/n.sample), iter.max= 100)
#   batch_stack$clustID = (n.total/n.sample*(i-1)) + batch_kmeans$cluster # this is so batches don't replicate IDs
#   
#   df_batch_all = rbind(df_batch_all, batch_stack) # bind batch to main df
#   centers_all = rbind(centers_all, batch_kmeans$centers |> as_tibble())
#   
#   batch_stack = NULL
#   message("done loop ", i, " out of ", n.sample)
#   
# }
# 
# ss_calc = df_batch_all |> 
#   dplyr::select(!c(batchID)) |> 
#   group_by(clustID) |> 
#   nest() |> 
#   summarise(
#     within_SS = map_dbl(data, ~calc_SS(.x))
#   )
# 
# # increase the size of second batch until this is >99%
# 1-sum(ss_calc$within_SS)/synthetic_anchors$totss