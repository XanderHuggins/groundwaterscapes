
# import the prototype data, and estimate the number of cluster centers
prototypes = readr::read_rds(here("data/som_files/som_files_full/som1_nrc_22_iter_40.rds"))
prototypes = prototypes$codes[[1]] |> as_tibble()

clut_apriori = NbClust(data = prototypes, 
                      min.nc = 2, max.nc = 30, 
                      method = "ward.D2", 
                      index = "all")

# weighted mean of all clustering suggestions
apriori_df = clut_apriori$Best.nc |> as_tibble() |> t() |> set_colnames(c('nc', 'value')) |> as.data.frame()
apriori_df = apriori_df$nc |> unlist() |> as.vector() |> as.numeric() |> table() |> as.data.frame() |> set_colnames(c('best_nc', 'freq')) 
apriori_df$best_nc = apriori_df$best_nc |> as.character() |> as.numeric()
apriori_df$freq = apriori_df$freq |> as.character() |> as.numeric()

best_nc = median(apriori_df$best_nc)[1]
best_nc = 6
# trunc(weighted.mean(x= apriori_df$best_nc, w = apriori_df$freq))
# results suggest that 5 clusters should be a-priori estimate
# weighted meadian is 5.5

# Import performance metrics of each SOM2 architecture size
df = list.files(here("data/som_files/som2_performance/"), pattern = ".rds", full.names = T) |> 
  map_dfr(readRDS) 
nrow(df)
names(df)[7] = "som_size"
# ! MAKE SURE nrc is named to som_size, wherever it is located in data.table

##
### ------------------- Stage -1: size preference function \\
##

triangle_df = tibble(
  som_size = seq(2, 30)) |> 
  mutate(size_preference = dnorm(seq(1, 100, length.out = 29)/100, 0.5, 2))
triangle_df$size_preference_scaled = minmaxnorm(triangle_df$size_preference / sd(triangle_df$size_preference))
plot(triangle_df$size_preference_scaled ~ triangle_df$som_size)

norm_extract = tibble(y = minmaxnorm(dnorm(seq(0, 100, length.out = 58)/100, mean = 0.5, sd = 1)),
                      x = c(2, c(2 + 28 * (seq(0, 100, length.out = 57)/100)))) 
norm_extract$y = as.numeric(norm_extract$y)
norm_extract$x = as.numeric(norm_extract$x) |> round(1)
# plot(minmaxnorm(dnorm(seq(0, 100, length.out = 58)/100, mean = 0.5, sd = 1)) ~
#        c(2, c(2 + 28 * (seq(0, 100, length.out = 57)/100))))

top = norm_extract |> dplyr::filter(x == best_nc) |> pull(y)
triangle_df$size_preference_scaled[triangle_df$size_preference_scaled > top] = top
plot(triangle_df$size_preference_scaled ~ triangle_df$som_size)
triangle_df$size_preference_scaled = 1- minmaxnorm(triangle_df$size_preference_scaled)
plot(triangle_df$size_preference_scaled ~ triangle_df$som_size)

df = merge(x = df, y = triangle_df,
           by.x = "som_size", by.y = "som_size")

df$sizeperception = log(df$som_size)
df$sizeperception_scaled = minmaxnorm(log(df$som_size) / sd(log(df$som_size)))
plot(df$sizeperception_scaled ~ df$som_size)

df$size_bias = (df$sizeperception_scaled + 3*df$size_preference_scaled)/4

plot(df$size_bias ~ df$som_size)
# df$sizeperception = df$nrc

##
### ------------------- Stage 0: create scaled metrics \\
##

# calculate explained variation so that very small n aren't benefitted by SOM metrics
df$unexv = 1-df$varra/100

# plot to see
plot(df$unexv ~ df$som_size)
plot(df$topo ~ df$som_size)
# plot(df$db_x ~ df$som_size)
plot(df$k_l ~ df$som_size)

# variance-scale and min-max normalize each index considered
df$dbi_scaled = minmaxnorm(df$db_x)
df$k_l_scaled = minmaxnorm(df$k_l)
df$topo_scaled = minmaxnorm(df$topo)
df$unexv_scaled = minmaxnorm(df$unexv)

plot(df$dbi_scaled ~ df$som_size)
plot(df$k_l_scaled ~ df$som_size)
plot(df$topo_scaled ~ df$som_size)
plot(df$unexv_scaled ~ df$som_size)

# SOM performance
df$SOM_perf = minmaxnorm(df$k_l_scaled + df$unexv_scaled)
plot(df$SOM_perf ~ df$som_size)

# Clustering performance
plot(df$dbi_scaled ~ df$som_size)

# size bias
df$sizebias_scaled = minmaxnorm(df$size_bias / sd(df$size_bias))
plot(df$sizebias_scaled ~ df$som_size)

df$perf = minmaxnorm(df$SOM_perf + df$dbi_scaled)
plot(df$perf ~ df$som_size)

##
### ------------------- Sensitivity approach iterate through sensitivity analysis, looking at how combinations of allowable MAD, and size bias affect outcome
## 

sensitivity_df = expand.grid(mad_range = seq(0, 1, length.out = 21),
                             bias_weight = seq(0, 1, length.out = 21),
                             size = rep(NA), iter = rep(NA))
sensitivity_df$mad_range[seq(1, 421, by = 21)] = 0.01

for (jj in 1:nrow(sensitivity_df)) {
  
  # jj = 410
  sensitivity_df$mad_range[jj]
  sensitivity_df$bias_weight[jj]
  ##
  ### ------------------- Stage 1: remove outliers to improve reproducibility \\
  ##
  
  # removal outliers from this composite score, to make sure relationship between two metrics is reproducible
  df_keep_combined = matrix(nrow = 1, ncol = ncol(df)) |>  as.data.frame()
  names(df_keep_combined) = names(df)
  
  # set median absolute deviation factor to detect outliers +/- this MAD around the median per SOM size
  mad_x = sensitivity_df$mad_range[jj] 
  
  global_allowable_noise = df |> group_by(som_size) |> 
    summarise(mad_perf = mad(perf)) |> pull(mad_perf) |> median()
  
  for (i in unique(df$som_size)) {
    # i = 12
    iter_df = df |> dplyr::filter(som_size == i)
    iter_perf = iter_df$perf
    iter_allowable_noise = min(c(mad(iter_perf), global_allowable_noise))
    min_lim = median(iter_perf) - mad_x*iter_allowable_noise # identify lower limit of non-outliers
    max_lim = median(iter_perf) + mad_x*iter_allowable_noise # identify upper limit of non-outliers
    
    iter_df = iter_df |> dplyr::filter(perf >= min_lim) |> dplyr::filter(perf <= max_lim) # keep only iterations within these limits
    df_keep_combined = rbind(df_keep_combined, iter_df) # bind non-outlier iterations
  }
  df_keep_combined = df_keep_combined[2:nrow(df_keep_combined),] # first row contains NA vals
  
  ##
  ### ------------------- Stage 2: introduce size bias \\
  ##
  
  df_keep_combined$perf_x_size = minmaxnorm(df_keep_combined$perf) + 
    (sensitivity_df$bias_weight[jj]*df_keep_combined$sizebias_scaled)
  plot(df_keep_combined$perf_x_size ~ df_keep_combined$som_size, main = paste0("mad ", sensitivity_df$mad_range[jj] , 
                                                                               " bias ",  sensitivity_df$bias_weight[jj]))
  
  rowno = which.min(df_keep_combined$perf_x_size)
  winning_size = df_keep_combined[rowno,]
  # winning_size
  sensitivity_df$size[jj] = winning_size$som_size
  sensitivity_df$iter[jj] = winning_size$iter
  message(jj)
  
}

hist(sensitivity_df$size)

# Plot geom_tile of results 

ggplot(sensitivity_df, aes(x = mad_range, bias_weight, fill= size)) + 
  geom_tile(width = 0.05) +
  # geom_text(aes(label=size)) +
  # geom_text(aes(label=iter)) +
  MetBrewer::scale_fill_met_c(name = "Redon") +
  coord_cartesian(expand = 0) +
  cowplot::theme_cowplot(font_size = 10) 

ggsave(plot = last_plot(),
       filename = here("plots/SOM2_sensitivity.png"),
       height = 10,
       width = 10,
       units = "cm",
       dpi = 400)

sensitivity_df |> filter(size == 18)

# though it's found in a minority of cases, we select som size of 17 as it best balances reproducibility, and size bias 

win_size = 18
win_iter = 23

file.copy(from = paste0(here("data/som_files/som2_files/som2_nclust_"),
                        win_size, "_iter_", win_iter, ".rds"),
          to = here("data/som_files/som_selections/som2_selection.rds"),
          overwrite = TRUE,
          copy.mode = TRUE)


##
## plot --
##
k_l_scaled
unexv_scaled
dbi_scaled


ggplot() +
  # explained variance
  # geom_line(aes(y = rank_s), col = "#EF440C", linewidth = 2) +
  geom_point(data = df,     aes(x= som_size, y = sizebias_scaled), col = "grey30", size = 3, alpha = 0.6) +
  # geom_point(data = best_at_size, aes(x= som_size, y = perf), col = "black", size = 5) +
  coord_cartesian(ylim=c(0, 1), xlim = c(1.5, 30.5), expand = 0, clip = "off") +
  scale_x_continuous(breaks = seq(4, 30, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  my_theme + 
  theme(axis.line = element_line(size = 1), 
        panel.grid.major = element_line(),
        axis.text = element_text(size=13),
        axis.title = element_blank()) 
ggsave(plot = last_plot(),
       filename = here("plots/SOM2_element_sizebias_scaled.png"),
       height = 10,
       width = 18,
       units = "cm",
       dpi = 400)

# win_size = 11
# win_iter = 119
# 
# file.copy(from = paste0(here("data/som_files/som2_files/som2_nclust_"),
#                         win_size, "_iter_", win_iter, ".rds"),
#           to = here("data/som_files/som_selections/som2_selection.rds"),
#           overwrite = TRUE,
#           copy.mode = TRUE)