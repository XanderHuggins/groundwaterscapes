library(here); source(here(("scripts/on_button.R")))

### ---------------------\\ 
# Script objective:
# Assess performance of all SOM models trained on synthetic data and select best performing model
### ---------------------\\ 

# Import performance metrics of each SOM1 architecture size
df = readr::read_rds(here("data/som_files/som1_performance_synthetic/som_SYNTHspace_performance_combined.rds"))

# We are interested in clusters that (1) preserve the topography of the data, and (2) have good cluster seperation

# plot the topographic error index -- and remove entries that are outliers
# outliers are defined as being >1 MAD of within-size deviation and median-across-size deviation from median at each size
# this approach thus preferenes sizes that have more reproducible performance metrics, and does not give advantage to sizes with large variances
plot(df$db_x ~ df$som_size)
plot(df$topo ~ df$som_size)
plot(df$k_l ~ df$som_size)
plot(100 - df$varra ~ df$som_size)

df_keep = matrix(nrow = 1, ncol = ncol(df)) |>  as.data.frame()
names(df_keep) = names(df)

##
### ------------------- Filter stage 1: remove outliers of individual performance metric\\
##

global_mad_db = df |> group_by(som_size) |> summarise(mad = mad(db_x)) |> pull(mad) |> mean()
global_mad_kl = df |> group_by(som_size) |> summarise(mad = mad(k_l)) |> pull(mad) |> mean()

for (i in unique(df$som_size)) {
    # i = 10
    iter_df = df |> dplyr::filter(som_size == i)

    local_mad_db = mad(iter_df$db_x)
    local_mad_kl = mad(iter_df$k_l)

    range_allow_db = min(c(global_mad_db, local_mad_db))
    range_allow_kl = min(c(global_mad_kl, local_mad_kl))

    iter_df = iter_df |>
      dplyr::filter(db_x >= median(db_x) - range_allow_db) |>
      dplyr::filter(db_x <= median(db_x) + range_allow_db) |>
      dplyr::filter(k_l >= median(k_l) - range_allow_kl) |>
      dplyr::filter(k_l <= median(k_l) + range_allow_kl)

    df_keep = rbind(df_keep, iter_df) # bind non-outlier iterations
}
df_keep = df_keep[2:nrow(df_keep),]
nrow(df_keep) # 456 kept out of 920
plot(df_keep$db_x ~ df_keep$som_size)
plot(df_keep$k_l ~ df_keep$som_size)
# removing these outliers gives results eligible for selection moving forward ...

df_keep = df_keep |> drop_na()

# variance-scale and min-max normalize each index considered
df_keep$dbi_scaled = minmaxnorm(df_keep$db_x)
df_keep$kl_scaled = minmaxnorm(df_keep$k_l)

plot(df_keep$dbi_scaled ~ df_keep$som_size)
plot(df_keep$kl_scaled ~ df_keep$som_size)

##
### ------------------- Filter stage 1: remove outliers of combined performance metric\\
##

df_keep$perf = minmaxnorm(df_keep$dbi_scaled + df_keep$kl_scaled)
plot(df_keep$perf ~ df_keep$som_size)

# removal outliers from this composite score, to make sure relationship between two metrics is reproducible
df_keep_combined = matrix(nrow = 1, ncol = ncol(df_keep)) |>  as.data.frame()
names(df_keep_combined) = names(df_keep)

# set median absolute deviation factor to detect outliers +/- this MAD around the median per SOM size
mad_x = 1 # set narrower band for high robustness (i.e., consistent determination of best-performing SOM size between script re-runs)

for (i in unique(df$som_size)) {
  # i = 12
  iter_df = df_keep |> dplyr::filter(som_size == i)
  iter_perf = iter_df$perf
  min_lim = median(iter_perf) - mad_x*min(c(mad(iter_perf), mad(df_keep$perf))) # identify lower limit of non-outliers
  max_lim = median(iter_perf) + mad_x*min(c(mad(iter_perf), mad(df_keep$perf))) # identify upper limit of non-outliers

  iter_df = iter_df |> dplyr::filter(perf >= min_lim) |> dplyr::filter(perf <= max_lim) # keep only iterations within these limits
  df_keep_combined = rbind(df_keep_combined, iter_df) # bind non-outlier iterations
}

# select the best performing iteration as the nominated iteration to move forward with
plot(df_keep_combined$perf ~ df_keep_combined$som_size, main = "less combined outliers")

# determine best-performing SOM among non-outlier iterations
rowno = which.min(df_keep_combined$perf)
winning_size = df_keep_combined[rowno,]
winning_size # this is best-performing SOM from first-stage

### We can observe that the best performing first-stage SOM is:
som_size = 22

iter_index = expand.grid(size_iter = c(1:60),
                         size = som_size)

write_rds(iter_index, file = here("data/som_files/som_derivation_data/00_full_SOM_iteration_sizing_index.rds"))


##
## -- plot
##

best_at_size = df_keep_combined |> 
  group_by(som_size) |> drop_na() |> 
  summarise(
    perf = min(perf, na.rm = T)
  )

ggplot() +
  # explained variance
  # geom_line(aes(y = rank_s), col = "#EF440C", linewidth = 2) +
  geom_point(data = df_keep_combined,     aes(x= som_size, y = perf), col = "grey30", size = 3, alpha = 0.6) +
  geom_point(data = best_at_size, aes(x= som_size, y = perf), col = "black", size = 5) +
  coord_cartesian(ylim=c(0, 1), xlim = c(9.5, 42.5), expand = 0, clip = "off") +
  scale_x_continuous(breaks = seq(4, 42, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  my_theme + 
  theme(axis.line = element_line(size = 1), 
        panel.grid.major = element_line(),
        axis.text = element_text(size=13),
        axis.title = element_blank()) 

ggsave(plot = last_plot(),
       filename = here("plots/SOM1_synthetic_best_size_evaluation.png"),
       height = 10,
       width = 18,
       units = "cm",
       dpi = 400)