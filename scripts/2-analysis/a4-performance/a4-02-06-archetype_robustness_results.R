
# make a raster stack of all archetype alternatives

arch_stack = c(rast(here("data/MAP_archetypes.tif")),
               rast(here("data/MAP_archetypes_iter_2.tif")),
               rast(here("data/MAP_archetypes_iter_3.tif")),
               rast(here("data/MAP_archetypes_iter_4.tif")),
               rast(here("data/MAP_archetypes_iter_5.tif")),
               rast(here("data/MAP_archetypes_iter_6.tif")),
               rast(here("data/input-data-stack.tif"))[[8]])
names(arch_stack) = c('arch', 'alt2', 'alt3', 'alt4', 'alt5', 'alt6', 'cellID')

arch_stack_df = arch_stack |> 
  as.data.frame()

arch_stack_df = arch_stack_df |> dplyr::filter(arch >= 1)
arch_stack_df$rowID = seq(1, nrow(arch_stack_df))
               
# generate list of 10,000 pairs of IDs
no_of_pairs = 1e5

rowID1 = arch_stack_df |> slice_sample(n = no_of_pairs) |> dplyr::pull(rowID)
rowID2 = arch_stack_df |> slice_sample(n = no_of_pairs) |> dplyr::pull(rowID)

arch_points1 = arch_stack_df[rowID1, ]
arch_points2 = arch_stack_df[rowID2, ]

arch_points1$cellID == arch_points2$cellID

# determine matches and misses for the set of pairs
match_df = data.frame(
  pairNo = seq(1, length(no_of_pairs)),
  cellID1 = arch_points1$cellID,
  cellID2 = arch_points2$cellID,
  archID = arch_points1$arch,
  arch_match = rep(NA),
  arch2_match = rep(NA),
  arch3_match = rep(NA),
  arch4_match = rep(NA),
  arch5_match = rep(NA),
  arch6_match = rep(NA)
)

match_df$arch_match =  (arch_points1$arch == arch_points2$arch)
match_df$arch2_match = (arch_points1$alt2 == arch_points2$alt2)
match_df$arch3_match = (arch_points1$alt3 == arch_points2$alt3)
match_df$arch4_match = (arch_points1$alt4 == arch_points2$alt4)
match_df$arch5_match = (arch_points1$alt5 == arch_points2$alt5)
match_df$arch6_match = (arch_points1$alt6 == arch_points2$alt6)

# create data frame to keep-track of individual archetype performance metrics
result_df = data.frame(
  arch = seq(1:10),
  match_match = rep(NA),
  match_miss = rep(NA),
  miss_match = rep(NA),
  miss_miss = rep(NA),
  count = rep(NA)
)

for (k in result_df$arch) {
  # k = 5
  
  match_df_temp = match_df |> dplyr::filter(archID == k)
  result_df$count[k] = nrow(match_df_temp)
  
  # Calculate average rate of match-match
  # match_df_temp = match_df[1:10,]
  
  comb1 = match_df_temp |> dplyr::filter(arch_match == TRUE & arch2_match == TRUE) |> nrow()
  comb2 = match_df_temp |> dplyr::filter(arch_match == TRUE & arch3_match == TRUE) |> nrow()
  comb3 = match_df_temp |> dplyr::filter(arch_match == TRUE & arch4_match == TRUE) |> nrow()
  comb4 = match_df_temp |> dplyr::filter(arch_match == TRUE & arch5_match == TRUE) |> nrow()
  comb5 = match_df_temp |> dplyr::filter(arch_match == TRUE & arch6_match == TRUE) |> nrow()
  result_df$match_match[k] = mean(c(comb1, comb2, comb3, comb4, comb5))
  
  # Calculate average rate of match-miss
  comb1 = match_df_temp |> dplyr::filter(arch_match == TRUE & arch2_match == FALSE) |> nrow()
  comb2 = match_df_temp |> dplyr::filter(arch_match == TRUE & arch3_match == FALSE) |> nrow()
  comb3 = match_df_temp |> dplyr::filter(arch_match == TRUE & arch4_match == FALSE) |> nrow()
  comb4 = match_df_temp |> dplyr::filter(arch_match == TRUE & arch5_match == FALSE) |> nrow()
  comb5 = match_df_temp |> dplyr::filter(arch_match == TRUE & arch6_match == FALSE) |> nrow()
  result_df$match_miss[k] = mean(c(comb1, comb2, comb3, comb4, comb5)) 
  
  # Calculate average rate of miss-match
  comb1 = match_df_temp |> dplyr::filter(arch_match == FALSE & arch2_match == TRUE) |> nrow()
  comb2 = match_df_temp |> dplyr::filter(arch_match == FALSE & arch3_match == TRUE) |> nrow()
  comb3 = match_df_temp |> dplyr::filter(arch_match == FALSE & arch4_match == TRUE) |> nrow()
  comb4 = match_df_temp |> dplyr::filter(arch_match == FALSE & arch5_match == TRUE) |> nrow()
  comb5 = match_df_temp |> dplyr::filter(arch_match == FALSE & arch6_match == TRUE) |> nrow()
  result_df$miss_match[k] = mean(c(comb1, comb2, comb3, comb4, comb5))
  
  # Calculate average rate of miss-miss
  comb1 = match_df_temp |> dplyr::filter(arch_match == FALSE & arch2_match == FALSE) |> nrow()
  comb2 = match_df_temp |> dplyr::filter(arch_match == FALSE & arch3_match == FALSE) |> nrow()
  comb3 = match_df_temp |> dplyr::filter(arch_match == FALSE & arch4_match == FALSE) |> nrow()
  comb4 = match_df_temp |> dplyr::filter(arch_match == FALSE & arch5_match == FALSE) |> nrow()
  comb5 = match_df_temp |> dplyr::filter(arch_match == FALSE & arch6_match == FALSE) |> nrow()
  result_df$miss_miss[k] = mean(c(comb1, comb2, comb3, comb4, comb5)) 

}

result_df$match_error = round(result_df$match_miss / (result_df$match_match + result_df$match_miss), 4)
result_df$miss_error =  round(result_df$miss_match / (result_df$miss_match + result_df$miss_miss), 4)
result_df$total_error = round((result_df$miss_match + result_df$match_miss) / result_df$count, 4)
result_df$match_count = result_df$match_match + result_df$match_miss
result_df$miss_count = result_df$miss_match + result_df$miss_miss

result_df

# total error overall
te = weighted.mean(x = result_df$total_error, w = result_df$count) 
match_error = weighted.mean(x = result_df$match_error, w = result_df$match_count) 
miss_error = weighted.mean(x = result_df$miss_error, w = result_df$match_count) 
sum(result_df$match_count)
sum(result_df$miss_count)

ggplot() +
  # explained variance
  # geom_line(aes(y = rank_s), col = "#EF440C", linewidth = 2) +
  geom_point(data = result_df,     aes(x= arch, y = 100*total_error, fill = as.factor(arch)), 
             col = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal_arch) +
  geom_hline(yintercept = 100*te, linewidth = 2) + 
  coord_cartesian(ylim=c(0, 20), xlim = c(0.5, 10.5), expand = 0, clip = "off") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  my_theme + 
  theme(axis.line = element_line(size = 1), 
        panel.grid.major = element_line(),
        axis.text = element_text(size=13),
        axis.title = element_blank()) 
ggsave(plot = last_plot(),
       filename = here("plots/confusion_matrix_total_error_archetypes.png"),
       height = 10,
       width = 18,
       units = "cm",
       dpi = 400)
