## --------------------------- \
# import archetypes and re-number the archetypes based on determined archetype colour scheme - archetype description pairs
archetypes_df = readr::read_rds(here("data/som-iter/final-kohonen-objs/som2_archetypes.rds"))
archetypes_df = archetypes_df$codes |> as.data.frame()
archetypes_df = sapply(archetypes_df,minmaxnorm)
archetypes_df = archetypes_df |> as.data.frame()

for (i in 1:ncol(archetypes_df)) {
  # i = 1
  vect_i = archetypes_df[,i]
  rel_vect = vect_i - mean(vect_i)
  archetypes_df[,i] = rel_vect
  
}

## --------------------------- \
# plot bar graph of each archetype to compare to Table 2 in main text
i = 10 # iteratively go through 10 archetypes and set re-classification matrix as needed on Line 32--
plot_df = data.frame(x = seq(1:8), y = archetypes_df[i,] |> as.numeric())
ggplot(data = plot_df) + 
  geom_bar(aes(x = x, y = y, fill = y), stat = "identity", width = 0.99) +
  scico::scale_fill_scico(begin = 0.1, end = 0.9, direction = -1, palette = "vik", midpoint = 0) +
  theme_void() +
  geom_hline(yintercept = 0, linewidth= 0.5) + 
  coord_cartesian(expand = 0, ylim = c(-0.5, 0.5)) +
  # scale_x_continuous(breaks = seq(1,8), labels = rownames(plot_df)) +
  theme(legend.position = "none")
  
# create reclassification matrix based on developed archetype names
rcl.m = matrix(c(
  c(1,  9), # SOM cluster 1 becomes Archetype 9
  c(2,  8), # SOM cluster 2 becomes Archetype 8
  c(3,  7), # SOM cluster 3 becomes Archetype 7
  c(4,  6), # SOM cluster 4 becomes Archetype 6
  c(5,  5), # SOM cluster 5 becomes Archetype 5
  c(6,  3), # SOM cluster 6 becomes Archetype 3
  c(7,  4), # SOM cluster 7 becomes Archetype 4
  c(8,  10),# SOM cluster 8 becomes Archetype 10
  c(9,  1), # SOM cluster 9 becomes Archetype 1
  c(10, 2)),# SOM cluster 10 becomes Archetype 2
  ncol = 2, byrow = T) |> as.data.frame() |> set_colnames(c('from', 'to'))

# write reclass matrix to file
readr::write_rds(x = rcl.m,
                 file = here("data/archetype_reorder_matrix.rds"))

# now reclassify and write archetype kononen object to file
archetypes_df = readr::read_rds(here("data/som-iter/final-kohonen-objs/som2_archetypes.rds"))

classif_rcl = data.frame(old = archetypes_df$unit.classif, new = rep(NA))
classif_rcl$new = rcl.m$to[match(classif_rcl$old, rcl.m$from)]
archetypes_df$unit.classif = classif_rcl$new
write_rds(x = archetypes_df, file = here("data/som-iter/final-kohonen-objs/som2_archetypes_reclassed.rds"))