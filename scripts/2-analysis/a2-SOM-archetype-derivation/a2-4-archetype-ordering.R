

# import archetypes and re-number the archetypes based on determined archetype numbers (to preserve colour scheme)
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

i = 10
plot_df = data.frame(x = seq(1:8), y = archetypes_df[i,] |> as.numeric())
ggplot(data = plot_df) + 
  geom_bar(aes(x = x, y = y, fill = y), stat = "identity", width = 0.99) +
  scico::scale_fill_scico(begin = 0.1, end = 0.9, direction = -1, palette = "vik", midpoint = 0) +
  theme_void() +
  geom_hline(yintercept = 0, linewidth= 0.5) + 
  coord_cartesian(expand = 0, ylim = c(-0.5, 0.5)) +
  # scale_x_continuous(breaks = seq(1,8), labels = rownames(plot_df)) +
  theme(legend.position = "none")
  
# previous archetypes (when n=11) 3 and 11 are compressed into one archetype (when n = 10)
# so sort 1,2, 4-11 in 1-10

# create reclassification matrix based on developed archetype names
rcl.m = matrix(c(
  c(1,  9), # instance of 1 is set to name 11:  extensive GDEs and low storage
  c(2,  8), # instance of 2 is set to name 10: extensive GDEs and climate functions
  c(3,  7), # instance of 3 is set to name 8:  largeholder farming with moderage GDEs
  c(4,  6), # instance of 4 is set to name 7:  largeholder farming with moderate dependence on groundwater 
  c(5,  5), # instance of 5 is set to name 6:  smallholder farming highly dependent of groundwwater
  c(6,  3), # instance of 6 is set to name 4: Earth system functions in remote regions w/ developed mgmt
  c(7,  4), # instance of 7 is set to name 9: Little ag or eco dependence, with high climate function w/ limited mgmt
  c(8,  10), # instance of 8 is set to name 5: underserviced populations with moderate ecosystems
  c(9,  1), # instance of 9 is set to name 2: remote regions with minimal functions
  c(10, 2)),# instance of 10 is set to name 1: large storage functions in generally remote regions
  ncol = 2, byrow = T) |> as.data.frame() |> set_colnames(c('from', 'to'))

readr::write_rds(x = rcl.m,
                 file = here("data/archetype_reorder_matrix.rds"))

# now reclassify and write to file
archetypes_df = readr::read_rds(here("data/som-iter/final-kohonen-objs/som2_archetypes.rds"))

classif_rcl = data.frame(old = archetypes_df$unit.classif, new = rep(NA))
classif_rcl$new = rcl.m$to[match(classif_rcl$old, rcl.m$from)]
archetypes_df$unit.classif = classif_rcl$new
write_rds(x = archetypes_df, file = here("data/som-iter/final-kohonen-objs/som2_archetypes_reclassed.rds"))