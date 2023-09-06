
ss_df = read_rds(here("data/ds_df_full.rds")) 
colnames = c("Water Table Ratio", "Porosity", "Terr. GDE", "Aqu. GDE", "Field size", "GW Irr.", "GW mgmt.", "Unimproved drinking water")

M = cor(x = ss_df |> 
          dplyr::select(!c(id)) |> 
          mutate(udw = -udw) |> 
          set_colnames(colnames) |> 
          slice_sample(n = 10000), 
        method = "pearson")
round(M, 2)

corrplot(M, method = 'shade', 
         diag = T,
         bg = "transparent",
         type = 'upper',
         tl.cex = 0.8,
         tl.col = "black")
