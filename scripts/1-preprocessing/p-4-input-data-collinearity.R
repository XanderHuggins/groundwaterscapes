
# ss_df = read_rds(here("data/ds_df_full.rds")) 

input_data = readr::read_rds(here("data/som_files/som_derivation_data/02_full_input_data_norm.rds"))

colnames = c("Water Table Ratio", "Porosity", "Terr. GDE", "Aqu. GDE", "Field size", "GW Irr.", "GW mgmt.", "Improved drinking water")

M = cor(x = input_data |> 
          mutate(udw = -udw) |> 
          set_colnames(colnames) |> 
          slice_sample(n = 40000), 
        method = "pearson")
round(M, 2)

pdf_size = 5
pdf(here("plots/input_correlation.pdf"), width=2*pdf_size, height=pdf_size)

corrplot(M, method = 'shade', 
         diag = T,
         bg = "transparent",
         type = 'upper',
         tl.cex = 0.8,
         tl.col = "black")
dev.off()
