
input_data = readr::read_rds(here("data/som_files/som_derivation_data/02_full_input_data_norm.rds")) |> 
  slice_sample(n = 10000)

prototypes = readr::read_rds(here("data/som_files/som_files_full/som1_nrc_22_iter_40.rds"))
prototypes = prototypes$codes[[1]] |> as_tibble()

# prototypes = readr::read_rds(here("data/som_files/som_files_full/som1_nrc_22_iter_40.rds"))
# prototypes = prototypes$codes[[1]] |> as_tibble()
# 
# plot(input_data$gde_t ~ input_data$gde_a, col = "black")
# plot(prototypes$gde_t ~ prototypes$gde_a, col = "red", add = TRUE)

# prototypes = readr::read_rds(here("data/som_files/som_selections/som1_nrc_22_iter_1.rds"))

for (i in 1:ncol(input_data)) {
  # i = 8
  input_data = readr::read_rds(here("data/som_files/som_derivation_data/02_full_input_data_norm.rds")) |> 
    slice_sample(n = 10000)
  input_dat = input_data[,i] |> unlist() |> as.vector()
  proto_dat = prototypes[,i] |> unlist() |> as.vector()
  add.name = names(input_data)[i]
  
  ks_test = ks.test(x = input_dat, y = proto_dat)
  
  pdf_size = 3.5
  pdf(paste0(here("plots/KS_ecdf_"), names(input_data)[i], ".pdf"), width=2*pdf_size, height=pdf_size)
  
  plot(ecdf(input_dat), verticals=TRUE, do.points=FALSE, col='black', lwd = 2,
       main = paste0(add.name, ": ", round(ks_test$statistic, 3)))
  plot(ecdf(proto_dat), verticals=TRUE, do.points=FALSE, add=TRUE, col='red', lwd = 2)
  
  dev.off()

}
