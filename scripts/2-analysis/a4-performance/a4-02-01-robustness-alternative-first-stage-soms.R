

nrc = 26

for (i in 3:6) {
  
  # i = 2
  message("starting alternative iteration ", i)
  
  # import the alternative sample sets
  input_df = readr::read_rds(paste0(here("data/ds_df_sampled_for_SOM_"), i, ".rds")) |> as_tibble()
  som_input = input_df |> dplyr::select(!c(id, all)) |> as.matrix()
  
  quality_df = data.frame(
    iter = seq(1:20),
    quant = rep(NA),
    varra = rep(NA),
    k_l  = rep(NA),
    topo = rep(NA),
    nrc = rep(nrc)
  )
  
  for (j in 1:(quality_df$iter |> max())) {
    # i = 1
    st = Sys.time()
    som_iter = supersom(som_input, 
                        grid = somgrid(xdim = nrc, ydim = nrc, topo="hexagonal"), 
                        rlen = 500, 
                        alpha = c(0.05, 0.01),
                        keep.data = TRUE,
                        cores = 8,
                        mode = "pbatch")
    et = Sys.time()
    message(et-st)
    
    ## write out SOM data to recall if best performing SOM
    write_rds(x = som_iter,
              file = paste0(here("data/som-iter/alternative-samples/kohonen-objs/som1_alt_"), i, "_iter_", j, ".rds")) 
    
    som_quality = aweSOM::somQuality(som = som_iter, traindat = som_input)
    
    quality_df$quant[j] = som_quality$err.quant[1] |> as.numeric() |> round(2)
    quality_df$varra[j] = som_quality$err.varratio[1] |> as.numeric() |> round(2)
    quality_df$k_l[j]   = som_quality$err.kaski[1] |> as.numeric() |> round(2)
    quality_df$topo[j]  = som_quality$err.topo[1] |> as.numeric() |> round(2)
    message("... iteration ", j, " is done for given SOM size")
  }
  
  write_rds(x = quality_df,
            file = paste0(here("data/som-iter/alternative-samples/som1_alt_"), i, ".rds"))
  message("SOM iterations for alternative ", i, " has completed")
}
