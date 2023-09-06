
# Import the winning alternative first-stage SOMs, and generate the second-stage SOMs
for (k in 2:6) {
  
  # k = 2

  # import winning first-stage SOM
  prototypes = readr::read_rds(paste0(here("data/som-iter/alternative-samples/kohonen-objs/winning_som1"), "/winning_som1_alt_", k, ".rds"))
  prototypes = prototypes$codes[[1]] |> as.data.frame()
  
  # convert prototypes to matrix to feed to som function
  som_input = prototypes |> as.matrix()
  
  n_clust = 10
  
  quality_df = data.frame(
    iter = seq(1:100),
    quant = rep(NA),
    varra = rep(NA),
    k_l  = rep(NA),
    topo = rep(NA),
    nrc = rep(n_clust),
    alt = rep(k)
  )
    
  for (i in 1:(quality_df$iter |> max())) {
    # i = 1
    
    som_iter = supersom(som_input, 
                        grid = somgrid(xdim = n_clust, ydim = 1, topo="hexagonal"), 
                        rlen = 500, 
                        alpha = c(0.05, 0.01),
                        keep.data = TRUE,
                        cores = 8)
    
    ## write out SOM data to recall if best performing SOM
    write_rds(x = som_iter,
              file = paste0(here("data/som-iter/alternative-samples/kohonen-objs/som2"), "/som2_alt_", k, "_iter_", i, "_nclust_", n_clust, ".rds")) 
    
    som_quality = aweSOM::somQuality(som = som_iter, traindat = som_input)
    
    quality_df$quant[i] = som_quality$err.quant[1] |> as.numeric() |> round(2)
    quality_df$varra[i] = som_quality$err.varratio[1] |> as.numeric() |> round(2)
    quality_df$k_l[i] = som_quality$err.kaski[1] |> as.numeric() |> round(2)
    quality_df$topo[i] = som_quality$err.topo[1] |> as.numeric() |> round(2)
    message("... ", i, "/", n_clust, "/iter:", k)
  }
  
  write_rds(x = quality_df,
            file = paste0(here("data/som-iter/alternative-samples/som2/som2_alt_"), k, "_nclust_", n_clust, ".rds"))
  message("SOM iterations for nclust=", n_clust, " has completed")
  
}  
