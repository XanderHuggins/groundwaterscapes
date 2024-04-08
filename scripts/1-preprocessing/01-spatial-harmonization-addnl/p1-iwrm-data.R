
# import iwrm data portal
iwrm = readr::read_csv("D:/Geodatabase/Management/iwrm-data-portal-2020.csv")

# extract layers pertaining to groundwater explicitly
iwrm = iwrm |> 
  dplyr::select(Country, ISO3, sdg_score_2020, s1.2b, s2.2a, s3.2b) |> 
  rowwise() |> 
  mutate(gw_layers = mean(c(s1.2b, s2.2a, s3.2b), na.rm = T),
         gw_leading = gw_layers - sdg_score_2020)

# import rnaturalearth shapefile
nat_vect = rnaturalearth::ne_countries(scale = 10, returnclass = "sf") |> 
  terra::vect()

# missing from the rnaturalearth database 
nat_vect$iso_a3[nat_vect$sovereignt == "France"] = "FRA"
nat_vect$iso_a3[nat_vect$sovereignt == "Norway"] = "NOR"
nat_vect$iso_a3[nat_vect$sovereignt == "Somaliland"] = "SOM"

# check = nat_vect[nat_vect$sovereignt == "Norway"]
# view(check)

# merge together
nat_vect = merge(x = nat_vect, y = iwrm, by.x = "iso_a3", by.y = "ISO3")
nat_vect$gw_leading %<>% as.numeric()
nat_vect$gw_layers %<>% as.numeric()

# identify which countries are missing gw_layers
nat_vect_full = nat_vect[!is.nan(nat_vect$gw_layers)]
nat_vect_miss = nat_vect[is.nan(nat_vect$gw_layers)]

# create raster identifying these countries with missing IWRM data
iwrm_missing = terra::rasterize(x = nat_vect_miss, y = WGS84_areaRaster(5/60) |> rast(), values = 1, touches = T,
                                filename = here("data/input/iwrm-MISSING.tif"), overwrite = TRUE)

# Countries missing data: Canada, Argentina, Uruguay, Venezuela, Brunei, Eritrea, Djibouti
missing_iso = nat_vect_miss$iso_a3
full_iso = nat_vect_full$iso_a3

# import worldwide governance indicators
wgi = read_csv("D:/Geodatabase/Governance/wgi_2020.csv")
wgi_missing = wgi |> filter(Code %in% missing_iso)
wgi_full = wgi |> filter(Code %!in% missing_iso) |> filter(Code %in% nat_vect$iso_a3)

# For each missing country, find its closest neighbour in the wgi database
for (i in 1:length(missing_iso)) {
  
  # i = 1
  min_index = rdist.w.na(X = wgi_missing[i,] |> dplyr::select(!c('Country/Territory', 'Code')) |> as.matrix(),
                         Y = wgi_full |> dplyr::select(!c('Country/Territory', 'Code')) |> as.matrix()) |> which.min()
  
  mis_code = wgi_missing[i,]$Code
  rep_code = wgi_full[min_index,]$Code
  
  # assign 
  nat_vect$gw_layers[nat_vect$iso_a3 == mis_code] = nat_vect$gw_layers[nat_vect$iso_a3 == rep_code]
}

# check that all have been replaced
nat_vect[is.nan(nat_vect$gw_layers)] |> nrow() #> 0


nat_vect$gw_layers_norm = ( nat_vect$gw_layers - mean(nat_vect$gw_layers, na.rm = T) ) / sd(nat_vect$gw_layers, na.rm = T)


terra::writeVector(x = nat_vect,
                   filename = "D:/Geodatabase/Management/iwrm-sdg-2020.gpkg",
                   filetype = "GPKG",
                   overwrite = TRUE)

terra::rasterize(x = terra::vect("D:/Geodatabase/Management/iwrm-sdg-2020.gpkg"),
                 y = terra::rast(here("data/ggrid_5arcmin.tif")),
                 field = 'gw_layers',
                 touches = TRUE,
                 filename = here("data/input/iwrm-gw-layers.tif"),
                 overwrite = TRUE)

terra::rasterize(x = terra::vect("D:/Geodatabase/Management/iwrm-sdg-2020.gpkg"),
                 y = terra::rast(here("data/ggrid_5arcmin.tif")),
                 field = 'gw_layers_norm',
                 touches = TRUE,
                 filename = here("data/input/iwrm-gw-layers-norm.tif"),
                 overwrite = TRUE)

plot(terra::rast(here("data/input/iwrm-gw-layers-norm.tif")))
plot(terra::rast(here("data/input/iwrm-gw-layers.tif")))


# 