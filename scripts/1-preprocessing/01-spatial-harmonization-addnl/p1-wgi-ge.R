
# create governance effectiveness raster

# bring in country outlines
nat_vect = rnaturalearth::ne_countries(scale = 10, returnclass = "sf") |> 
  terra::vect()

# missing from the rnaturalearth database 
nat_vect$iso_a3[nat_vect$sovereignt == "France"] = "FRA"
nat_vect$iso_a3[nat_vect$sovereignt == "Norway"] = "NOR"
nat_vect$iso_a3[nat_vect$sovereignt == "Somaliland"] = "SOM"
nat_vect$iso_a3[nat_vect$sovereignt == "Democratic Republic of the Congo"] = "ZAR"
nat_vect$iso_a3[nat_vect$sovereignt == "Kazakhstan"] = "KAZ"
nat_vect$iso_a3[nat_vect$sovereignt == "Andorra"] = "ADO"
nat_vect$iso_a3[nat_vect$sovereignt == "Romania"] = "ROM"
nat_vect$iso_a3[nat_vect$sovereignt == "Kosovo"] = "KSV"

nat_vect$iso_a3[nat_vect$admin == "Aland"] = "FIN"

nat_vect$iso_a3[nat_vect$admin == "Palestine"] = "WBG"
nat_vect$iso_a3[nat_vect$admin == "Northern Cyprus"] = "CYP"
nat_vect$iso_a3[nat_vect$admin == "East Timor"] = "TMP"
nat_vect$iso_a3[nat_vect$admin == "Southern Patagonian Ice Field"] = "ARG"
nat_vect$iso_a3[nat_vect$admin == "Falkland Islands"] = "GBR"

nat_vect = nat_vect[,c("iso_a3")]


# import worldwide governance indicators
wgi = read_csv("D:/Geodatabase/Governance/wgi_2020.csv") |> 
  dplyr::select(c("Code", "GE"))

nat_vect = merge(x = nat_vect, y = wgi, by.x = "iso_a3", by.y = "Code")


# write to file
terra::writeVector(x = nat_vect,
                   filename = "D:/Geodatabase/Management/wgi_goveff_2020.gpkg",
                   filetype = "GPKG",
                   overwrite = TRUE)

terra::rasterize(x = terra::vect("D:/Geodatabase/Management/wgi_goveff_2020.gpkg"),
                 y = terra::rast(here("data/ggrid_5arcmin.tif")),
                 field = 'GE',
                 touches = TRUE,
                 filename = here("data/input/wgi_ge_2020.tif"),
                 overwrite = TRUE)

plot(terra::rast(here("data/input/wgi_ge_2020.tif")))
