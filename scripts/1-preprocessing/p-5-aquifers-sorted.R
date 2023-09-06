# order aquifers alphabetically

whymap = terra::vect(here("data/input/Major__Aquifers.shp"))

head(whymap)

whymap = whymap[order(whymap$Aquifer_sy),]
whymap$IDalph = seq(1, nrow(whymap))

writeVector(x = whymap,
            filename = here("data/input/major_aquifers_sorted.sqlite"),
            filetype = "SQLite", overwrite = TRUE)
