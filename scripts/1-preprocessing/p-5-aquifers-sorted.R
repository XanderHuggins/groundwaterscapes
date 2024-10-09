### ---------------------\\ 
# Script objective:
# Sort WHYMAP aquifers alphabetically 
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

whymap = terra::vect(here("data/input/Major__Aquifers.shp"))

whymap = whymap[order(whymap$Aquifer_sy),]
whymap$IDalph = seq(1, nrow(whymap))

writeVector(x = whymap,
            filename = here("data/input/major_aquifers_sorted.sqlite"),
            filetype = "SQLite", overwrite = TRUE)