terra::resample(x = terra::rast("D:/Geodatabase/Agriculture/Global_field_sizes/dominant_field_size_categories.tif"),
                y = terra::rast(here("data/ggrid_5arcmin.tif")),
                method = "mode",
                threads = TRUE,
                filename = here("data/input/field_size_modal_5arcmin.tif"), overwrite = TRUE)

rasterDT::subsDT(x = raster(here("data/input/field_size_modal_5arcmin.tif")),
                 dict = data.frame(from = c(3507,  3506, 3505, 3504, 3503, 3502),
                                   to   = c(   0,     1,    2,    3,    4,    5)),
                 filename = here("data/input/field_size_modal_rcl_5arcmin.tif"))