
# Import all data
id_ras = terra::rast(here("data/earth_mask_5arcmin.tif"))
id_ras[] = 1:ncell(id_ras)

# Create raster stack of all inputs and mask to land areas
data_stack = c(
  terra::rast(here("data/input/wtr_5arcmin_aridmask.tif")),
  terra::rast(here("data/input/porosity_5arcmin.tif")),
  terra::rast(here("data/input/gde-density.tif")) |> na_cells_to_val(value = -1), 
  terra::rast(here("data/input/field_size_modal_rcl_5arcmin.tif")) |> na_cells_to_val(value = -1), 
  terra::rast(here("data/input/gmia_aeigw_pct_aei.tif")) |> na_cells_to_val(value = -1), 
  terra::rast(here("data/input/iwrm-gw-layers-norm.tif")),
  terra::rast(here("data/input/udw_raw.tif")) |> na_cells_to_val(value = -1),
  id_ras) |> 
  terra::mask(mask = terra::rast(here("data/earth_mask_5arcmin.tif")),
              maskvalues = c(0,2,3))

complete_coverage = rast(here("data/complete_coverage.tif"))
data_stack[complete_coverage == 0] = NA

## --------------------------- \
# Earth system classifications

# 1/ water table ratio
# data_stack[[1]][data_stack[[1]] == -9] = 
plot(data_stack[[1]])

rcl.m = c(-Inf, -2, 1,
          -2, -1, 2,
          -1, 0, 3,
          0, 1, 4,
          1, 2, 5,
          2, Inf, 6) |> 
  matrix(ncol = 3, byrow = TRUE)

wtr_class = terra::classify(x = data_stack[[1]], rcl = rcl.m, include.lowest = TRUE)
plot(wtr_class)

# 2/ porosity    
data_stack[[2]][data_stack[[2]] == 0] = NA
plot(data_stack[[2]])

rcl.m = c(0, 0.05, 1,
          0.05, 0.10, 2,
          0.10, 0.15, 3,
          0.15, 0.20, 4,
          0.20, 0.25, 5,
          0.25, Inf, 6) |> 
  matrix(ncol = 3, byrow = TRUE)

por_class = terra::classify(x = data_stack[[2]], rcl = rcl.m, include.lowest = TRUE)
plot(por_class)

earth_matrix = (10*wtr_class) + por_class
plot(earth_matrix)
earth_matrix[] |> unique() |> length()

## --------------------------- \
# Ecosystem classifications

rcl.m = c(-Inf, 0.001, 1,
          0.001, 0.2, 2,
          0.2, 0.4, 3,
          0.4, 0.6, 4,
          0.6, 0.8, 5,
          0.8, Inf, 6) |> 
  matrix(ncol = 3, byrow = TRUE)

# 3/ terrestrial GDEs
gde_t_class = terra::classify(x = data_stack[[3]], rcl = rcl.m, include.lowest = TRUE)
plot(gde_t_class)

# 4/ aquatic GDEs
gde_a_class = terra::classify(x = data_stack[[4]], rcl = rcl.m, include.lowest = TRUE)
plot(gde_a_class)

gde_matrix = (10*gde_t_class) + gde_a_class
plot(gde_matrix)
gde_matrix[] |> unique() |> length()


## --------------------------- \
# Food system classifications

plot(data_stack[[5]])

rcl.m = c(-Inf, 0.5, 1,
          0.5, 1.5, 2,
          1.5, 2.5, 3,
          2.5, 3.5, 4, 
          3.5, 4.5, 5,
          4.5, Inf, 6) |> 
  matrix(ncol = 3, byrow = TRUE)

# 5/ field size
fsize_class = terra::classify(x = data_stack[[5]], rcl = rcl.m, include.lowest = TRUE)
plot(fsize_class)

rcl.m = c(-Inf, 0.001, 1,
          0.001, 0.2, 2,
          0.2, 0.4, 3,
          0.4, 0.6, 4,
          0.6, 0.8, 5,
          0.8, Inf, 6) |> 
  matrix(ncol = 3, byrow = TRUE)

# 6/ area irrigated with groundwater
aeigw_class = terra::classify(x = data_stack[[6]]/100, rcl = rcl.m, include.lowest = TRUE)
plot(aeigw_class)

foodsys_matrix = (10*aeigw_class) + fsize_class
plot(foodsys_matrix)
foodsys_matrix[] |> unique() |> length()

## --------------------------- \
# Governance classifications

iwrm_rast = raster(here("data/input/iwrm-gw-layers.tif"))
rcl.m = c(-Inf, 10, 1,
          10, 30, 2,
          30, 50, 3,
          50, 70, 4,
          70, 90, 5,
          90, Inf, 6) |> 
  matrix(ncol = 3, byrow = TRUE)

iwrm_class = terra::classify(x = rast(iwrm_rast), rcl = rcl.m, include.lowest = TRUE)
plot(iwrm_class)

plot(data_stack[[8]])
rcl.m = c(-Inf, 0.025, 1,
          0.025, 0.05, 2,
          0.05, 0.10, 3,
          0.10, 0.20, 4,
          0.20, 0.40, 5,
          0.40, Inf, 6) |> 
  matrix(ncol = 3, byrow = TRUE)

udw_class = terra::classify(x = data_stack[[8]], rcl = rcl.m, include.lowest = TRUE)
plot(udw_class)

gov_matrix = (10*iwrm_class) + udw_class
plot(gov_matrix)

# unique count
gov_matrix[] |> unique() |> length()

## --------------------------- \
# Stack of sub-domain matrices

matrix_stack = c(earth_matrix, gde_matrix, foodsys_matrix, gov_matrix) |> 
  terra::mask(mask = terra::rast(here("data/earth_mask_5arcmin.tif")),
              maskvalues = c(0,2,3))

# mask-out below 60 degrees South
Sof60r = rast(x = rast(WGS84_areaRaster(5/60)), vals = 0)
Sof60r[1800:2160,] = 1 # rows that correspond with areas south of 60S
matrix_stack[Sof60r == 1] = NA

all_combined = (1e6*matrix_stack[[1]]) + (1e4*matrix_stack[[2]]) + (1e2*matrix_stack[[3]]) + matrix_stack[[4]]
matrix_stack = c(matrix_stack, all_combined)
names(matrix_stack) = c('earth', 'eco', 'food', 'gov', 'all')
plot(matrix_stack[[5]])

matrix_stack[[5]][is.na(matrix_stack[[1]]) | is.na(matrix_stack[[2]]) | is.na(matrix_stack[[3]]) | is.na(matrix_stack[[4]])] = NA

# write stack to file
terra::writeRaster(x = matrix_stack, 
                   filename = here("data/matrix_stack.tif"),
                   overwrite = TRUE,
                   wopt=list(datatype="FLT8S")) # need this type to preserve all 8 digits in unique matrix ID, else trimmed to 6 sigfig

## --------------------------- \
# identify number of unique matrix combinations
matrix_stack$all |> unique() |> nrow() # 79,177 unique combinations... 

# sanity check that writing to file preserves complete ID 
matrix_stack2 = rast(here("data/matrix_stack.tif"))
matrix_stack2$all |> unique() |> nrow() # 79,177 unique combinations... 
freq_df = matrix_stack2[[5]] |> as.vector() |> table() |> as.data.frame() 
freq_df |> filter(Freq >= 1) |> nrow()
freq_df |> filter(Freq > 1) |> nrow()

## --------------------------- \
## old scripts below -- ignore

# # determine frequency of each combination
# freq_df = matrix_stack[[5]] |> as.vector() |> table() |> as.data.frame() 
# freq_df |> filter(Freq >= 1) |> nrow()
# freq_df |> filter(Freq > 1) |> nrow()
