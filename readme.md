# Groundwaterscapes code repository

This is the code repository associated with the manuscript: Huggins et al. (in revision @ Water Resources Research). Groundwaterscapes: A global classification and mapping of groundwater’s large-scale socioeconomic, ecological, and Earth system functions.  Preprint available at: https://doi.org/10.31223/X5M382

This repository contains all scripts necessary to preprocess input data and re-run the groundwaterscape derivation method. Some scripts were executed on clusters provided by the Digital Research Alliance of Canada, so this workflow is not set-up to be run on one's local computer. On this basis, this repository is best understood as a workbook that demonstrates all steps undertaken to generate study outcomes rather than.

**Workbook script folder structure**: <br/>
- `on_button.R`: calls the here() function and source runs scripts in `\00-steup` and `\0-functions` folders.
- `\00-steup`: loads packages, sets package options, and sets plotting themes.
- `\0-functions`: custom functions with explanatory names.
- `\scripts\1-preprocessing`:
  - contains preprocessing scripts to harmonize data to 5 arcminute resolution, found in subfolder `\01-spatial-harmonizattion-addnl`.
  -  creates a raster stack of harmonized input data, normalizes data, and checks for collinearity.
- `\scripts\2-analysis\a1-function-combinations`: matrix plots and maps of individual system functions as shown in Figure 2 in main text.
- `\scripts\2-analysis\a2-SOM-clustering`:
  - contains all self-organizing map scripts. A brief objective is provided at the top of each individual script in this folder.
  -  `s01-01-som1-iterations-syntheticspace.R` is written to be run on a cluster using `SOM-iter-array-syntheticspace.sl`
  -  `s02-01-som1-iterations-fullspace.R` is written to be run on a cluster using `SOM-iter-array-fullspace.sl`
- `\scripts\2-analysis\a3-landscape-metrics`: includes a script to landscape metrics of groundwaterscapes.
- `\scripts\2-analysis\a4-performance`: derives and maps grid cell residuals from groundwaterscape model.
- [datasources.md](https://github.com/XanderHuggins/gcs-archetypes/blob/master/datasources.md) : documentation and web-links to all data used in this study.
<br/>

Additional scripts, such as plotting scripts, can be made available upon request.

For any questions about this repository, please contact:
Xander Huggins
xanderhuggins@uvic.ca
<br/>

<p align="center">
  <img src="https://raw.githubusercontent.com/XanderHuggins/gcs-archetypes/master/assets/00_groundwaterscape_main_figure.png"
  width="100%"/>
</p>
<br/>
