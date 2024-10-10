# Groundwaterscapes code repository

[![](<https://img.shields.io/badge/Dataverse DOI-10.5683/SP3/MFYCWV-yellow>)](https://doi.org/10.5683/SP3/MFYCWV)

This is the code repository associated with the manuscript: Huggins et al. (2024). **Groundwaterscapes: A global classification and mapping of groundwater’s large-scale socioeconomic, ecological, and Earth system functions**.  *Water Resources Research*. (in press).

This repository includes all scripts necessary to preprocess input data and reproduce the groundwaterscape map. Some scripts have been executed on clusters provided by the Digital Research Alliance of Canada and will not be fully executable on local machines.

**`\Scripts` folder sub-structure**: <br/>
- `on_button.R`: Calls the `here()` function and sources scripts in the `\00-setup` and `\0-functions` folders.
- `\0-functions`: Contains custom functions with explanatory names.
- `\00-steup`: Loads required packages, sets package options, and configures plotting themes.
- `\1-preprocessing`: 
  -  Contains preprocessing scripts to harmonize data to a 5-arcminute resolution. This folder includes the subfolder `\01-spatial-harmonizattion-addnl`.
  -  Creates a raster stack of harmonized input data, normalizes the data, and checks for collinearity.
- `\2-analysis\a1-function-combinations`:
  -  Contains matrix plots and maps of individual system functions as shown in Figure 2 of the main text.
- `\2-analysis\a2-SOM-clustering`:
  -  Contains all scripts related to self-organizing maps (SOM). Each script includes a brief objective at the top.
  -  Note: `a2-1-1-som1-iterations-syntheticspace.R` is written to run on a cluster using `SOM-iter-array-syntheticspace.sl`
  -  Note: `a2-2-1-som1-iterations-fullspace.R` is written to run on a cluster using `SOM-iter-array-fullspace.sl`
- `\2-analysis\a3-poshoc\`:
  -  Includes a script that calculates landscape metrics of groundwaterscapes within aquifers.
  -  Calculates grid cell residuals from the local groundwaterscape model.
<br/>

Additional scripts, such as for plotting, can be made available upon request.

For any questions about this repository, please contact: <br/>
Xander Huggins <br/>
xander.huggins@ubc.ca <br/>
<br/>

<p align="center">
  <img src="https://raw.githubusercontent.com/XanderHuggins/gcs-archetypes/master/assets/00_groundwaterscape_main_figure.png"
  width="100%"/>
</p>
<br/>
