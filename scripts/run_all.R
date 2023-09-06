
#    Title: Global groundwater archetypes
#     Author: Xander Huggins
#     Affiliations: 
#                  1. University of Victoria (Canada)
#                  2. Global Institute for Water Security (Canada)
#                  3. International Institute for Applied Systems Analysis (Austria)
#     Last update: 05 September 2023

## note:
#    the purpose of this list of source scripts is not for users to run this script as a stand-alone script.
#    INSTEAD, consider this script as providing an overview of the sequence in which the scripts need to be 
#    executed, and this list of source scripts allows  individual 'task' to be run sequentially in a common script


# Set working directory to the location of the project
library(here)

# Source setup scripts, including: wd args, plotting themes, custom functions, etc.
invisible(sapply(list.files(here("scripts/00-setup"), full.names = T), source)) 

#/-----------------------------------------------------------------#
#/ PREPARE DATA                                             -------#

# Generate global grid with cell values representing surface area in km2
source(here("scripts/1-preprocessing/p-1-glob-grids.R"))

# Spatially harmonize data inputs and normalise data layers
source(here("scripts/1-preprocessing/p-2-input-data-spatial-harmonization.R"))

# Develop core raster stack of input data 
source(here("scripts/1-preprocessing/p-3-input-data-stack.R"))

# Assess collinearity of data inputs
source(here("scripts/1-preprocessing/p-4-input-data-collinearity.R"))

# Sort aquifer vector file
source(here("scripts/1-preprocessing/p-5-aquifers-sorted.R"))


#/----------------------------------------------------------------#
#/ DATA EXPLORATION                                         ------#

# Determine unique function combination codes
source(here("scripts/2-analysis/a1-function-combinations/a1-1-subdomain-overspecified-archetypes.R"))


#/----------------------------------------------------------------#
#/ SEQUENTIAL 2-STAGE SOM                                   ------#

# Sample input 
source(here("scripts/2-analysis/a2-SOM-archetype-derivation/a2-1-sampling-som-input.R"))

# First-stage SOM 
source(here("scripts/2-analysis/a2-SOM-archetype-derivation/a2-2-first-stage-som.R"))

# Second-stage SOM 
source(here("scripts/2-analysis/a2-SOM-archetype-derivation/a2-3-second-stage-som.R"))

# Archetype re-ordering to fit description+colour palette
source(here("scripts/2-analysis/a2-SOM-archetype-derivation/a2-4-archetype-ordering.R"))

# Mapping archetypes back to grid cells
source(here("scripts/2-analysis/a2-SOM-archetype-derivation/a2-5-membership-classification.R"))


#/----------------------------------------------------------------#
#/ LANDSCAPE METRICS                                        ------#

# Landscape metrics of archetypes
source(here("scripts/2-analysis/a3-landscape-metrics/a3-1-landscape-metrics.R"))


#/----------------------------------------------------------------#
#/ GRID CELL RESIDUALS                                      ------#

# Grid cell archetype residuals 
source(here("scripts/2-analysis/a4-performance/a4-01-archetypes-representativeness.R"))


#/----------------------------------------------------------------#
#/ ARCHETYPE ROBUSTNESS                                     ------#

# Reproducing first-stage SOM, and identifying best-performing iteration 
source(here("scripts/2-analysis/a4-performance/a4-02-01-robustness-alternative-first-stage-soms.R"))
source(here("scripts/2-analysis/a4-performance/a4-02-02-best-performing-first-stage-soms.R"))

# Reproducing second-stage SOM, and identifying best-performing iteration 
source(here("scripts/2-analysis/a4-performance/a4-02-03-alternative-second-stage-soms.R"))
source(here("scripts/2-analysis/a4-performance/a4-02-04-best-performing-second-stage-soms.R"))

# Map alternative archetypes back to grid cells 
source(here("scripts/2-analysis/a4-performance/a4-02-05-alternative-samples-archetype-mapping.R"))

# Evalaute robustness of archetypes to samples 
source(here("scripts/2-analysis/a4-performance/a4-02-06-archetype_robustness_results.R"))
