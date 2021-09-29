Discription of all necessary scripts used in figure creation and analysis.


~~ Scripts run on local machine ~~

1. Concurrent_vegetation_change.R
# Bivariate models and figures between greening variables and tree cover.
# Creates a tif of the greening index
# Examines coocurrence between greening metrics and tree cover (Figure 6
and S7)

2. RF_importance_figure_effectsize.R
# Creates importance plots with z-scored effect sizes (Figures 3, 5 and S6)

3. boruta_analysis.R
# Boruta variable selection algorithm

4. Grazing.R
# Creates grazing tif from Gilbert et al., 2018 data

5. NGP_AGDD_analysis_2.R
# Creates AGDD variables (historic, present, difference) from NEX data

6. NGP_pdif_mk_analysis.R
# NGP MODIS data change over time mann-kendall test for significance ##
# Creates change rasters

7. Pixel_stats.R
# Pixel Statistics for NGP, Greening and TC change
# Calculates changes in shrub vs glassland
 
8. Precipitation_variable_creator.R
# Creates all precipitation variables and precipitation CV

9. Predictor_var_500m_aggregation.R
# This script compiles all rasters into the data frame for the NGP random
forest analysis

10. Response_vars_500m_aggregation.R
# Creates the response var tifs for Tree Cover Change and Greening vars.

11. Soils.R
# Resamples soils data and produces soils tif

12. Temperature_variable_creator.R
# Creates all temperature variables except AGDD. See NGP_AGDD_analsis_2.R.

13. WGS_to_LAEA_conversion.R
# Simple script that converts WGS (un)projected rasters to LAEA projected
rasters.


~~ Scripts run on Hyalite Super Computure ~~
1. NGP_rf_analysis.R
# Script that runs all random forest analyses and exports RDS files


 
