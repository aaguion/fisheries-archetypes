This repository contains R code and data for the LCCA model and figures to characterize marine small-scale fisheries in Aguion et al. paper (In review)

The following R packages were used to analyse data and create figures
```r
install.packages(c("dplyr", "ggplot2", "gplots", "forcats", "scales", "svglite", "fmsb", "glca", "cluster", "stats", "dendextend", "reshape2"))
```

# **Data**
###### *Fig_ssf_definition.csv*: Percentage of marine ssf catch accounted for depending on the criteria used to define "small-scale"

###### *Fig_characterization_catch.csv*: Annual characterized catch (2013-2017) per country per total characterization score

###### *Char_fishery_units.csv*: Characterization values for 1,255 fishery units from 43 countries using FAO matrix

###### *Fig_national_archetypes.csv*: Characterized catch per fisheries archetype at subnational level for three countries 

# **R Scripts**
###### *Fig_ssf_definition.R*: Fig. 1

###### *Fig_characterization_catch.R*: Calculate cumulative catch at global, regional and country levels per total characterization score. Fig. 2

###### *LCCA_archetypes.R*: Multilevel Latent Class Model (LCCA) to identify fisheries archetypes at global, regional and national levels. Fig. 3

###### *Fig_national_archetypes.R*: Fig. 4


This is a product of the Illuminating Hidden Harvests Research Initiative.
