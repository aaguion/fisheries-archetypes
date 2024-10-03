###### This repository contains R code and data for the LCCA model and figures created to characterize marine small-scale fisheries in Aguion et al. paper

###### The following R packages were used to analyse data and create figures
```r
install.packages(c("dplyr", "ggplot2", "gplots", "forcats", "scales", "svglite", "fmsb", "glca", "cluster", "stats", "dendextend", "reshape2"))

# **Data**
*Fig_ssf_definition.csv*: Percentage of marine ssf catch accounted for depending on the criteria used to define "small-scale" (Box Fig. 1)

*Fig_characterization_catch.csv*: Annual characterized catch (2013-2017) per country per total characterization score (i.e. degree of smallscaleness)

*Char_fishery_units.csv*: Values from the characterization matrix (13 attributes) for 1,255 fishery units from 43 countries

*Fig_national_archetypes.csv*: Characterized catch per fisheries archetype at subnational level for three countries (Kenya, Madagascar and Philippines)

# **R Scripts**
*Fig_ssf_definition*: Produce Fig. 1

*Fig_characterization_catch*: Calculate cumulative catch at global, regional and country levels per total characterization score. Produce Fig. 2

*LCCA_archetypes*: Multilevel Latent Class Model (LCCA) to identify fisheries archetypes at global, regional and national levels. Produce Fig. 3

*Fig_national_archetypes*: Produce Fig. 4
