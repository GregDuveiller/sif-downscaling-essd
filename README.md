# sif-downscaling-essd

This repository includes R code associated with the following paper:

**Duveiller G., Filipponi F., Walther S., Köhler P., Frankenberg C., Guanter L., and Cescatti A.** 2020. A spatially downscaled sun-induced fluorescence global product for enhanced monitoring of vegetation productivity. _Earth System Science Data_. https://doi.org/10.5194/essd-2019-121

## Description
This repository includes the main script that performs the downscaling operation described in the paper. This consists in increasing the spatial resolution of the original GOME-2 sun induced fluorescence (SIF) from 0.5 dd to 0.05 dd using a semi-emprical regionalized model based on light-use effieciency calibrated with explanatory variables derived from the MODIS instrument. The final dataset that is described in the paper is stored in the JRC Data Catalog (https://data.jrc.ec.europa.eu/dataset/21935ffc-b797-4bee-94da-8fec85b3f9e1).

This repository also includes all the scripts needed to reproduce the plots in the paper. The summarized data behind these plots can be found in the following zenodo repository: 10.5281/zenodo.3753024

## Input data
The GOME-2 input files used in the study came from the following sources: 

ftp://fluo.gps.caltech.edu/data/Philipp/GOME-2/gridded_daily_corrected/

https://avdc.gsfc.nasa.gov/pub/data/satellite/MetOp/GOME_F/

The MODIS explanatory variables are stored in the LPDAAC
https://lpdaac.usgs.gov/

The study also required data from OCO-2 and TROPOMI that had to be aggregated in a particular format and are available in this zenodo repository. 10.5281/zenodo.3753024
