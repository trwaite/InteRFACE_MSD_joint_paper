# InteRFACE MSD joint paper
This repository contains data and code to reproduce the analysis for the InteRFACE MSD joint paper. Inputs consist of various historical data and assumptions, which can be found in `historical_data`, as well as `rgcam` project files containing various queries from the GCAM runs, which can be found in `rgcam_data`. R scripts contain code to run the analysis for the three main workflows described below, saving results figures in `figures`.

## Global energy demand
The `analysis_global_demand` folder contains an R script to analyze trends in global primary energy by fuel and global oil consumption, based on queries in `rgcam_data/prj_year4_global`. 

## AK oil revenue
The `analysis_oil_revenue` folder contains two R scripts:
* `revenue_models.R`: derives the linear and exponential regression models used to estimate Alaskan oil revenue based on production and price. Saves model parameters to `model_coefs.csv`.
* `revenue_projections.R`: uses the models derived in `revenue_models.R` to project future Alaskan oil revenue based on projected production and price from GCAM-USA.

## Residential energy
The `analysis_resid_energy` folder contains the code and inputs to run the residential heating demand model for the case study regions (North Slope Borough and Northwest Arctic Borough). The `resid_energy_demand_inputs` folder contains both exogenous input data as well as data derived by the R scripts below:
* `get_borough_hddcdd.R`: processes the heating degree days (HDD) input data for the two boroughs.
* `calc_borough_pc_gdp.R`: uses historical borough population and GDP data, combined with GCAM projected per capita gdp for AK d1, to produce borough-level projected per capita GDP.
* `calc_heating_service_prices.R`: uses historical borough heating fuel price data and GCAM-USA base year refined liquids enduse prices to derive calibration adders for heating fuel prices in the NSB and NAB. Uses these adders to adjust GCAM-USA's projected AK residential heating (fuel furnace) service prices to get projected residential heating service prices for NSB and NAB.

`project_heating_service.R` uses outputs from the R scripts above, combined with additional exogenous inputs and assumptions, to calibrate and run the residential energy demand function for the NSB and NAB, resuling in projected heating demand, heating expenditures, and heating energy burden.

Lastly, `compare_GCAM_USA_satiation_level.R` contains code to compare the boroughs' heating satiation levels (based on `residenergy_demand_inputs/AHFC_2018)housing_assessment.csv`) to the satiation level assumed fro Alaska in GCAM-USA.
