library(tidyverse)

# data and assumptions ---------------------------------------------------------

# AK HDD in 2015 from GCAM-USA AK region
GCAM_AK_HDD <- 9687.966

# borough HDD
borough_base_HDD <- read_csv("resid_energy_demand_inputs/hdcd_borough.csv") %>%
  filter(year == 2015, scenario == "Ref") %>%
  select(-c(year, scenario, borough.cdd))

# assumed baseyear satiation gap for AK d1
assumed_AK_d1_baseyear_gap <- 0.3

# method 1 ---------------------------------------------------------------------

# AK satiation level from GCAM-USA AK region (resid heating modern)
# this is AK_baseyear_serv/AK_baseyear_flsp*1.05
# (from L244.ThermalServiceSatiation_gcamusa)
GCAM_AK_satiation_level <- 0.5304242


# adjust borough satiation level by comparing HDD to AK region
borough_satiation_level1 <- borough_base_HDD %>%
  mutate(satiation.level = GCAM_AK_satiation_level*borough.hdd/GCAM_AK_HDD)


# method 2 ---------------------------------------------------------------------

GCAM_AK_d1_base_service <- 0.000741511
GCAM_AK_d1_base_flsp <- 0.002026706

# calculate AK d1 satiation level based on the GCAM AK d1 base service per
# flsp and the assumed AK d1 base year satiation gap
AK_d1_satiation <- GCAM_AK_d1_base_service/GCAM_AK_d1_base_flsp*(1/(1-assumed_AK_d1_baseyear_gap))

# adjust borough satiation level by comparing HDD to AK region
borough_satiation_level2 <- borough_base_HDD %>%
  mutate(satiation.level = AK_d1_satiation*borough.hdd/GCAM_AK_HDD)


# method 3 ---------------------------------------------------------------------

# calculate borough satiation levels directly from borough base service
# and assumed AK d1 baseyear gap
