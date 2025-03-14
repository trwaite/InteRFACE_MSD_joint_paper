library(tidyverse)

# input data
input_data_dir <- "analysis_resid_energy/resid_energy_demand_inputs"

# location of GCAM database
db_path <- "my_db_path"
# names of GCAM databases
db_name <- "my_db_name"

# conversions
diesel_gal_to_Btu <- 128488
Btu_to_GJ <- 1.055e-6
tonne_per_kg <- 10^-3

# assumed base year heating satiation levels
# (see resid_energy_demand_inputs/AHFC_2018_housing_assessment.xlsx)
NSB_satiation_level <- 1.018
NAB_satiation_level <- 0.908

# whether to reread GCAM results from a database
# (if not, use existing rgcam project file)
reread_db <- F

# load inputs ------------------------------------------------------------------

## GCAM outputs and gcamdata inputs ============================================

if(reread_db){
  conn <- rgcam::localDBConn(db_path, db_name)
  prj_resid <- rgcam::addScenario(conn, "prj_resid_energy",
                                  "queries/resid_energy_queries.xml")
} else{
  prj_resid <- rgcam::loadProject("rgcam_data/prj_resid_energy")
}


# read in shell conductance and floor to surface ratio from gcamdata input
shell_conduct_floor_ratio <- as_tibble(read.csv(paste0(input_data_dir,
                                                       "/L244.ShellConductance_bld_gcamusa.csv"),
                                                skip = 1, blank.lines.skip = TRUE)) %>%
  filter(nodeInput=='resid') %>%
  separate(gcam.consumer,c('bld','group'),'_') %>%
  select(region,group,year,shell.conductance,floor.to.surface.ratio)

# read in internal gains scalars from gcamdata input
internal_gain_scaler <- as_tibble(read.csv(paste0(input_data_dir,
                                                  "/L244.Intgains_scalar_gcamusa.csv"),
                                           skip = 1, blank.lines.skip = TRUE)) %>%
  filter(nodeInput=='resid', region == "AK") %>%
  separate(thermal.building.service.input,c('service','group'),'_') %>%
  filter(service == "resid heating modern") %>%
  select(region,service,group,internal.gains.scalar)

# get floorspace for AK d1
flsp <- rgcam::getQuery(prj_resid, "building floorspace") %>%
  separate(building, into = c("building", "group"), sep = "_") %>%
  filter(grepl("resid", building), group == "d1") %>%
  select(Units, scenario, region, group, year, flsp = value)

# get internal gains for AK d1
internal_gain <- rgcam::getQuery(prj_resid, "building internal gain energy by service") %>%
  filter(grepl("resid", sector), grepl("_d1", sector), !grepl("_d10", sector))

## inputs calculated in other scripts ==========================================
source("analysis_resid_energy/calc_borough_pc_gdp.R")
source("analysis_resid_energy/get_borough_hddcdd.R")
source("analysis_resid_energy/calc_heating_service_prices.R")

# read in inputs calculated from above scripts
borough_pc_gdp <- read_csv("analysis_resid_energy/resid_energy_demand_inputs/borough_gdppc_scaled.csv")
borough_hddcdd <- read_csv("analysis_resid_energy/resid_energy_demand_inputs/HDDCDD_borough_all.csv")
borough_heat_price <- read_csv("analysis_resid_energy/resid_energy_demand_inputs/price_fuel_furnace.csv")
fuel_adders <- read_csv("analysis_resid_energy/resid_energy_demand_inputs/fuel_adders_final.csv")


# run heating demand model -----------------------------------------------------

## calculate internal gains per unit floorspace ================================

internal_gain_per_flsp <- internal_gain %>%
  separate(sector, into = c("service", "group"), sep = "_") %>%
  group_by(Units, scenario, region, group, year) %>%
  summarize(gains = sum(value)) %>%
  ungroup() %>%
  left_join(flsp, by = c("scenario", "region", "group", "year")) %>%
  mutate(gains_per_flsp = gains/flsp,
         Units = "EJ/billion m^2") %>%
  select(scenario, region, group, year, gains_per_flsp, Units)


## calculate affordability =====================================================

# affordability = pcGDP/price
affordability <- borough_heat_price %>%
  filter(year >= 2015, year <= 2075) %>%
  left_join(borough_pc_gdp,
            by = c("scenario", "year", "group", "region")) %>%
  mutate(afford = gdppc.grp/value) %>%
  select(-c(value, gdppc.grp, Units.x, Units.y))


## calculate thermal load ======================================================

# thermal load = HDDCDD*shell.conductance*fts.ratio + int.gain.scalar*int.gain.per.flsp
thermal_load <- internal_gain_per_flsp %>%
  select(-region) %>%
  gcamdata::repeat_add_columns(tibble(region = c("North Slope Borough",
                                                 "Northwest Arctic Borough"))) %>%
  left_join(internal_gain_scaler %>% select(-region),
            by = c('group')) %>%
  left_join(shell_conduct_floor_ratio %>% filter(region == "AK") %>% select(-region),
            by = c('year', 'group')) %>%
  mutate(CI = case_when(grepl("Ref_e", scenario) ~ "Ref",
                        grepl("LowC_e", scenario) ~ "LowC",
                        T ~ "none")) %>%
  filter(year >= 2015, year <= 2070) %>%
  left_join(borough_hddcdd, by = c("region", "service", "year", "CI")) %>%
  mutate(thermal.load =
           degree.days*shell.conductance*floor.to.surface.ratio +
           internal.gains.scalar*gains_per_flsp) %>%
  select(scenario, CI, region, group, year, service, thermal.load)


## calibrate service demand function ===========================================

# coef = 1/base_thermal_load
coef <- thermal_load %>%
  filter(year == 2015) %>%
  mutate(coef = 1/thermal.load) %>%
  select(-c(thermal.load, group, service, year))

# satiation impedance as a function of affordability and
# assumed base year satiation gap
# apply NAB (unsubsidized) impedance to both boroughs
sat_impedance <- affordability %>%
  filter(year == 2015, region == "Northwest Arctic Borough") %>%
  mutate(base_year_gap = 0.3,
         satiation.impedance = (-log(2)*afford)/log(base_year_gap)) %>%
  select(scenario, satiation.impedance) %>%
  gcamdata::repeat_add_columns(tibble(region = c("North Slope Borough",
                                                 "Northwest Arctic Borough")))

# satiation level
sat_level <-
  # start with affordability table to get all scenario-region combinations
  affordability %>%
  filter(year == 2015) %>%
  mutate(satiation.level = if_else(region == "North Slope Borough",
                                   NSB_satiation_level,
                                   NAB_satiation_level)) %>%
  select(scenario, region, satiation.level)


## project future service demand ===============================================

# per capita demand
serv_den_calc_heating <- affordability %>%
  left_join(thermal_load,
            by = c("scenario", "service", "group", "year", "region"))  %>%
  left_join(coef, by = c('scenario', 'region', 'CI')) %>%
  left_join(sat_impedance, by = c("scenario", "region")) %>%
  left_join(sat_level, by = c("scenario", "region")) %>%
  mutate(econ.eff = 1-exp(-log(2)/satiation.impedance*afford),
         serv.den.calc = coef*thermal.load*econ.eff*satiation.level,
         tl_coef = thermal.load*coef) %>%
  select(scenario, CI, region, service, group, year, serv.den.calc)


## calculate per capita demand =================================================

# population
pop <- rgcam::getQuery(prj_resid, "population by region")

# calculate per capita floorspace
pc_flsp <- flsp %>%
  left_join(pop, by = c("scenario", "region", "year")) %>%
  # multiply by 1000 (as orig units are thousands)
  # and divide by 10 (to split up income deciles)
  mutate(population = value*100,
         pc_flsp = flsp/population*10^9,
         units = "m^2/person") %>%
  select(scenario, region, group, year, pc_flsp, units)

# per capita heating demand
pc_heating_demand <- serv_den_calc_heating %>%
  left_join(pc_flsp %>% select(-region), by = c("scenario", "group", "year")) %>%
  mutate(pc_serv = serv.den.calc*pc_flsp) %>%
  select(scenario, CI, region, group, service, year, pc_serv)



## calculate per capita expenditures and heating energy burden =================

# expenditures: multiply per capita demand by heating service price
heating_expenditures <- pc_heating_demand %>%
  left_join(borough_heat_price,
            by = c("scenario", "region", "group", "year")) %>%
  mutate(expenditure = pc_serv*value) %>%
  separate(scenario, into = c("scenario", "ens"), sep = "_") %>%
  filter(year >= 2015, year <= 2070,
         group == "d1")

# burden: divide per capita expenditures by per capita income
heating_burden <- heating_expenditures %>%
  left_join(borough_pc_gdp,
            by = c("scenario", "region", "group", "year")) %>%
  mutate(burden = expenditure/gdppc.grp) %>%
  separate(scenario, into = c("scenario", "ens"), sep = "_") %>%
  filter(year >= 2015, year <= 2070,
         group == "d1")


# make figures -----------------------------------------------------------------

## floorspace ==================================================================
# AK d1 projected per-capita floorspace demand
pc_flsp %>%
  filter(scenario == "Ref", year >= 2015) %>%
  ggplot(aes(x = year, y = pc_flsp)) +
  geom_line() +
  theme_bw() +
  xlab("") + ylab("Per-capita floorspace (m^2)")

ggsave("figures/pc_flsp.png", width = 7, height = 4, units = "in")


## HDD =========================================================================

HDDCDD_borough_plot <- borough_hddcdd %>%
  filter(grepl("heating", service)) %>%
  mutate(scenario = case_when(CI == "Ref" ~ "Ref|7.0",
                              CI == "LowC" ~ "LowC|2.6",
                              T ~ "Ref|static")) %>%
  # add in LowC|static
  rbind(borough_hddcdd %>% filter(grepl("heating", service),
                                  CI == "none") %>%
          mutate(scenario = "LowC|static"))

HDDCDD_borough_plot$scenario <-
  factor(HDDCDD_borough_plot$scenario,
         levels = c("Ref|static", "LowC|static", "Ref|7.0", "LowC|2.6"))

# HDD/CDD by borough and scenario
HDDCDD_borough_plot %>%
  ggplot(aes(x = year, y = degree.days, color = scenario, lty = scenario)) +
  geom_line() +
  scale_x_continuous(breaks = c(2020, 2040, 2060)) +
  scale_color_manual(values = jgcricolors::jgcricol()$pal_16[c(2,3,2,3)],
                     name = "Scenario") +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"),
                        name = "Scenario") +
  facet_wrap(~region) +
  theme_bw() +
  xlab("") + ylab("HDD")

ggsave("figures/borough_HDD_CDD.png", width = 7, height = 3, units = "in")



## fuel prices =================================================================

# to get price of just fuel (not whole service), need to re-apply borough
# adders to refined liquids enduse price
ref_liq_price <- rgcam::getQuery(prj_resid, "refined liquids enduse price")

# add borough adders
ref_liq_price_borough <- ref_liq_price %>%
  gcamdata::repeat_add_columns(tibble(region = c("North Slope Borough",
                                                 "Northwest Arctic Borough"))) %>%
  left_join(fuel_adders, by = "region") %>%
  mutate(value = value + adder) %>%
  select(-adder)

# add carbon price
c_price <- rgcam::getQuery(prj_resid, "CO2 prices")

# apply to refined liquids enduse, which has C coef of 19.6 kg C per GJ
c_price_fuel <- c_price %>%
  mutate(c_price = value*extended_gdp_deflator(1975, 1990)*tonne_per_kg*19.6) %>%
  select(scenario, year, c_price)

total_price_fuel <- ref_liq_price_borough %>%
  left_join(c_price_fuel, by = c("scenario", "year")) %>%
  replace_na(list(c_price = 0)) %>%
  mutate(value = value + c_price)

# convert units
ref_liq_price_plot <- total_price_fuel %>%
  filter(year >= 2015, year <= 2075) %>%
  mutate(ens = case_when(grepl("Ref_e", scenario) ~ gsub("Ref_", "", scenario),
                         grepl("LowC_e", scenario) ~ gsub("LowC_", "", scenario),
                         T ~ "none"),
         scenario = case_when(grepl("Ref_e", scenario) ~ "Ref|7.0",
                              grepl("LowC_e", scenario) ~ "LowC|2.6",
                              grepl("Ref", scenario) ~ "Ref|static",
                              T ~ "LowC|static"),
         value = value*extended_gdp_deflator(2015, 1975)*diesel_gal_to_Btu*Btu_to_GJ,
         units = "2015$/gallon")

# reorder/ rename scenarios
ref_liq_price_plot$scenario <-
  factor(ref_liq_price_plot$scenario,
         levels = c("Ref|static", "LowC|static", "Ref|7.0", "LowC|2.6"))

# plot
ggplot() +
  geom_point(data = hist_heat_fuel_price_avg %>%
               mutate(region = case_when(borough == "NAB" ~ "Northwest Arctic Borough",
                                         borough == "NSB" ~ "North Slope Borough",
                                         T ~ borough)),
             aes(x = ReportingYear, y = median_price)) +
  geom_line(data = ref_liq_price_plot,
            aes(x = year, y = value, color = scenario, lty = scenario)) +
  scale_color_manual(values = jgcricolors::jgcricol()$pal_16[c(2,3,2,3)],
                     name = "Scenario") +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"),
                        name = "Scenario") +
  expand_limits(y = 0) +
  facet_wrap(~region) +
  theme_bw() +
  xlab("") + ylab("2015$/gallon")

ggsave("figures/borough_HF_price.png", width = 7, height = 3, units = "in")



## thermal load ================================================================

thermal_load_plot <- thermal_load %>%
  filter(year >= 2015, year <= 2070,
         group == "d1", service == "resid heating modern") %>%
  mutate(ens = case_when(grepl("Ref_e", scenario) ~ gsub("Ref_", "", scenario),
                         grepl("LowC_e", scenario) ~ gsub("LowC_", "", scenario),
                         T ~ "none"),
         scenario = case_when(grepl("Ref_e", scenario) ~ "Ref|7.0",
                              grepl("LowC_e", scenario) ~ "LowC|2.6",
                              grepl("Ref", scenario) ~ "Ref|static",
                              T ~ "LowC|static"))

thermal_load_plot$scenario <-
  factor(thermal_load_plot$scenario,
         levels = c("Ref|static", "LowC|static", "Ref|7.0", "LowC|2.6"))

thermal_load_plot %>%
  ggplot(aes(x = year, y = thermal.load, color = scenario,
             lty = scenario)) +
  geom_line() +
  expand_limits(y = 0) +
  scale_color_manual(values = jgcricolors::jgcricol()$pal_16[c(2,3,2,3)],
                     name = "Scenario") +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"),
                        name = "Scenario") +
  facet_wrap(~region) +
  theme_bw() +
  xlab("Year") + ylab("Thermal load")

ggsave("figures/thermal_load.png", width = 8, height = 4, units = "in")



## per floorspace heating demand ===============================================

# rename/ reorder scenarios
pf_heating_demand_plot <- serv_den_calc_heating %>%
  mutate(ens = case_when(grepl("Ref_e", scenario) ~ gsub("Ref_", "", scenario),
                         grepl("LowC_e", scenario) ~ gsub("LowC_", "", scenario),
                         T ~ "none"),
         scenario = case_when(grepl("Ref_e", scenario) ~ "Ref|7.0",
                              grepl("LowC_e", scenario) ~ "LowC|2.6",
                              grepl("Ref", scenario) ~ "Ref|static",
                              T ~ "LowC|static")) %>%
  filter(year >= 2015, year <= 2070,
         group == "d1")

pf_heating_demand_plot$scenario <-
  factor(pf_heating_demand_plot$scenario,
         levels = c("Ref|static", "LowC|static", "Ref|7.0", "LowC|2.6"))

# plot
pf_heating_demand_plot %>%
  ggplot(aes(x = year, y = serv.den.calc, color = scenario, lty = scenario)) +
  geom_line() +
  expand_limits(y = 0) +
  scale_color_manual(values = jgcricolors::jgcricol()$pal_16[c(2,3,2,3)],
                     name = "Scenario") +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"),
                        name = "Scenario") +
  facet_wrap(~region, nrow = 1) +
  theme_bw() +
  ylab("Per floorspace heating demand (GJ/m^2)")

ggsave("figures/pflsp_demand.png", width = 8, height = 4, units = "in")



## per capita heating demand ===================================================

# rename/reorder scenarios
pc_heating_demand_plot <- pc_heating_demand %>%
  mutate(ens = case_when(grepl("Ref_e", scenario) ~ gsub("Ref_", "", scenario),
                         grepl("LowC_e", scenario) ~ gsub("LowC_", "", scenario),
                         T ~ "none"),
         scenario = case_when(grepl("Ref_e", scenario) ~ "Ref|7.0",
                              grepl("LowC_e", scenario) ~ "LowC|2.6",
                              grepl("Ref", scenario) ~ "Ref|static",
                              T ~ "LowC|static")) %>%
  filter(year >= 2015, year <= 2070,
         group == "d1")

pc_heating_demand_plot$scenario <-
  factor(pc_heating_demand_plot$scenario,
         levels = c("Ref|static", "LowC|static", "Ref|7.0", "LowC|2.6"))

pc_heating_demand_plot %>%
  ggplot(aes(x = year, y = pc_serv, color = scenario, lty = scenario)) +
  geom_line() +
  #ylim(c(0, 80)) +
  expand_limits(y = 0) +
  scale_color_manual(values = jgcricolors::jgcricol()$pal_16[c(2,3,2,3)],
                     name = "Scenario") +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"),
                        name = "Scenario") +
  facet_wrap(~region, nrow = 1) +
  theme_bw() +
  ylab("Per capita heating demand (GJ)") + xlab("")

ggsave("figures/pc_heating_demand.png", width = 7, height = 3, units = "in")



## heating expenditures ========================================================

heating_expenditures_plot <- heating_expenditures %>%
  mutate(scenario = case_when(scenario == "Ref" & CI == "none" ~ "Ref|static",
                              scenario == "Ref" ~ "Ref|7.0",
                              CI == "none" ~ "LowC|static",
                              T ~ "LowC|2.6")) %>%
  filter(year >= 2015, year <= 2070,
         group == "d1")

heating_expenditures_plot$scenario <-
  factor(heating_expenditures_plot$scenario,
         levels = c("Ref|static", "LowC|static", "Ref|7.0", "LowC|2.6"))

# plot
heating_expenditures_plot %>%
  ggplot(aes(x = year, y = expenditure*extended_gdp_deflator(2020, 1975),
             color = scenario, lty = scenario)) +
  geom_line() +
  expand_limits(y = c(0,10)) +
  scale_color_manual(values = jgcricolors::jgcricol()$pal_16[c(2,3,2,3)],
                     name = "Scenario") +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"),
                        name = "Scenario") +
  facet_wrap(~region, nrow = 1) +
  theme_bw() +
  ylab("Annual heating expenditures per capita (2020$)")

ggsave("figures/heating_expenditures.png", width = 8, height = 4, units = "in")



## heating energy burden =======================================================

# rename/ reorder scenarios
heating_burden_plot <- heating_burden %>%
  mutate(scenario = case_when(scenario == "Ref" & CI == "none" ~ "Ref|static",
                              scenario == "Ref" ~ "Ref|7.0",
                              CI == "none" ~ "LowC|static",
                              T ~ "LowC|2.6")) %>%
  filter(year >= 2015, year <= 2070,
         group == "d1")

heating_burden_plot$scenario <-
  factor(heating_burden_plot$scenario,
         levels = c("Ref|static", "LowC|static", "Ref|7.0", "LowC|2.6"))

# plot
heating_burden_plot %>%
  ggplot(aes(x = year, y = burden*100, color = scenario, lty = scenario)) +
  geom_line() +
  scale_x_continuous(breaks = c(2020, 2040, 2060)) +
  expand_limits(y = c(0,10)) +
  scale_color_manual(values = jgcricolors::jgcricol()$pal_16[c(2,3,2,3)],
                     name = "Scenario") +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"),
                        name = "Scenario") +
  facet_wrap(~region, nrow = 1) +
  theme_bw() +
  ylab("Heating energy burden (%)") + xlab("")

ggsave("figures/heating_energy_burden.png", width = 7, height = 3, units = "in")
