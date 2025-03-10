
input_data_dir <- "analysis_resid_energy/resid_energy_demand_inputs"

# get borough HDD projections --------------------------------------------------

# read in data
HDDCDD_borough <- read_csv(paste0(input_data_dir,
                                  "/hdcd_borough.csv")) %>%
  pivot_longer(c(borough.cdd, borough.hdd),
               names_to = "service", values_to = "value")

# use reference values for 2015 in both CI scenarios
HDDCDD_borough_2015 <- HDDCDD_borough %>%
  filter(year == 2015) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(SSP126 = Ref) %>%
  pivot_longer(c(Ref, SSP126), names_to = "scenario", values_to = "value")

HDDCDD_borough_adj <- HDDCDD_borough %>%
  filter(year > 2015) %>%
  rbind(HDDCDD_borough_2015)


# make df of HDDCDD by borough for all climate impacts scenarios
HDDCDD_borough_CI <- HDDCDD_borough_adj %>%
  rename(region = borough, degree.days = value) %>%
  mutate(scenario = if_else(scenario == "SSP126", "LowC", scenario),
         service = if_else(service == "borough.cdd", "resid cooling modern",
                           "resid heating modern"),
         nodeInput = "resid",
         building.node.input = "resid_building",
         CI = scenario) %>%
  select(-scenario)

# make df of HDDCDD by borough for all non- climate impacts scenarios
# (keep HDDCDD at 2015 levels)
HDDCDD_borough_noCI <- HDDCDD_borough_CI %>%
  filter(year == 2015, CI == "Ref") %>%
  mutate(CI = "none") %>%
  select(-year) %>%
  group_by(region, service) %>%
  gcamdata::repeat_add_columns(tibble(year = unique(HDDCDD_borough_CI$year))) %>%
  ungroup()

HDDCDD_borough_all <- rbind(HDDCDD_borough_CI, HDDCDD_borough_noCI)

write_csv(HDDCDD_borough_all, "analysis_resid_energy/resid_energy_demand_inputs/HDDCDD_borough_all.csv")
