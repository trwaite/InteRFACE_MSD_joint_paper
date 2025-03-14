diesel_gal_to_Btu <- 128488
Btu_to_GJ <- 1.055e-6

nsb_communities <- c("Anaktuvuk Pass", "Atqasuk", "Kaktovik", "Nuiqsut",
                     "Point Hope", "Point Lay", "Utqiagvik", "Wainwright")


nwab_communities <- c("Ambler", "Buckland", "Deering", "Kiana", "Kivalina",
                      "Kobuk", "Kotzebue", "Noatak", "Noorvik", "Selawik", "Shungnak")

# get historical heating fuel prices in the boroughs ---------------------------

# read in data and filter to NAB and NSB communities
hist_heat_fuel_price <- read_csv("historical_data/Heating_Fuel_Price.csv", skip = 1) %>%
  filter(CommunityName %in% c(nsb_communities, nwab_communities)) %>%
  mutate(borough = if_else(CommunityName %in% nsb_communities, "NSB", "NAB"),
         # convert to 2015$
         HFRetailGal = HFRetailGal*extended_gdp_deflator(2015, ReportingYear))

# calculate average and median price per year and borough
hist_heat_fuel_price_avg <- hist_heat_fuel_price %>%
  group_by(borough, ReportingYear) %>%
  mutate(median_price = median(HFRetailGal, na.rm = T)) %>%
  ungroup()


# calculate borough fuel price adders ------------------------------------------

# get GCAM refined liquids enduse price
GCAM_heat_fuel_prices <- rgcam::getQuery(prj_resid, "refined liquids enduse price") %>%
  filter(scenario == "Ref", year == 2015) %>%
  mutate(value = value*extended_gdp_deflator(2015, 1975)*diesel_gal_to_Btu*Btu_to_GJ,
         borough = "GCAM Ref (AK)",
         median_price = value) %>%
  select(year, borough, value, median_price)

fuel_price_compare <- hist_heat_fuel_price_avg %>%
  select(year = ReportingYear, borough, value = HFRetailGal,
         median_price) %>%
  drop_na() %>%
  rbind(GCAM_heat_fuel_prices)

# calculate adders and multiplyers using 2010-2020 medians from borough data
fuel_adjusters <- fuel_price_compare %>%
  select(year, borough, value) %>%
  filter(year > 2010, year <= 2020) %>%
  group_by(borough) %>%
  summarize(median_price = median(value, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = borough, values_from = median_price) %>%
  pivot_longer(c(NSB, NAB), names_to = "borough", values_to = "median_price") %>%
  mutate(adder = median_price - `GCAM Ref (AK)`,
         multiplier = median_price/`GCAM Ref (AK)`,
         join = T) %>%
  select(borough, adder, multiplier, join)

# convert adders back to 1975$/GJ for use in the demand function
fuel_adders_final <- fuel_adjusters %>%
  mutate(adder = adder*extended_gdp_deflator(1975, 2015)/diesel_gal_to_Btu/Btu_to_GJ,
         region = if_else(borough == "NSB", "North Slope Borough",
                          "Northwest Arctic Borough")) %>%
  select(region, adder)

write_csv(fuel_adders_final, "analysis_resid_energy/resid_energy_demand_inputs/fuel_adders_final.csv")


# calculate projected borough heating service prices ---------------------------

# get GCAM projected tech service prices
prices <- rgcam::getQuery(prj_resid, "building costs by tech")

# filter to fuel furnace and apply borough adders
# get fuel furnace price
price_fuel_furnace <- prices %>%
  filter(sector == 'resid heating modern_d1',
         technology == "fuel furnace") %>%
  select(-region) %>%
  # apply borough price adders
  gcamdata::repeat_add_columns(tibble(region = c("North Slope Borough",
                                                 "Northwest Arctic Borough"))) %>%
  left_join(fuel_adders_final, by = "region") %>%
  # add the adder divided by the technology efficiency to convert adder per GJ
  # fuel input to adder per GJ output
  # TODO: add efficiency as input file
  mutate(value = value + adder/.83) %>%
  select(-adder) %>%
  separate(sector, into = c("service", "group"), sep = "_")

write_csv(price_fuel_furnace, "analysis_resid_energy/resid_energy_demand_inputs/price_fuel_furnace.csv")


# figure: community heating fuel prices vs GCAM price --------------------------
fuel_price_compare$borough <-
  factor(fuel_price_compare$borough,
         levels = c("NAB", "NSB", "GCAM Ref (AK)"),
         labels = c("Northwest Arctic Borough", "North Slope Borough", "GCAM Ref (AK)"))

ggplot() +
  geom_point(data = fuel_price_compare %>% filter(borough != "GCAM Ref (AK)"),
             aes(x = year, y = value, color = borough), alpha = 0.3) +
  geom_line(data = fuel_price_compare %>% filter(borough != "GCAM Ref (AK)"),
            aes(x = year, y = median_price, color = borough)) +
  geom_hline(data = fuel_price_compare %>% filter(borough == "GCAM Ref (AK)"),
             aes(yintercept = value), lty = 2) +
  theme_bw() +
  coord_cartesian(xlim = c(2005, 2023), clip = 'off') +
  theme(plot.margin = unit(c(1,3,1,1), "lines")) +
  scale_color_discrete(name = "") +
  geom_text(aes(x = 2027.5, y = 3.474, label = "GCAM-USA AK 2015")) +
  xlab("") + ylab("Heating fuel price (2015$/gallon)")

ggsave("figures/fuel_price_compare.png", width = 8, height = 4, units = "in")
