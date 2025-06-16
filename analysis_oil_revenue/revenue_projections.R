# ..............................................................................
# libraries and constatnts -----------------------------------------------------
# ..............................................................................

library(tidyverse)

# location of GCAM database
db_path <- "my_db_path"
# names of GCAM databases
db_name <- "my_db_name"

source("utils/extended_gdp_deflator.R")

# unit conversions
million_barrel_to_barrel <- 1e6
barrel_to_btu <- 5.8e6
btu_to_EJ <- 1.055e-15
million_barrel_to_EJ <- million_barrel_to_barrel*barrel_to_btu*btu_to_EJ
usd_1975_to_2015 <- 3.507477
EJ_to_GJ <- 1e9

# whether to reread GCAM results from a database
# (if not, use existing rgcam project file)
reread_db <- F

# ..............................................................................
# load data  -------------------------------------------------------------------
# ..............................................................................

# run revenue models to get model coefficients
source("analysis_oil_revenue/revenue_models.R")

# read in model coefficients
model_coefs <- read_csv("analysis_oil_revenue/model_coefs.csv")

# read database or load rgcam project file
if(reread_db){
  conn <- rgcam::localDBConn(db_path, db_name)
  prj <- rgcam::addScenario(conn, "prj_oil_revenue",
                                  "queries/oil_queries.xml")
} else{
  prj <- rgcam::loadProject("rgcam_data/prj_oil_revenue")
}

# read in historical revenue data
historical_revenue <- read.csv("historical_data/oil_gas_revenue_historical.csv") %>%
  group_by(year) %>%
  summarize(revenue = sum(value)*extended_gdp_deflator(2015,year)) %>%
  ungroup()


# ..............................................................................
# post-process GCAM outputs ----------------------------------------------------
# ..............................................................................

# calculate total crude oil production
AK_total_oil <- rgcam::getQuery(prj, "crude oil production") %>%
  group_by(Units, scenario, region, year) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  # convert to GJ for multiplying with price below
  mutate(value = value*EJ_to_GJ,
         Units = "GJ")

# convert oil prices
AK_oil_price <- rgcam::getQuery(prj, "AK crude oil price") %>%
  mutate(value = value*extended_gdp_deflator(2015, 1975),
         Units = "2015$/GJ")


# calculate oil market value
AK_mkt_value <- AK_total_oil %>%
  left_join(AK_oil_price, by = c("scenario", "year")) %>%
  mutate(mkt_value = value.x*value.y/1e6,
         Units = "millions 2015$") %>%
  select(scenario, year, mkt_value, Units) %>%
  filter(year <= 2075)


# ..............................................................................
# project tax revenue ----------------------------------------------------------
# ..............................................................................

# apply models to project tax revenue
AK_tax_revenue <- AK_mkt_value %>%
  mutate(actual_period = case_when(year < 2006 ~ "pre_2006",
                            T ~ "post_2006"),
         join = T) %>%
  left_join(model_coefs %>% mutate(join = T), by = "join") %>%
  select(-join) %>%
  mutate(projected_revenue = case_when(type == "linear" ~ intercept + mkt_value_coef*mkt_value,
                                       T ~ exp(intercept)*exp(mkt_value_coef)^mkt_value)) %>%
  select(scenario, year, period, actual_period, type, Units, projected_revenue) %>%
  separate(scenario, into = c("scenario", "ensemble"), sep = "_") %>%
  mutate(climate_impacts = !is.na(ensemble)) %>%
  # add max and min for ensembles
  group_by(scenario, year, period, actual_period, climate_impacts, type, Units) %>%
  mutate(ens_min = min(projected_revenue),
         ens_max = max(projected_revenue)) %>%
  ungroup()

# version with 0 as a lower bound for tax revenue
# (this removes negative projections from linear model with a negative intercept)
AK_tax_revenue_capped <- AK_tax_revenue %>%
  mutate(projected_revenue = if_else(projected_revenue < 0, 0, projected_revenue),
         ens_min = if_else(ens_min < 0, 0, ens_min),
         ens_max = if_else(ens_max < 0, 0, ens_max))



# ..............................................................................
# make figures -----------------------------------------------------------------
# ..............................................................................

# read in historical oil production for plotting
AK_oil_prod_hist <- read.csv("historical_data/oil_price_production.csv") %>%
  filter(variable == "production")

# plot historical and projected oil production
AK_total_oil_plot <- AK_total_oil %>%
  filter(year <= 2075, year >= 1990) %>%
  mutate(ens = case_when(grepl("Ref_e", scenario) ~ gsub("Ref_", "", scenario),
                         grepl("LowC_e", scenario) ~ gsub("LowC_", "", scenario),
                         T ~ "none"),
         scenario = case_when(grepl("Ref_e", scenario) ~ "Ref|7.0",
                              grepl("LowC_e", scenario) ~ "LowC|2.6",
                              grepl("Ref", scenario) ~ "Ref|static",
                              T ~ "LowC|static"),
         value = value/EJ_to_GJ/million_barrel_to_EJ,
         Units = "million barrels")

AK_total_oil_plot$scenario <-
  factor(AK_total_oil_plot$scenario,
         levels = c("Ref|static", "LowC|static", "Ref|7.0", "LowC|2.6"))

ggplot() +
  geom_line(data = AK_total_oil_plot %>% filter(year >= 2015),
            aes(x = year, y = value, color = scenario, lty = scenario,
                group = interaction(scenario, ens))) +
  geom_point(data = AK_oil_prod_hist,
             aes(x = year, y = value)) +
  expand_limits(y = 0) +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"),
                        name = "Scenario") +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#E69F00", "#56B4E9"),
                     name = "Scenario") +
  scale_x_continuous(breaks = c(1980, 2020, 2060)) +
  theme_bw() +
  xlab("") + ylab("Million barrels")

ggsave("figures/AK_oil_production.png",
       width = 7, height = 4, units = "in")

# read in historical oil price for plotting
AK_oil_price_hist <- read.csv("historical_data/oil_price_production.csv") %>%
  filter(variable == "price per barrel of oil") %>%
  mutate(value = extended_gdp_deflator(2015, year)*value)

# plot historical and projected oil price
AK_oil_price_plot <- AK_oil_price %>%
  filter(year <= 2075, year >= 1990) %>%
  mutate(ens = case_when(grepl("Ref_e", scenario) ~ gsub("Ref_", "", scenario),
                         grepl("LowC_e", scenario) ~ gsub("LowC_", "", scenario),
                         T ~ "none"),
         scenario = case_when(grepl("Ref_e", scenario) ~ "Ref|7.0",
                              grepl("LowC_e", scenario) ~ "LowC|2.6",
                              grepl("Ref", scenario) ~ "Ref|static",
                              T ~ "LowC|static"),
         value = value*million_barrel_to_EJ*10^3,
         units = "2015$/barrel")

AK_oil_price_plot$scenario <-
  factor(AK_oil_price_plot$scenario,
         levels = c("Ref|static", "LowC|static", "Ref|7.0", "LowC|2.6"))

 ggplot() +
  geom_line(data = AK_oil_price_plot %>% filter(year >= 2015),
            aes(x = year, y = value,
                color = scenario, lty = scenario,
                group = interaction(scenario, ens))) +
  geom_point(data = AK_oil_price_hist %>% filter(year <= 2015),
             aes(x = year, y = value)) +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"),
                        name = "Scenario") +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#E69F00", "#56B4E9"),
                     name = "Scenario") +
  scale_x_continuous(breaks = c(1980, 2020, 2060)) +
  theme_bw() +
  xlab("") + ylab("2015$/barrel")

ggsave("figures/AK_oil_price.png",
       width = 7, height = 4, units = "in")

# compare GCAM historical prices with AK historical data
oil_price_compare <- AK_oil_price_plot %>%
  filter(year <= 2020, scenario == "Ref|static") %>%
  mutate(Source = "GCAM historical periods") %>%
  select(year, Source, value) %>%
  rbind(AK_oil_price_hist %>%
              mutate(Source = "AK historical data") %>%
          select(year, Source, value))

oil_price_compare %>%
  filter(year >= 1990, year <= 2020) %>%
  ggplot(aes(x = year, y = value, color = Source)) +
  geom_line() +
  theme_bw() +
  xlab("") + ylab("2015$/barrel")

ggsave("figures/historical_oil_price_comparison.png",
       width = 6, height = 4, units = "in")


# plot with all model versions together (and capped at 0)
AK_tax_revenue_capped_combined <- AK_tax_revenue_capped %>%
  replace_na(list(ensemble = "none")) %>%
  mutate(id = paste(period, type, ensemble, sep = "_"),
         scenario = case_when(scenario == "Ref" & !climate_impacts ~ "Ref|static",
                              scenario == "Ref" ~ "Ref|7.0",
                              scenario == "LowC" & !climate_impacts ~ "LowC|static",
                              T ~ "LowC|2.6")) %>%
  select(scenario, year, type, ensemble, id, climate_impacts, Units, projected_revenue) %>%
  group_by(scenario, year, climate_impacts) %>%
  mutate(ens_min = min(projected_revenue),
         ens_max = max(projected_revenue)) %>%
  ungroup()

AK_tax_revenue_capped_combined$scenario <-
  factor(AK_tax_revenue_capped_combined$scenario,
         levels = c("Ref|static", "LowC|static", "Ref|7.0", "LowC|2.6"))

# plot with only the main case shown as lines
ggplot() +
  # historical data
  geom_point(data = historical_revenue,
             aes(x = year, y = revenue/1000),
             color = "black") +
  # GCAM projections starting in 2020 with cliamte impacts
  geom_line(data = AK_tax_revenue_capped_combined %>%
              filter(year >= 2020,
              grepl("post_2006_exponential", id)),
            aes(x = year, y = projected_revenue/1000, color = scenario, lty = scenario,
                group = interaction(scenario, ensemble))) +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"),
                        name = "Scenario") +
  # shaded area around ensembles
  geom_ribbon(data = AK_tax_revenue_capped_combined %>%
                filter(year >= 2020, climate_impacts),
              aes(x = year, ymin = ens_min/1000, ymax = ens_max/1000,
                  fill = scenario), alpha = 0.3) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#E69F00", "#56B4E9"),
                     name = "Scenario") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#E69F00", "#56B4E9"),
                    name = "Scenario", guide = "none") +
  theme_bw() +
  scale_x_continuous(breaks = c(1980, 2020, 2060)) +
  ylab("Oil Revenue (billion 2015$)") + xlab("")

ggsave("figures/proj_revenue_combined.png",
       width = 7, height = 4, units = "in")


# old plots showing each model separately
# plot projected revenue by revenue model type
# (exponential vs linear, pre-2006 vs post-2006)
ggplot() +
  # historical data
  geom_line(data = historical_revenue,
            aes(x = year, y = revenue),
            color = "black", lty = 1) +
  # GCAM projections starting in 2020, with climate impacts
  geom_line(data = AK_tax_revenue %>%
              filter(year >= 2020, climate_impacts),
            aes(x = year, y = projected_revenue, color = scenario, lty = ensemble)) +
  scale_linetype_manual(values = rep(1, 13)) +
  # without climate impacts
  geom_line(data = AK_tax_revenue %>% filter(year >= 2020, !climate_impacts),
            aes(x = year, y = projected_revenue, color = scenario, lty = ensemble),
            lty = 2) +
  # shaded area around ensembles
  geom_ribbon(data = AK_tax_revenue %>% filter(year >= 2020),
              aes(x = year, ymin = ens_min, ymax = ens_max, fill = scenario),
              alpha = 0.3) +
  scale_color_manual(values = c("#E69F00", "#56B4E9"), name = "Scenario") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name = "Scenario") +
  facet_grid(period~type) +
  theme_bw() +
  scale_x_continuous(n.breaks = 8) +
  guides(lty = "none") +
  ylab("Oil Revenue (2015$)") + xlab("")

ggsave("figures/proj_revenue_separate_with_negatives.png",
       width = 8, height = 4, units = "in")


# plot version with lower revenue bound capped at 0 (no negatives)
ggplot() +
  # historical data
  geom_line(data = historical_revenue,
            aes(x = year, y = revenue/1000),
            color = "black", lty = 1) +
  # GCAM projections starting in 2020, with climate impacts
  geom_line(data = AK_tax_revenue_capped %>% filter(year >= 2020, climate_impacts),
            aes(x = year, y = projected_revenue/1000, color = scenario, lty = ensemble)) +
  scale_linetype_manual(values = rep(1, 13)) +
  # without climate impacts
  geom_line(data = AK_tax_revenue_capped %>% filter(year >= 2020, !climate_impacts),
            aes(x = year, y = projected_revenue/1000, color = scenario, lty = ensemble),
            lty = 2) +
  # shaded area around ensembles
  geom_ribbon(data = AK_tax_revenue_capped %>% filter(year >= 2020),
              aes(x = year, ymin = ens_min/1000, ymax = ens_max/1000, fill = scenario),
              alpha = 0.3) +
  scale_color_manual(values = c("#E69F00", "#56B4E9"), name = "Scenario") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name = "Scenario") +
  facet_grid(period~type) +
  theme_bw() +
  scale_x_continuous(n.breaks = 8) +
  guides(lty = "none") +
  ylab("Oil Revenue (billion 2015$)") + xlab("")

ggsave("figures/proj_revenue_separate.png",
       width = 8, height = 4, units = "in")
