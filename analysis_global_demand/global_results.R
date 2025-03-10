library(tidyverse)

million_barrel_to_barrel <- 1e6
barrel_to_btu <- 5.8e6
btu_to_EJ <- 1.055e-15

million_barrel_to_EJ <- million_barrel_to_barrel*barrel_to_btu*btu_to_EJ

usd_1975_to_2015 <- 3.507477

EJ_to_GJ <- 1e9

prj_global <- rgcam::loadProject("rgcam_data/prj_year4_global")


# primary energy consumption ---------------------------------------------------
primary_energy <- rgcam::getQuery(prj_global,
                                  "primary energy consumption by region (direct equivalent)")

global_primary_energy <- primary_energy %>%
  filter(region != "USA") %>%
  group_by(scenario, fuel, year) %>%
  summarize(value = sum(value)) %>%
  ungroup()

global_primary_energy$scenario <-
  factor(global_primary_energy$scenario,
         levels = c("Ref", "LowC", paste0("Ref_e", seq(1, 13)),
                    paste0("LowC_e", seq(1, 13))),
         labels = c("Ref|static", "LowC|static", paste0("Reference_e", seq(1, 13)),
                    paste0("LowC_e", seq(1, 13))))

global_primary_energy$fuel <-
  factor(global_primary_energy$fuel,
         levels = c("a oil", "b natural gas", "c coal", "d biomass",
                    "e nuclear", "f hydro", "g wind", "h solar",
                    "i geothermal", "j traditional biomass",
                    "coal production", "crude oil production",
                    "natural gas production"),
         labels = c("oil", "gas", "coal", "biomass", "nuclear", "hydro",
                    "wind", "solar", "geothermal", "biomass",
                    "coal production", "crude oil production",
                    "natural gas production"))

global_primary_energy %>%
  filter(scenario %in% c("Ref|static", "LowC|static"),
         year >= 2015, year <= 2075) %>%
  group_by(scenario, year, fuel) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  #mutate(scenario = if_else(scenario == "Reference", "Ref|static", "LowC|static")) %>%
  ggplot(aes(x = year, y = value, fill = fuel)) +
  geom_area() +
  scale_fill_manual(values = c(jgcricolors::jgcricol()$pal_all,
                    "coal production" = "purple",
                    "crude oil production" = "pink",
                    "natural gas production" = "grey60")) +
  facet_wrap(~scenario) +
  theme_bw() +
  #theme(text = element_text(size = 19)) +
  xlab("") + ylab("Primary energy (EJ)")

ggsave("figures/global_primary_energy.png",
       width = 7.5, height = 3, units = "in")

# get proportions
global_primary_energy_prop <- global_primary_energy %>%
  group_by(scenario, year, fuel) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  group_by(scenario, year) %>%
  mutate(prop = value/sum(value)) %>%
  ungroup()


# oil demand -------------------------------------------------------------------

global_oil <- global_primary_energy %>%
  filter(fuel == "oil")

global_oil_plot <- global_oil %>%
  filter(year >= 2015, year <= 2075) %>%
  mutate(ens = case_when(grepl("Reference_e", scenario) ~ gsub("Reference_", "", scenario),
                         grepl("LowC_e", scenario) ~ gsub("LowC_", "", scenario),
                         T ~ "none"),
         scenario = case_when(grepl("Reference_e", scenario) ~ "Ref|7.0",
                              grepl("LowC_e", scenario) ~ "LowC|2.6",
                              T ~ as.character(scenario)),
         value = value/million_barrel_to_EJ/1000,
         units = "billion barrels")

global_oil_plot$scenario <-
  factor(global_oil_plot$scenario,
         levels = c("Ref|static", "LowC|static", "Ref|7.0", "LowC|2.6"))

global_oil_plot %>%
  ggplot(aes(x = year, y = value, color = scenario, lty = scenario,
             group = interaction(scenario, ens))) +
  geom_line() +
  expand_limits(y = 0) +
  scale_color_manual(values = jgcricolors::jgcricol()$pal_16[c(3,2,3,2)],
                     name = "Scenario") +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed"),
                        name = "Scenario") +
  theme_bw() +
  xlab("Year") + ylab("Oil demand (billion barrels)")

ggsave("figures/global_oil_demand.png",
       width = 6, height = 4, units = "in")


