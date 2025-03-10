source("utils/extended_gdp_deflator.R")
USD_2022_to_1975 <- 0.234460479
def90_75<-2.212
prj_resid <- rgcam::loadProject("rgcam_data/prj_year4_resid_energy")

# get GCAM AK decile 1 per capita GDP projections ------------------------------
subregional_income <- rgcam::getQuery(prj_resid, "subregional income-resid")

subregional_income_1975dollar <- subregional_income %>%
  separate(`gcam-consumer`, into = c("temp", "group"), sep = "_") %>%
  mutate(gdppc.grp = value*1000/def90_75) %>%
  select(-value, -temp)

# get borough base per capita GDP ----------------------------------------------
nsb_communities <- c("Anaktuvuk Pass", "Atqasuk", "Kaktovik", "Nuiqsut",
                     "Point Hope", "Point Lay", "Utqiagvik", "Wainwright")


nwab_communities <- c("Ambler", "Buckland", "Deering", "Kiana", "Kivalina",
                      "Kobuk", "Kotzebue", "Noatak", "Noorvik", "Selawik", "Shungnak")

# read in income data and filter to communities in nsb and nab
community_income <- read_csv("historical_data/community_income.csv",
                             skip = 1) %>%
  filter(Place %in% c(nsb_communities, nwab_communities),
         Start_Year == 2018) %>%
  mutate(borough = if_else(Place %in% nsb_communities,
                           "North Slope Borough", "Northwest Arctic Borough")) %>%
  select(borough, community = Place, pc_income = e_Per_Capita_Income)

# read in population data and filter to communities in nsb and nab
community_population <- read_csv("historical_data/community_population.csv",
                                 skip = 1) %>%
  filter(Place %in% c(nsb_communities, nwab_communities),
         Start_Year == 2018) %>%
  mutate(borough = if_else(Place %in% nsb_communities,
                           "North Slope Borough", "Northwest Arctic Borough")) %>%
  select(borough, community = Place, population = e_Pop_Total)

borough_population <- community_population %>%
  group_by(borough) %>%
  summarize(population = sum(population)) %>%
  ungroup()

# calculate total borough per capita income
borough_income <- community_income %>%
  left_join(community_population, by = c("borough", "community")) %>%
  mutate(total_income = pc_income*population) %>%
  group_by(borough) %>%
  summarize(pc_income = sum(total_income)/sum(population)) %>%
  ungroup()

# convert to 1975$ for comparison with GCAM data
borough_income_1975dollar <- borough_income %>%
  mutate(pc_income = pc_income*USD_2022_to_1975)

# project borough per capita income --------------------------------------------

# get scalars from 2020 data
gdppc_scalars <- subregional_income_1975dollar %>%
  filter(scenario == "Ref", group == "d1", year == 2020) %>%
  select(year, GCAM_gdppc = gdppc.grp) %>%
  full_join(borough_income_1975dollar %>% mutate(year = 2020),
            by = "year") %>%
  mutate(gdppc.scalar = pc_income/GCAM_gdppc) %>%
  select(borough, gdppc.scalar)

# calculate projected borough GDP using GCAM d1 projections and scalars
borough_gdppc_scaled <- subregional_income_1975dollar %>%
  filter(group == "d1") %>%
  gcamdata::repeat_add_columns(tibble(borough = unique(borough_income$borough))) %>%
  left_join(gdppc_scalars, by = "borough") %>%
  mutate(gdppc.grp = gdppc.grp*gdppc.scalar) %>%
  select(scenario, borough, year, group, gdppc.grp, Units) %>%
  rename(region = borough)


write_csv(borough_gdppc_scaled, "analysis_resid_energy/resid_energy_demand_inputs/borough_gdppc_scaled.csv")


# plot borough pc GDP compared to GCAM-USA AK d1 and d2 ------------------------

ggplot() +
  geom_line(data = borough_gdppc_scaled %>%
              full_join(subregional_income_1975dollar %>%
                          filter(group %in% c("d1", "d2"))) %>%
              filter(scenario == "Ref", year >= 2015) %>%
              mutate(gdppc.grp = gdppc.grp/USD_2022_to_1975,
                     region = case_when(region == "AK" & group == "d1" ~ "AK d1 (GCAM-USA)",
                                        region == "AK" & group == "d2" ~ "AK d2 (GCAM-USA)",
                                        T ~ region)),
            aes(x = year, y = gdppc.grp, color = region, lty = region)) +
  scale_color_manual(values = c("black", "black",
                                jgcricolors::jgcricol()$pal_16[c(2,3)]),
                     name = "") +
  scale_linetype_manual(values = c(1, 2, 1, 1), name = "") +
  geom_point(data = borough_income %>% mutate(year = 2020),
             aes(x = year, y = pc_income, color = borough)) +
  guides(color = guide_legend(override.aes = list(shape = NA)))+
  theme_bw() +
  xlab("Year") + ylab("Per capita GDP (2022$)")

ggsave("figures/borough_projected_pcGDP.png",
       width = 6, height = 4, units = "in")
