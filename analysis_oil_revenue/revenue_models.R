library(tidyverse)

cb_pal <- c("#E69F00", "#56B4E9", "#009E73",
            "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

save_figures <- T

source("utils/extended_gdp_deflator.R")

#...............................................................................
# Read in data -----------------------------------------------------------------
#...............................................................................

# oil and gas revenue (all sources)
oil_gas_revenue <- read_csv("historical_data/oil_gas_revenue_historical.csv") %>%
  # deflate
  mutate(value = value*extended_gdp_deflator(2015, year)) %>%
  # filter to first full year of prudhoe bay production
  filter(year > 1978)

# total oil and gas revenue by year
total_revenue <- oil_gas_revenue %>%
  group_by(year) %>%
  summarise(value = sum(value))

# oil price and production
oil_price_production <- read_csv("historical_data/oil_price_production.csv") %>%
  filter(variable %in% c("price per barrel of oil", "production")) %>%
  # deflate the prices
  mutate(value = case_when(variable == "price per barrel of oil" ~ value*extended_gdp_deflator(2015, year),
                           T ~ value),
         #simplify variable names
         variable = case_when(grepl("price", variable) ~ "price",
                              T ~ "production"),
         # add units
         units = case_when(variable == "price" ~ "2015$/barrel",
                           T ~ "barrels")) %>%
  # filter to first full year of prudhoe bay production
  filter(year > 1978)


#...............................................................................
# Historical revenue -----------------------------------------------------------
#...............................................................................

# plot revenue over time
total_revenue %>%
  #filter(year >= 1977) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  theme_bw() +
  xlab("Year") + ylab("Revenue (millions of dollars)")

if(save_figures){
  ggsave("figures/historical_revenue_total.png",
         width = 6, height = 4, units = "in")
}


# plot revenue over time by type
oil_gas_revenue %>%
  #filter(year >= 1977) %>%
  mutate(type = case_when(grepl("bonus|conserv|hazard|special", type) ~ "revenue - other",
                          T ~ type)) %>%
  rename(Source = type) %>%
  group_by(year, Source) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(x = year, y = value, fill = Source)) +
  #geom_area() +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cb_pal) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Year") + ylab("Revenue (millions of dollars)")

if(save_figures){
  ggsave("figures/historical_revenue_by_source.png",
         width = 7, height = 5, units = "in")
}


#...............................................................................
# Historical price and production ----------------------------------------------
#...............................................................................

# plot price over time
oil_price_production %>%
  filter(variable == "price") %>%
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  theme_bw() +
  xlab("Year") + ylab("Oil price (2015$/barrel)")

if(save_figures){
  ggsave("figures/historical_oil_price.png",
         width = 6, height = 4, units = "in")
}


# plot production over time
oil_price_production %>%
  filter(variable == "production") %>%
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  theme_bw() +
  xlab("Year") + ylab("Oil production (million barrels)")

if(save_figures){
  ggsave("figures/historical_oil_production.png",
         width = 6, height = 4, units = "in")
}

# plot market value over time
oil_price_production %>%
  select(-units) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(mkt_value = price*production) %>%
  ggplot(aes(x = year, y = mkt_value)) +
  geom_line() +
  theme_bw() +
  xlab("Year") + ylab("Market value of oil produced (million 2015$)")

if(save_figures){
  ggsave("figures/historical_oil_value.png",
         width = 6, height = 4, units = "in")
}


#...............................................................................
# Revenue models ---------------------------------------------------------------
#...............................................................................

# start df of model results
model_coefs <- tibble(period = character(),
                      type = character(),
                      intercept = numeric(),
                      mkt_value_coef = numeric())

production_price_revenue <-
  total_revenue %>%
  mutate(variable = "revenue",
         units = "2015$") %>%
  rbind(oil_price_production) %>%
  select(-units) %>%
  pivot_wider(names_from = variable, values_from = value)

## Linear models: all years together ===========================================

### Production #################################################

summary(lm(`revenue` ~ production, production_price_revenue))

# plot revenue against production
production_price_revenue %>%
  ggplot(aes(x = production, y = `revenue`, col = year)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, col = "black", lty = 2) +
  annotate("text", x = 100, y = 11000,
           label = "y = 3.21x + 1435\np = 0.013\nR^2 = 0.1") +
  theme_bw() +
  xlab("Oil production (millions of barrels)") +
  ylab("Total oil and gas revenue (millions of dollars)")


### Price ######################################################

price_reg <- (lm(revenue ~ price, production_price_revenue))
summary(price_reg)

price_resids <- price_reg$residuals
hist(price_resids)
qqnorm(price_resids)
qqline(price_resids)

# plot revenue against oil price
production_price_revenue %>%
  #filter(year >= 1980) %>%
  ggplot(aes(x = price, y = revenue, col = year)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, col = "black", lty = 2) +
  annotate("text", x = 15, y = 11000,
           label = "y = 70.74x + 313.5\np < 2.2e-16\nR^2 = 0.73") +
  theme_bw() +
  xlab("Oil price (dollars per barrel)") +
  ylab("Total oil and gas revenue (millions of dollars)")


### Market value ###############################################
mkt_value <- production_price_revenue %>%
  mutate(mkt_value = production*price,
         avg_tax_rate = revenue/mkt_value) %>%
  mutate(period = case_when(year < 2006 ~ "pre-2006",
                            T ~ "post-2006"))

mkt_value$period <- factor(mkt_value$period,
                           levels = c("pre-2006", "post-2006"))

price_times_prod_reg <- (lm(revenue ~ mkt_value, mkt_value))
summary(price_times_prod_reg)

price_times_prod_resids <- price_times_prod_reg$residuals
hist(price_times_prod_resids)
qqnorm(price_times_prod_resids)
qqline(price_times_prod_resids)


## Linear models: pre- and post- 2006 ==========================================

### Pre-2006 (market value) ####################################################

# run linear regression
lin_mkt_val_1 <- lm(revenue ~ mkt_value,
                    filter(mkt_value, period == "pre-2006"))
summary(lin_mkt_val_1)

# save model: pre_2006_lin
model_coefs <- model_coefs %>%
  add_row(period = "pre_2006",
          type = "linear",
          intercept = lin_mkt_val_1$coefficients[1],
          mkt_value_coef = lin_mkt_val_1$coefficients[2])

# check residuals
lin_mkt_val_1_resids <- lin_mkt_val_1$residuals
hist(lin_mkt_val_1_resids)
qqnorm(lin_mkt_val_1_resids)
qqline(lin_mkt_val_1_resids)


### Post-2006 (market value) ###################################################

# run linear regression
lin_mkt_val_2 <- lm(revenue ~ mkt_value,
                    filter(mkt_value, period == "post-2006"))
summary(lin_mkt_val_2)

# save model: post_2006_lin
model_coefs <- model_coefs %>%
  add_row(period = "post_2006",
          type = "linear",
          intercept = lin_mkt_val_2$coefficients[1],
          mkt_value_coef = lin_mkt_val_2$coefficients[2])

# check residuals
lin_mkt_val_2_resids <- lin_mkt_val_2$residuals
hist(lin_mkt_val_2_resids)
qqnorm(lin_mkt_val_2_resids)
qqline(lin_mkt_val_2_resids)


## Exponential models: pre- and post- 2006 =====================================

### Pre-2006 (market value) ####################################################

# run exponential regression
# ln(y) = 2.806e-5x + 7.688
exp_mkt_val_1 <- lm(log(revenue) ~ mkt_value,
                    filter(mkt_value, period == "pre-2006"))
summary(exp_mkt_val_1)

# save model: pre_2006_exp
model_coefs <- model_coefs %>%
  add_row(period = "pre_2006",
          type = "exponential",
          intercept = exp_mkt_val_1$coefficients[1],
          mkt_value_coef = exp_mkt_val_1$coefficients[2])

# check residuals
exp_mkt_val_1_resids <- exp_mkt_val_1$residuals
hist(exp_mkt_val_1_resids)
qqnorm(exp_mkt_val_1_resids)
qqline(exp_mkt_val_1_resids)


### Post-2006 (market value) ###################################################

# run exponential regression
# ln(y) = 1.095e-4x + 6.443
exp_mkt_val_2 <- lm(log(revenue) ~ mkt_value,
                    filter(mkt_value, period == "post-2006"))
summary(exp_mkt_val_2)

# save model: post_2006_exp
model_coefs <- model_coefs %>%
  add_row(period = "post_2006",
          type = "exponential",
          intercept = exp_mkt_val_2$coefficients[1],
          mkt_value_coef = exp_mkt_val_2$coefficients[2])

# check residuals
exp_mkt_val_2_resids <- exp_mkt_val_2$residuals
hist(exp_mkt_val_2_resids)
qqnorm(exp_mkt_val_2_resids)
qqline(exp_mkt_val_2_resids)


## Save all model data =========================================================
write_csv(model_coefs, "analysis_oil_revenue/model_coefs.csv")

## Model plots =================================================================

### Linear model using all years ###############################################

mkt_value %>%
  ggplot(aes(x = mkt_value, y = revenue, col = period)) +
  geom_point() +
  #geom_text(aes(label = year)) +
  geom_smooth(method = "lm", se = F, col = "black", lty = 2) +
  #annotate("text", x = 3000, y = 11000,
  # label = "y = .29x - 340\np < 2.2e-16\nR^2 = 0.79") +
  theme_bw() +
  expand_limits(y = 0) +
  xlab("Oil price (2015$/barrel) * production (millions of barrels)") +
  ylab("Revenue (millions 2015$)")

if(save_figures){
  ggsave("figures/lin_model_all_years.png",
         width = 7, height = 3.5, units = "in")
}

### Linear models by period ####################################################

mkt_value %>%
  ggplot(aes(x = mkt_value, y = revenue)) +
  geom_point() +
  #geom_text(aes(label = year), position = "jitter") +
  geom_smooth(method = "lm", se = F, col = "blue", lty = 1) +
  facet_wrap(~period) +
  theme_bw() +
  expand_limits(y = 0) +
  xlab("Market value (millions 2015 USD)") +
  ylab("Revenue (millions 2015 USD)")

if(save_figures){
  ggsave("figures/lin_models_by_period.png",
         width = 9, height = 3, units = "in")

}


### Exponential models by period ###############################################

# get data for exponential model
data_1 <- mkt_value %>% filter(period == "pre-2006")
data_2 <- mkt_value %>% filter(period == "post-2006")

xvals_1 <- seq(from = min(data_1$mkt_value, na.rm = T),
               to = max(data_1$mkt_value, na.rm = T),
               by = 1)
xvals_2 <- seq(from = min(data_2$mkt_value, na.rm = T),
               to = max(data_2$mkt_value, na.rm = T),
               by = 1)

yvals_1 <- exp(7.688)*exp(2.806e-5)^xvals_1
yvals_2 <- exp(6.443)*exp(1.095e-4)^xvals_2

exp_reg_1 <- data.frame(mkt_value = xvals_1,
                        revenue = yvals_1, period = "pre-2006")
exp_reg_2 <- data.frame(mkt_value = xvals_2,
                        revenue = yvals_2, period = "post-2006")

exp_reg_all <- rbind(exp_reg_1, exp_reg_2)

# reorder periods
exp_reg_all$period <-
  factor(exp_reg_all$period,
         levels = c("pre-2006", "post-2006"))

# plot exponential model
ggplot() +
  geom_point(data = mkt_value,
             aes(x = mkt_value, y = revenue)) +
  geom_line(data = exp_reg_all,
            aes(x = mkt_value, y = revenue), col = "blue", lwd = 1) +
  facet_wrap(~period, scales = "free") +
  theme_bw() +
  xlab("Market value (millions 2015 USD)") +
  ylab("Revenue (millions 2015 USD)")

if(save_figures){
  ggsave("figures/exp_models_by_period.png",
         width = 9, height = 3, units = "in")
}


### Modeled revenue vs real revenue ############################################

# generate modeled revenue data for each year
# linear model
modeled_rev_linear <- mkt_value %>%
  mutate(modeled_revenue = case_when(period == "pre-2006" ~ 0.144*mkt_value + 1328,
                                     T ~ 0.490*mkt_value - 3174),
         model = "linear")
# exponential model
modeled_rev_exp <- mkt_value %>%
  mutate(modeled_revenue = case_when(period == "pre-2006" ~ exp(7.688)*exp(2.806e-5)^mkt_value,
                                     T ~ exp(6.443)*exp(1.095e-4)^mkt_value),
         model = "exponential")
# combine models
modeled_rev_all <- rbind(modeled_rev_linear, modeled_rev_exp)

# plot both models with actual revenue data
modeled_rev_all %>%
  ggplot() +
  geom_line(aes(x = year, y = revenue), lty = 2) +
  geom_line(aes(x = year, y = modeled_revenue, col = model)) +
  scale_color_manual(values = cb_pal[c(1,2)], name = "Model") +
  theme_bw() +
  xlab("Year") + ylab("Revenue")

if(save_figures){
  ggsave("figures/all_models.png",
         width = 8, height = 4, units = "in")
}


## LOOCV =======================================================================

ctrl <- caret::trainControl(method = "LOOCV")

### Pre-2006 linear ############################################################

loocv_lin_1 <- caret::train(revenue ~ mkt_value,
                            data = data_1, method = "lm",
                            trControl = ctrl)

print(loocv_lin_1)

### Pre-2006 exponential #######################################################
loocv_exp_1 <- caret::train(log(revenue) ~ mkt_value,
                            data = data_1, method = "lm",
                            trControl = ctrl)

print(loocv_exp_1)

# re-transform the exponential predictions to calculate MAE and RMSE
# for comparison with the linear model
resids_1 <- loocv_exp_1$pred %>%
  select(pred, obs) %>%
  mutate(
    pred = exp(pred), obs = exp(obs),
    resid = abs(pred - obs),
    resid_sq = (pred - obs)^2)

mean(resids_1$resid)
sqrt(mean(resids_1$resid_sq))


### Post-2006 linear ###########################################################

loocv_lin_2 <- caret::train(revenue ~ mkt_value,
                            data = data_2, method = "lm",
                            trControl = ctrl)

print(loocv_lin_2)

### Post-2006 exponential ######################################################
loocv_exp_2 <- caret::train(log(revenue) ~ mkt_value,
                            data = data_2, method = "lm",
                            trControl = ctrl)

print(loocv_exp_2)

# re-transform the exponential predictions to calculate MAE and RMSE
# for comparison with the linear model
resids_2 <- loocv_exp_2$pred %>%
  select(pred, obs) %>%
  mutate(
    pred = exp(pred), obs = exp(obs),
    resid = abs(pred - obs),
    resid_sq = (pred - obs)^2)

mean(resids_2$resid)
sqrt(mean(resids_2$resid_sq))


