#### Model panel of Chi neighborhoods ------------------------------------------

#dependencies
library(tidyverse)
library(sf)
library(lmtest)
library(sandwich)

#set working directory
setwd("R:/Project/chi-neighborhood-change/gang-nc-analysis")

#read in spatial data for tracts
panel <- st_read("./output/chi-tract-panel.geojson")


#framework: model whether there was a change in gang presence for tracts. Since
#           this depends on prior period, stratify sample so that the model for
#           new presence at t is for tracts without gang at t-1 
#           (vice versa for new absence)
 
#           NB: Since these are differenced, they are fixed effect logit models

#prepare data for modeling
panel <- panel %>% 
  filter(YEAR > 2004, lag_pov != -999, pop_dens != 0, 
         !is.na(chg_med_hh_inc), !is.na(chg_med_rent))

#tabulation for new absent by new tif district
panel %>% 
  st_drop_geometry %>% 
  filter(lag_present) %>% 
  group_by(new_tif, new_absent) %>% 
  tally %>% 
  mutate(prop  = n/sum(n))

#tabulation for new present by new tif district
panel %>% 
  st_drop_geometry %>% 
  filter(lag_absent) %>% 
  group_by(new_tif, new_present) %>% 
  tally %>% 
  mutate(prop  = n/sum(n))


#### A. New Gang Absence Models ------------------------------------------------

## Model estimation

#estimate GLM
abs_m1 <- glm(new_absent ~ new_tif + new_high_pov + 
                chg_tot_pop + chg_entropy + chg_col_grad + chg_med_hh_inc + 
                chg_med_rent + chg_pct_hsp,
             data = panel %>% filter(lag_present),
             family = "binomial")

#classical SEs
summary(abs_m1)

#robust SEs
coeftest(abs_m1, vcov = vcovHC(abs_m1))

#BIC
BIC(abs_m1)

## Model prediction

#create grid of representative values to predict probability of new absence at
abs_inc_grid <- expand_grid(
  YEAR = unique(levels(factor(panel$YEAR))),
  new_tif = c(FALSE),
  chg_pop_dens = mean(panel %>% filter(lag_present) %>% pull(chg_pop_dens)),
  chg_entropy = mean(panel %>% filter(lag_present) %>% pull(chg_entropy)),
  dist_to_cbd = mean(panel %>% filter(lag_present) %>% pull(dist_to_cbd)),
  chg_pov = mean(panel %>% filter(lag_present) %>% pull(chg_pov)),
  chg_col_grad = mean(panel %>% filter(lag_present) %>% pull(chg_col_grad)),
  chg_med_hh_inc = seq(quantile(panel %>% filter(lag_present) %>% pull(chg_med_hh_inc), .05),
                       quantile(panel %>% filter(lag_present) %>% pull(chg_med_hh_inc), .95),
                       1000),
  chg_new_hu = mean(panel %>% filter(lag_present) %>% pull(chg_new_hu)),
  chg_vac_hu = mean(panel %>% filter(lag_present) %>% pull(chg_vac_hu)),
  chg_50_plus = mean(panel %>% filter(lag_present) %>% pull(chg_50_plus)),
  chg_rnt_occ = mean(panel %>% filter(lag_present) %>% pull(chg_rnt_occ)),
  chg_med_rent = mean(panel %>% filter(lag_present) %>% pull(chg_med_rent))
)

#predict to the grid
abs_inc_grid$pred <- predict(abs_m1, newdata = abs_inc_grid, se = TRUE, type = "response")$fit
abs_inc_grid$se <- predict(abs_m1, newdata = abs_inc_grid, se = TRUE, type = "response")$se

#visualize the predictions
ggplot(abs_inc_grid, aes(x = chg_med_hh_inc, y = pred, fill = YEAR, color = YEAR,
                     ymin = pred - 1.96 * se, ymax = pred + 1.96 * se)) +
  geom_line(lwd = 1) +
  geom_ribbon(alpha = .25, color = NA) +
  theme_minimal() +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "\nChange in Median HH Income", 
       y = "Pr(New Gang Absence between t-1 and t)\n",
       fill = "Year", color = "Year") +
  ggsave(filename = "./output/abs_model_med_inc_pred.pdf", 
         width = 8, height = 6, dpi = 300)

#create grid of representative values to predict probability of new absence at
abs_tif_grid <- expand_grid(
  YEAR = unique(levels(factor(panel$YEAR))),
  new_tif = c(TRUE, FALSE),
  chg_pop_dens = mean(panel %>% filter(lag_present) %>% pull(chg_pop_dens)),
  chg_entropy = mean(panel %>% filter(lag_present) %>% pull(chg_entropy)),
  dist_to_cbd = mean(panel %>% filter(lag_present) %>% pull(dist_to_cbd)),
  chg_pov = mean(panel %>% filter(lag_present) %>% pull(chg_pov)),
  chg_col_grad = mean(panel %>% filter(lag_present) %>% pull(chg_col_grad)),
  chg_med_hh_inc = quantile(panel %>% filter(lag_present) %>% pull(chg_med_hh_inc), .9),
  chg_new_hu = mean(panel %>% filter(lag_present) %>% pull(chg_new_hu)),
  chg_vac_hu = mean(panel %>% filter(lag_present) %>% pull(chg_vac_hu)),
  chg_50_plus = mean(panel %>% filter(lag_present) %>% pull(chg_50_plus)),
  chg_rnt_occ = mean(panel %>% filter(lag_present) %>% pull(chg_rnt_occ)),
  chg_med_rent = mean(panel %>% filter(lag_present) %>% pull(chg_med_rent))
)

#predict to the grid
abs_tif_grid$pred <- predict(abs_m1, newdata = abs_tif_grid, se = TRUE, type = "response")$fit
abs_tif_grid$se <- predict(abs_m1, newdata = abs_tif_grid, se = TRUE, type = "response")$se

#visualize the predictions
ggplot(abs_tif_grid, aes(x = new_tif, y = pred, fill = new_tif,
                         ymin = pred - 1.96 * se, ymax = pred + 1.96 * se)) +
  facet_grid(~ YEAR) +
  geom_bar(stat = "identity") +
  geom_errorbar() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "\nNew TIF District between t-1 and t", 
       y = "Pr(New Gang Absence between t-1 and t)\n",
       fill = "") +
  ggsave(filename = "./output/abs_model_new_tif_pred.pdf", 
         width = 8, height = 6, dpi = 300)


#### B. New Gang Presence Models -----------------------------------------------

## Model estimation

#estimate GLM
pres_m1 <- glm(new_present ~ new_tif + new_high_pov +
                 chg_pop_dens + chg_entropy + chg_col_grad + chg_med_hh_inc,  
              data = panel %>% filter(lag_present),
              family = "binomial")

#classical SEs
summary(pres_m1)

#robust SEs
coeftest(pres_m1, vcov = vcovCL(pres_m1)) #robust SEs


## Model prediction

#create grid of representative values to predict probability of new absence at
pres_inc_grid <- expand_grid(
  YEAR = 2010,
  new_tif = c(TRUE, FALSE),
  new_high_pov = TRUE,
  chg_pop_dens = mean(panel %>% filter(lag_absent) %>% pull(chg_pop_dens)),
  chg_entropy = mean(panel %>% filter(lag_absent) %>% pull(chg_entropy)),
  chg_pov = mean(panel %>% filter(lag_absent) %>% pull(chg_pov)),
  chg_col_grad = mean(panel %>% filter(lag_absent) %>% pull(chg_col_grad)),
  chg_med_hh_inc = mean(panel %>% filter(lag_absent) %>% pull(chg_med_hh_inc))
)

#predict to the grid
pres_inc_grid$pred <- predict(pres_m1, newdata = pres_inc_grid, se = TRUE, type = "response")$fit
pres_inc_grid$se <- predict(pres_m1, newdata = pres_inc_grid, se = TRUE, type = "response")$se

#visualize the predictions
ggplot(pres_inc_grid, aes(x = new_tif, y = pred, fill = new_tif,
                         ymin = pred - 1.96 * se, ymax = pred + 1.96 * se)) +
  facet_grid(YEAR ~ new_high_pov) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "\nNew TIF in neighborhood", 
       y = "Pr(New Gang Presence between t and t+1)\n") +
  guides(fill = FALSE) +
  ggsave(filename = "./output/pres_model_med_inc_pred.pdf", 
         width = 8, height = 6, dpi = 300)


#### C. Gang Count Models ------------------------------------------------------

test <- glm(count ~ factor(YEAR), panel, family = "quasipoisson")
summary(test)
