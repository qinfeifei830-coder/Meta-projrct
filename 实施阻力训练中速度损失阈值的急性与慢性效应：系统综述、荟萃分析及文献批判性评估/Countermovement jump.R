# needed packages

library(tidyverse)
library(readxl)
library(metafor)
library(dmetar)

# 2. Import data ====

df_cmj <- read_excel("Dataset - longitudinal studies.xlsx", 
                     sheet = "CMJ") 

# 3. Calculate effect sizes ====

cmj <- escalc(measure = "MC", 
                    m1i = MeanExperimentalPOST, 
                    m2i = MeanExperimentalPRE, 
                    sd1i = SDexperimentalPRE, 
                    sd2i = SDexperimentalPOST,
                    ni = Nexperimental, 
                    ri = rExperimental, 
                    data = df_cmj)

# 4. Multilevel mixed-effects meta-regression models (Only theoretically relevant ones) ====

res1 <- rma.mv(yi = yi,
               V = vi,
               data = cmj,
               random = ~ 1 | Study/es_id,
               method = "ML", 
               test = "t",
               slab = Study, 
               mods = ~ StudyDuration) # Significant moderator (good range of values)

res2 <- rma.mv(yi = yi,
               V = vi,
               data = cmj,
               random = ~ 1 | Study/es_id,
               method = "ML",
               test = "t",
               slab = Study, 
               mods = ~ load) # Not significant moderator (good range of values)

res1 <- rma.mv(yi = yi,
               V = vi,
               data = cmj,
               random = ~ 1 | Study/es_id,
               method = "ML", 
               test = "t",
               slab = Study, 
               mods = ~ VL + StudyDuration + load)

res2 <- rma.mv(yi = yi,
               V = vi,
               data = cmj,
               random = ~ 1 | Study/es_id,
               method = "ML", 
               test = "t",
               slab = Study, 
               mods = ~ VL + StudyDuration)

res3 <- rma.mv(yi = yi,
               V = vi,
               data = cmj,
               random = ~ 1 | Study/es_id,
               method = "ML", 
               test = "t",
               slab = Study, 
               mods = ~ VL)

anova(res1, res2)
anova (res2, res3) # Model with velocity loss and study duration is the best

# Fit the final model again using REML method ====

res2 <- rma.mv(yi = yi,
               V = vi,
               data = cmj,
               random = ~ 1 | Study/es_id,
               method = "REML", 
               test = "t",
               slab = Study, 
               mods = ~ VL + StudyDuration)


# Check whether multilevel structure is essential ====

l3.removed <- rma.mv(yi = yi,
                     V = vi,
                     data = cmj,
                     random = ~ 1 | Study/es_id,
                     method = "REML",
                     test = "t",
                     slab = Study, 
                     mods = ~ VL + StudyDuration,
                     sigma2 = c(0, NA))

anova(res2, l3.removed) # Due to the nested structure of the data, multilevel model will be retained

# 5. I2 heterogeneity by level calculation (note: this is reported for the model with moderators) ====
# The I2 for models with moderators represents estimates of how much of the unaccounted for variability is due to 
# residual heterogeneity.

i2 <- var.comp(res2)

summary(i2)

plot(i2)

# R^2 for between (level 3) and within study (level 2) heterogeneity ====
# One first has to fit the model without moderators for this purpose 

res1 <- rma.mv(yi = yi,
               V = vi,
               data = cmj,
               random = ~ 1 | Study/es_id,
               method = "REML",
               test = "t",
               slab = Study)

100 * pmax(0, (res1$sigma2 - res2$sigma2) / res1$sigma2) # 100.00000  29.11845

# 6. Dose-response relationship for velocity loss and strength gains ====
# This dataframe represents three levels of effect sizes from the "raw" data
# 1) Individual study effect size; 2) average individual threshold effect size; 3) average group threshold effect size

cmj2 <- cmj %>% 
  group_by(VL) %>%
  mutate(average_it_es = mean(yi)) %>% 
  ungroup() %>% 
  group_by(threshold) %>% 
  mutate(average_gt_es = mean(average_it_es)) %>% 
  ungroup() %>% 
  mutate(VL2 = case_when(
    threshold == "Low threshold" ~ 5, # wrangling needed for the figure only
    threshold == "Moderate threshold" ~ 22.5, # wrangling needed for the figure only
    threshold == "High threshold" ~ 42.5), # wrangling needed for the figure only
    VL2 = as.numeric(VL2))

# 7. Sensitivity analysis and model diagnostics ====

# Externally studentised residuals on individual effect sizes (velocity loss threshold groups)

rstudent.rma.mv(res2) # Pareja Blanco et al. (2017) [74] 30% Velocity loss group has a high residual

# Hat values

hat <- data.frame(hatvalues.rma.mv(res2)) %>% 
  rename(hat = hatvalues.rma.mv.res2.) %>% 
  filter(hat > 2 * mean(hat)) # 3 groups could be labelled as leverage points

# Cooks' distance (essentially combining outliers and leverage points into one) ====

cooks <- cooks.distance.rma.mv(res2)
cooks %>% 
  cbind(cmj$Study) %>% 
  subset(cooks > 3*mean(cooks)) %>% 
  View() 

# Fernandez-Ortega [66], 0% velocity loss group in Pareja Blanco et al. (2020) [25], and Pareja Blanco et al. (2017) [74] 30% Velocity loss group
# seem to be influential points


# Run the model without these particular groups ====

res_inf <- rma.mv(yi = yi,
                  V = vi,
                  data = filter(cmj, !es_id %in% c(2, 8, 9)),
                  random = ~ 1 | Study/es_id,
                  method = "REML",
                  test = "t",
                  slab = Study, 
                  mods = ~ VL + StudyDuration)


# Overall, the results are robust as exclusion of "influential" groups did not meaningfully change the interpretation
# The effects of velocity loss and study duration are even more "reliable" (due to narrower CI).

