# needed packages

library(tidyverse)
library(readxl)
library(metafor)
library(dmetar)

# 1. Set working directory ====

setwd("/Users/ivanjukic/Desktop/") # on Mac

# 2. Import data ====

df_sprint <- read_excel("Dataset - longitudinal studies.xlsx", sheet = 6) 

# 3. Calculate effect sizes ====

sprint <- escalc(measure = "MC", 
              m1i = MeanExperimentalPOST, 
              m2i = MeanExperimentalPRE, 
              sd1i = SDexperimentalPRE, 
              sd2i = SDexperimentalPOST,
              ni = Nexperimental, 
              ri = rExperimental, 
              data = df_sprint)

# 4. Multilevel mixed-effects meta-regression models (Only theoretically relevant ones) ====

res1 <- rma.mv(yi = yi,
               V = vi,
               data = sprint,
               random = ~ 1 | Study/es_id,
               method = "ML", 
               test = "t",
               slab = Study, 
               mods = ~ StudyDuration) # Significant moderator (good range of values)

res2 <- rma.mv(yi = yi,
               V = vi,
               data = sprint,
               random = ~ 1 | Study/es_id,
               method = "ML",
               test = "t",
               slab = Study, 
               mods = ~ load) # Not significant moderator (good range of values)

res3 <- rma.mv(yi = yi,
               V = vi,
               data = sprint,
               random = ~ 1 | Study/es_id,
               method = "ML",
               test = "t",
               slab = Study, 
               mods = ~ StrengthLevel) # Not significant moderator (good range of values)

res1 <- rma.mv(yi = yi,
               V = vi,
               data = sprint,
               random = ~ 1 | Study/es_id,
               method = "ML", 
               test = "t",
               slab = Study, 
               mods = ~ VL)

res2 <- rma.mv(yi = yi,
               V = vi,
               data = sprint,
               random = ~ 1 | Study/es_id,
               method = "ML", 
               test = "t",
               slab = Study, 
               mods = ~ VL + StudyDuration)

anova(res1, res2) # Model with velocity loss and study duration is the best

# Fit the final model again using REML method ====

res2 <- rma.mv(yi = yi,
               V = vi,
               data = sprint,
               random = ~ 1 | Study/es_id,
               method = "REML", 
               test = "t",
               slab = Study, 
               mods = ~ VL + StudyDuration)


# Check whether multilevel structure is essential ====

l3.removed <- rma.mv(yi = yi,
                     V = vi,
                     data = sprint,
                     random = ~ 1 | Study/es_id,
                     method = "REML",
                     test = "t",
                     slab = Study, 
                     mods = ~ VL + StudyDuration,
                     sigma2 = c(0, NA))

anova(res2, l3.removed) # Multilevel model is absolutely necessary 

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
               data = sprint,
               random = ~ 1 | Study/es_id,
               method = "REML",
               test = "t",
               slab = Study)

100 * pmax(0, (res1$sigma2 - res2$sigma2) / res1$sigma2) # 33.75372 76.93038

# 6. Dose-response relationship for velocity loss and strength gains ====
# This dataframe represents three levels of effect sizes from the "raw" data
# 1) Individual study effect size; 2) average individual threshold effect size; 3) average group threshold effect size


sprint2 <- sprint %>% 
  group_by(VL) %>%
  mutate(average_it_es = mean(yi)) %>% 
  ungroup() %>% 
  group_by(threshold) %>% 
  mutate(average_gt_es = mean(average_it_es)) %>% 
  ungroup() %>% 
  mutate(VL2 = case_when(
    threshold == "Low threshold" ~ 5,
    threshold == "Moderate threshold" ~ 22.5,
    threshold == "High threshold" ~ 42.5),
    VL2 = as.numeric(VL2))

# 7. Sensitivity analysis and model diagnostics ====

# Externally studentised residuals on individual effect sizes (velocity loss threshold groups)

rstudent.rma.mv(res2) # Not a single group exceeds an absolute value of ± 3

# Hat values

hat <- data.frame(hatvalues.rma.mv(res2)) %>% 
  rename(hat = hatvalues.rma.mv.res2.) %>% 
  filter(hat > 2 * mean(hat)) # 2 groups could be labelled as leverage points

# Cooks' distance (essentially combining outliers and leverage points into one) ====

cooks <- cooks.distance.rma.mv(res2)
cooks %>% 
  cbind(sprint$Study) %>% 
  subset(cooks > 3*mean(cooks)) %>% 
  View() 

# Fernandez-Ortega [66], 0% and 40% velocity loss groups in Pareja Blanco et al. (2020) [25] seem to be influential points

# Run the model without these particular groups ====

res_inf <- rma.mv(yi = yi,
                  V = vi,
                  data = filter(sprint, !es_id %in% c(1, 11, 12)),
                  random = ~ 1 | Study/es_id,
                  method = "REML",
                  test = "t",
                  slab = Study, 
                  mods = ~ VL + StudyDuration)


# Overall, the results with regards to velocity loss are robust as exclusion of "influential" groups did not meaningfully change the interpretation
# The effects of velocity loss are even more "reliable" (due to narrower CI). However, study duration is not a significant moderator anymore


