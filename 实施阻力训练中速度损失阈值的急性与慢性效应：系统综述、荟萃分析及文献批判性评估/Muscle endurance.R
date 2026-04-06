# needed packages
library(ggplot2)
library(tidyverse)
library(readxl)
library(metafor)
library(dmetar)

# 2. Import data ====

df_endurance <- read_excel("C:/Users/26069/Desktop/meta data/实施阻力训练中速度损失阈值的急性与慢性效应：系统综述、荟萃分析及文献批判性评估/Dataset - longitudinal studies.xlsx", 
                           sheet = "Muscle endurance")

# 3. Calculate effect sizes ====

endurance <- escalc(measure = "MC", 
                      m1i = MeanExperimentalPOST, 
                      m2i = MeanExperimentalPRE, 
                      sd1i = SDexperimentalPRE, 
                      sd2i = SDexperimentalPOST,
                      ni = Nexperimental, 
                      ri = rExperimental, 
                      data = df_endurance)

# 4. Multilevel mixed-effects meta-regression models (Only velocity loss moderator due to lack of studies/groups) ====

res1 <- rma.mv(yi = yi,
               V = vi,
               data = endurance,
               random = ~ 1 | Study/es_id,
               method = "REML", # Right away, as this is the final model
               test = "t",
               slab = Study, 
               mods = ~ VL)

# Check whether multilevel structure is essential ====

l3.removed <- rma.mv(yi = yi,
                     V = vi,
                     data = endurance,
                     random = ~ 1 | Study/es_id,
                     method = "REML",
                     test = "t",
                     slab = Study, 
                     mods = ~ VL,
                     sigma2 = c(0, NA))

anova(res1, l3.removed) # Due to the nested structure of the data, multilevel model will be retained

# 5. I2 heterogeneity by level calculation (note: this is reported for the model with moderators) ====
# The I2 for models with moderators represents estimates of how much of the unaccounted for variability is due to 
# residual heterogeneity.

i2 <- var.comp(res1)

summary(i2)

plot(i2)


# R^2 for between (level 3) and within study (level 2) heterogeneity ====
# One first has to fit the model without moderators for this purpose 

res0 <- rma.mv(yi = yi,
               V = vi,
               data = endurance,
               random = ~ 1 | Study/es_id,
               method = "REML",
               test = "t",
               slab = Study)

100 * pmax(0, (res0$sigma2 - res1$sigma2) / res0$sigma2) # 0, 0.

# 6. Dose-response relationship for velocity loss and strength gains ====
# This dataframe represents three levels of effect sizes from the "raw" data
# 1) Individual study effect size; 2) average individual threshold effect size; 3) average group threshold effect size

endurance2 <- endurance %>% 
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

rstudent.rma.mv(res1) # Not a single group exceeds an absolute value of ± 3

# Hat values

hat <- data.frame(hatvalues.rma.mv(res1)) %>% 
  rename(hat = hatvalues.rma.mv.res1.) %>% 
  filter(hat > 2 * mean(hat)) # 3 groups could be labelled as leverage points

# Cooks' distance (essentially combining outliers and leverage points into one) ====

cooks <- cooks.distance.rma.mv(res1)
cooks %>% 
  cbind(endurance$Study) %>% 
  subset(cooks > 3*mean(cooks)) %>% 
  View() 

# 0% velocity loss group in Pareja Blanco et al. (2020) [24], and 50% velocity loss in Sanchez-Moreno seem to be influential points


# Run the model without these particular groups ====

res_inf <- rma.mv(yi = yi,
                  V = vi,
                  data = filter(endurance, !es_id %in% c(5, 17)),
                  random = ~ 1 | Study/es_id,
                  method = "REML",
                  test = "t",
                  slab = Study, 
                  mods = ~ VL)


# Overall, the results are robust as exclusion of "influential" groups did not meaningfully change the interpretation
