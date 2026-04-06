# needed packages

library(tidyverse)
library(readxl)
library(metafor)
library(dmetar)
library(clubSandwich)

# 2. Import data ====

df_submax <- read_excel("C:/Users/26069/Desktop/meta data/实施阻力训练中速度损失阈值的急性与慢性效应：系统综述、荟萃分析及文献批判性评估/Dataset - longitudinal studies.xlsx", 
                        sheet = "Velocity @ submax loads")

# 3. Calculate effect sizes ====

submax <- escalc(measure = "SMCR", 
                 m1i = MeanExperimentalPOST, 
                 m2i = MeanExperimentalPRE, 
                 sd1i = SDexperimentalPRE, 
                 ni = Nexperimental, 
                 ri = rExperimental, 
                 data = df_submax)

# Impute variance-covariance matrix with a default correlation between outcomes assumed to be 0.6 across the studies in a working model

V <- impute_covariance_matrix(vi = submax$vi, cluster = submax$study_id, r = 0.6)

# 4. Multivariate random-effects meta-regression models (Only theoretically relevant, predetermined ones) ====

res1 <- rma.mv(y = yi, 
               V = V, 
               random = list(~ Loads_group | Study, ~ 1 | es_id),
               struct = "UN",
               test = "t",
               method = "ML",
               slab = Study,
               data = submax,
               mods = ~ VL*Loads_group + StudyDuration + body + load)

res2 <- rma.mv(yi = yi, 
               V = V, 
               random = list(~ Loads_group | Study, ~ 1 | es_id),
               struct = "UN",
               test = "t",
               method = "ML",
               slab = Study,
               data = submax,
               mods = ~ VL*Loads_group)


# 5. Test which model is the "best" ====

anova(res1, res2) # There is essentially no difference between the complex model and the "simple" model with only velocity loss and the outcome
# In fact, "simpler" model is actually slightly better due to lower (AICc), and will be retained to keep the model parsimony.

# 6. Refit the model using REML method ====

res2 <- rma.mv(yi = yi, 
               V = V, 
               random = list(~ Loads_group | Study, ~ 1 | es_id),
               struct = "UN",
               test = "t",
               method = "REML",
               slab = Study,
               data = submax,
               mods = ~ VL*Loads_group)


# 7. Dose-response relationship for velocity loss and velocity at submax loads gains ====
# This dataframe represents three levels of effect sizes from the "raw" data
# 1) Individual study effect size; 2) average individual threshold effect size; 3) average group threshold effect size

submax2 <- submax %>% 
  group_by(Loads_group, VL) %>%
  mutate(average_it_es = mean(yi)) %>% 
  ungroup() %>% 
  group_by(Loads_group, threshold) %>% 
  mutate(average_gt_es = mean(average_it_es)) %>% 
  ungroup() %>% 
  mutate(VL2 = case_when(
    threshold == "Low threshold" ~ 5, # wrangling needed for the figure only
    threshold == "Moderate threshold" ~ 22.5, # wrangling needed for the figure only
    threshold == "High threshold" ~ 42.5), # wrangling needed for the figure only
    VL2 = as.numeric(VL2))

# 8. Sensitivity analysis and model diagnostics ====

# Externally studentised residuals on individual effect sizes (velocity loss threshold groups)

rstudent.rma.mv(res2) # No groups exceed an absolute value of ± 3

# Hat values

hat <- data.frame(hatvalues.rma.mv(res2)) %>% 
  rename(hat = hatvalues.rma.mv.res2.) %>% 
  filter(hat > 2 * mean(hat)) # 11 groups could be labeled as leverage points

# Cooks' distance (combining outliers and leverage points into one) ====

cooks <- cooks.distance.rma.mv(res2)
cooks %>% 
  cbind(submax$Study) %>% 
  subset(cooks > 3*mean(cooks)) %>% 
  View() # There are 7 influential groups

# Visualise the above
plot(cooks.distance(res2), type="o", pch=19)

# Remove the influential groups and impute a new variance-covariance matrix without them

submax_inf <- submax %>% 
  filter(!es_id %in% c(10, 19, 27, 33, 34, 69, 73))

V2 <- impute_covariance_matrix(vi = submax_inf$vi, cluster = submax_inf$study_id, r = 0.6)

# Run the model without these groups ====

res_inf <- rma.mv(yi = yi, 
               V = V2, 
               random = list(~ Loads_group | Study, ~ 1 | es_id),
               struct = "UN",
               test = "t",
               method = "REML",
               slab = Study,
               data = submax_inf,
               mods = ~ VL*Loads_group)

coef_test(res_inf, vcov = "CR2")
conf_int(res_inf, vcov = "CR2")

# Overall results have changed after the exclusion of the "influential" studies. Since there were multiple groups (6) that changed the interpretation
# of the model considerably, which now aligns with the raw data more closely, visual representation of the model will be presented without influential
# groups

# 9. Sensitivity check (different correlations between outcomes) ====

# r = 0.4 ====
V2 <- impute_covariance_matrix(vi = submax_inf$vi, cluster = submax_inf$study_id, r = 0.4)

res_inf <- rma.mv(yi = yi, 
                  V = V2, 
                  random = list(~ Loads_group | Study, ~ 1 | es_id),
                  struct = "UN",
                  test = "t",
                  method = "REML",
                  slab = Study,
                  data = submax_inf,
                  mods = ~ VL*Loads_group)

coef_test(res_inf, vcov = "CR2") # Overall results are robust as the interaction remained intact with the assumption of r = .4

# r = 0.8 ====
V2 <- impute_covariance_matrix(vi = submax_inf$vi, cluster = submax_inf$study_id, r = 0.8)

res_inf <- rma.mv(yi = yi, 
                  V = V2, 
                  random = list(~ Loads_group | Study, ~ 1 | es_id),
                  struct = "UN",
                  test = "t",
                  method = "REML",
                  slab = Study,
                  data = submax_inf,
                  mods = ~ VL*Loads_group)

coef_test(res_inf, vcov = "CR2") # Overall results are robust as the interaction is still significant with the assumption of r = .8

# 10. Dose-response relationship for velocity loss and velocity at submax loads gains ====
# This dataframe represents three levels of effect sizes from the "raw" data
# 1) Individual study effect size; 2) average individual threshold effect size; 3) average group threshold effect size

submax2 <- submax %>% 
  group_by(Loads_group, VL) %>%
  mutate(average_it_es = mean(yi)) %>% 
  ungroup() %>% 
  group_by(Loads_group, threshold) %>% 
  mutate(average_gt_es = mean(average_it_es)) %>% 
  ungroup() %>% 
  mutate(VL2 = case_when(
    threshold == "Low threshold" ~ 5, # wrangling needed for the figure only
    threshold == "Moderate threshold" ~ 22.5, # wrangling needed for the figure only
    threshold == "High threshold" ~ 42.5), # wrangling needed for the figure only
    VL2 = as.numeric(VL2))


