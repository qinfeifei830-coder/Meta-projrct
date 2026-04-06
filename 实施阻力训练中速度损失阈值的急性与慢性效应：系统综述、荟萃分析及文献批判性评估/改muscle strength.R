library(ggplot2)
library(tidyverse)
library(readxl)
library(metafor)
library(dmetar)
library(influenceR)
library(meta)
library(grid)


df_strength <- read_excel("C:/Users/26069/Desktop/meta data/实施阻力训练中速度损失阈值的急性与慢性效应：系统综述、荟萃分析及文献批判性评估/Dataset - longitudinal studies.xlsx", 
                          sheet = "Muscle strength")


strength <- escalc(measure = "SMCR", #标准化前后测均值变化量
                   m1i = MeanExperimentalPOST, 
                   m2i = MeanExperimentalPRE, 
                   sd1i = SDexperimentalPRE, 
                   ni = Nexperimental, 
                   ri = rExperimental, 
                   data = df_strength)

strength[,c("VL", "StudyDuration","StrengthLevel")] %>% cor()
multimodel.inference(TE = "yi", 
                     seTE = "vi",
                     data = strength,
                     predictors = c("VL", "body", 
                                    "StrengthLevel", "load","StudyDuration"),
                     interaction = TRUE)


res2 <- rma.mv(yi = yi,
               V = vi,
               data = strength,
               random = ~ 1 | Study/es_id,
               method = "REML",
               test = "t",
               slab = Study, 
               mods = ~ VL + body + StrengthLevel + StudyDuration)
summary(res2)

i2 <- var.comp(res2)
summary(i2)

res1 <- rma.mv(yi = yi,
               V = vi,
               data = strength,
               random = ~ 1 | Study/es_id,
               method = "REML",
               test = "t",
               slab = Study)

100 * pmax(0, (res1$sigma2 - res2$sigma2) / res1$sigma2)


strength2 <- strength %>% 
  group_by(VL) %>%
  mutate(average_it_es = mean(yi)) %>% 
  ungroup() %>% 
  group_by(threshold) %>% 
  mutate(average_gt_es = mean(average_it_es)) %>% 
  ungroup() %>% 
  mutate(VL2 = case_when(
    threshold == "Low threshold" ~ 5, # wrangling needed for the figure only仅为该图表需要进行数据整理
    threshold == "Moderate threshold" ~ 22.5, # wrangling needed for the figure only
    threshold == "High threshold" ~ 42.5), # wrangling needed for the figure only
    VL2 = as.numeric(VL2)) 



m.gen <- metagen(TE = yi,
                 seTE = vi,
                 studlab = Study,
                 data =strength,
                 sm = "SMD",
                 common = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK",
                 title = "Third Wave Psychotherapies")

m.rma <- rma(yi = m.gen$TE,
             sei = m.gen$seTE,
             method = m.gen$method.tau,
             test = "knha")

res.gosh <- gosh(m.rma)
plot(res.gosh, alpha = 0.01)
res.gosh.diag <- gosh.diagnostics(res.gosh, 
                                  km.params = list(centers = 2),
                                  db.params = list(eps = 0.08, 
                                                   MinPts = 50))
res.gosh.diag
plot(res.gosh.diag)





