setwd("C:/Users/PC/Desktop/森林图")
library(meta)
library(meta)
library(grid)
library(readxl)
library(dplyr)
# 合并数据并处理可能的缺失值

combined_data  <-  read_excel("C:/Users/PC/Desktop/游戏化运动对于青少年/游戏化运动对于青少年基线表格.xlsx", 
                                        sheet = "Exercise Intensity")
# 创建元分析对象，包含所有研究
meta_analysis <- meta::metacont(
  n.e = combined_data$int_samplesize, 
  mean.e = combined_data$int_mean, 
  sd.e = combined_data$int_sd,
  n.c = combined_data$con_samplesize, 
  mean.c = combined_data$con_mean, 
  sd.c = combined_data$con_sd,
  studlab = combined_data$author, 
  sm = "SMD",  # 使用标准化均值差作为效应量
  method.smd = "Hedges",#采用Hedges作为g检验
  random = TRUE,  # 使用随机效应模型
  common = FALSE,
  method.tau = "REML", 
  subgroup = combined_data$Subgroup)
meta_analysis
png("Exercise Intensity.png", width = 1200, height = 800, res = 100)
meta::forest(meta_analysis,xlim = c(-5, 5),cex = 0.6,layout = "RevMan5",col.square = "skyblue")
dev.off()
