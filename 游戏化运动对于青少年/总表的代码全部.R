setwd("C:/Users/PC/Desktop/森林图")
library(meta)
library(grid)
library(readxl)
library(dplyr)
library(metafor)
library(dmetar)
library(gridExtra)
library(ggplot2)
# 合并数据并处理可能的缺失值

combined_data  <-  read_excel("C:/Users/PC/Desktop/游戏化运动对于青少年/游戏化运动对于青少年基线表格.xlsx", 
                              sheet = "All")
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
  method.tau = "REML")
meta_analysis
png("All.png", width = 1200, height = 800, res = 100)
meta::forest(meta_analysis,xlim = c(-5, 5),cex = 0.6,layout = "RevMan5",col.square = "skyblue")
dev.off()
# 为等高线定义填充颜色
col.contour = c("gray75", "gray85", "gray95")
# #生成漏斗图（此处未标注研究名称）
meta::funnel(meta_analysis, xlim = c(-0.5, 2),contour = c(0.9, 0.95, 0.99),col.contour = col.contour)
# Add a legendlegend
legend(x = 1.6, y = 0.01,legend = c("p < 0.1", "p < 0.05", "p < 0.01"),fill = col.contour)
# Add a title
title(" Funnel Plot")
#影响性分析
m.gen.inf <- InfluenceAnalysis(meta_analysis, random = TRUE)
#划地块图
plot(m.gen.inf, "baujat")
#影响因素诊断
plot(m.gen.inf, "influence")
#留一法图片
plot(m.gen.inf, "es")
plot(m.gen.inf, "i2")
#gosh方法
m.gen<-meta_analysis
m.rma <- rma(yi = m.gen$TE,sei = m.gen$seTE,method = m.gen$method.tau,test = "knha")
res.gosh <- gosh(m.rma)
plot(res.gosh, alpha = 0.01)
res.gosh.diag <- gosh.diagnostics(res.gosh,km.params = list(centers = 2),db.params = list(eps = 0.08,MinPts = 50))
res.gosh.diag
plot(res.gosh.diag)

# Calculate Egger's test 

# 创建一个包含用于埃格检验所需变量的新数据框
egger_data <- with(combined_data, data.frame(G, SeG))

# 将smd及其标准误差转换为无截距模型变量
egger_data$y <- egger_data$G / egger_data$SeG  # 标准正态离差
egger_data$x <- 1 / egger_data$SeG^2  # 精度的平方

# 为埃格检验拟合加权回归模型
egger_model <- lm(y ~ x, data = egger_data)
egger_model
# 获取回归模型的结果，以提取埃格检验的统计量和 p 值
egger_results <- summary(egger_model)

# 获取回归模型的结果，以提取埃格检验的统计量和 p 值
eggers_value <- coef(egger_model)[1]
p_value <- coef(summary(egger_model))[1,4]

# 输出埃格检验的统计量和p值
list(Eggers_Value = eggers_value, P_Value = p_value)

