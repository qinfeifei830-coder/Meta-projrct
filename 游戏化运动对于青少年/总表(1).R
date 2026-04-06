library(metafor)
library(readxl)
library(dplyr)
library(rms)
library(Hmisc)             
library(esc)
library(ggplot2)
library(dmetar)
library(meta)
library(rms)
library(splines)
library(mgcv)
library(tidyverse)
library(dmetar)  

my_data<- read_excel("C:/Users/26069/Desktop/游戏化运动对于青少年/游戏化运动对于青少年基线表格.xlsx")
#将强度转换为因子变量
full.model <- rma.mv(G, ViG, slab = author, random = ~ 1 | studyid,  test = "t", method = "REML", data=my_data)
full.model

# 总结该模型
model_summary <- summary(full.model)

# 提取研究间异质性（tau²）
between_heterogeneity <- model_summary$tau2
print(between_heterogeneity)

#提取研究间异质性（sigma²）
within_heterogeneity <- model_summary$sigma2
print(within_heterogeneity)

#forest plot
#步骤 1：按效应量估计值（降序）对数据框进行排序
my_data_sorted <- my_data[order(-my_data$G), ]

# 步骤 2：使用排序后的数据拟合模型
full.model_sorted <- rma.mv(G, ViG, slab = author, random = ~ 1 | studyid, test = "t", method = "REML", data = my_data_sorted)

#步骤 3：创建森林图
#获取模型摘要
model_summary <- summary(full.model_sorted)

# 提取汇总效应估计值
summary_effect <- model_summary$b
#提取汇总效应的 95% 置信区间
ci_lower <- model_summary$ci.lb
ci_upper <- model_summary$ci.ub

# 使用自定义的 x 轴范围创建森林图
forest(full.model_sorted,  xlim = c(-5, 5), cex=0.6)

# 提取汇总效应估计值及其 95% 置信区间
model_summary <- summary(full.model_sorted)
summary_effect <- model_summary$b
ci_lower <- model_summary$ci.lb
ci_upper <- model_summary$ci.ub

# 为摘要效果添加一条垂直线
abline(v = summary_effect, col = "blue", lty = 2, lwd = 2)

# 从森林图中获取当前的 y 轴范围
ylim <- par("usr")[3:4]

# 使用图表的完整高度，为汇总效应的 95% 置信区间添加一个阴影带
polygon(x = c(ci_lower, ci_upper, ci_upper, ci_lower), 
        y = c(ylim[1], ylim[1], ylim[2], ylim[2]),  # Full height of the plot
        col = rgb(0, 0, 1, 0.2),  # Semi-transparent blue color
        border = NA)  # No border

# 添加一个 x 轴标签
mtext("Hedges' g", side = 1, line = 1, cex = 0.6)  # Adjust `line` for label position and `cex` for font size


# 添加一个 x 轴标签
funnel(full.model, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"))

# Calculate Egger's test 

# 创建一个包含用于埃格检验所需变量的新数据框
egger_data <- with(my_data, data.frame(G, SeG))

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

# 计算全模式下的I2
W <- diag(1/full.model$vi)
X <- model.matrix(full.model)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(full.model$sigma2) / (sum(full.model$sigma2) + (full.model$k-full.model$p)/sum(diag(P)))

# 使用标准化残差检查异常值
outliers <- resid(full.model, type = "rstudent")

# 识别潜在的异常值（常用的临界值是绝对值＞2）
potential_outliers <- which(abs(outliers) > 2)
potential_outliers

# 从数据框中移除潜在的异常值
df_no_outliers <- my_data[-potential_outliers, ]

# 在不考虑潜在异常值的情况下重复该分析
full.model_no_outliers <- rma.mv(yi = G, V = ViG, slab = author, random = ~ 1 | studyid, test = "t", method = "REML", data = df_no_outliers)

#展示模型去除异常值后的结果
summary(full.model_no_outliers)

# 总结该模型
model_summary_no_outliers <- summary(full.model_no_outliers)

# 总结该模型
between_heterogeneity <- model_summary_no_outliers$tau2
print(between_heterogeneity)

# 提取研究内异质性（σ²）
within_heterogeneity <- model_summary_no_outliers$sigma2
print(within_heterogeneity)

# 计算无异常值模型的I2值
W <- diag(1/full.model_no_outliers$vi)
X <- model.matrix(full.model_no_outliers)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(full.model_no_outliers$sigma2) / (sum(full.model_no_outliers$sigma2) + (full.model_no_outliers$k-full.model_no_outliers$p)/sum(diag(P)))

