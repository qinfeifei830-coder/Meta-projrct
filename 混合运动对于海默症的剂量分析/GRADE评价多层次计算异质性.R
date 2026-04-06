library(dplyr)
library(readxl)
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
# 读取数据
my_data<- read_excel("C:/Users/26069/Desktop/混合运动对于海默症的剂量分析/新行为认知指标改.xlsx")

# 分组拆分
data_0 <- my_data %>% filter(Exercise_time_per_week_coded == 0)
data_1 <- my_data %>% filter(Exercise_time_per_week_coded == 1)
data_2 <- my_data %>% filter(Per_session_exercise_duration_coded == 2)
full.model0 <- rma.mv(G, ViG, slab = author, random = ~ 1 | studyid,  test = "t", method = "REML", data=data_0)
full.model0
full.model1 <- rma.mv(G, ViG, slab = author, random = ~ 1 | studyid,  test = "t", method = "REML", data=data_1)
full.model1
full.model2 <- rma.mv(G, ViG, slab = author, random = ~ 1 | studyid,  test = "t", method = "REML", data=data_2)
full.model2
#计算全模式下的I2
W <- diag(1/full.model0$vi)
X <- model.matrix(full.model0)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(full.model0$sigma2) / (sum(full.model0$sigma2) + (full.model0$k-full.model0$p)/sum(diag(P)))
#计算全模式下的I2
W <- diag(1/full.model1$vi)
X <- model.matrix(full.model1)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(full.model1$sigma2) / (sum(full.model1$sigma2) + (full.model1$k-full.model1$p)/sum(diag(P)))
#计算全模式下的I2
W <- diag(1/full.model2$vi)
X <- model.matrix(full.model2)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(full.model2$sigma2) / (sum(full.model2$sigma2) + (full.model2$k-full.model1$p)/sum(diag(P)))

