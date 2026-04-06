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
my_data<- read_excel("C:/Users/26069/Desktop/择时运动/血糖数据提取表改-2026.03.19-ZYW.xlsx", 
                      sheet = "周期性干预Data")

df_es <- escalc(
  measure = "SMD",
  m1i = int_mean, sd1i = int_sd, n1i = int_samplesize,
  m2i = con_mean, sd2i = con_sd, n2i = con_samplesize,
  data = my_data
)

my_data<- df_es
# 分组拆分
data_0 <- my_data %>% filter(Regions_of_the_country_coded == 0)
data_1 <- my_data %>% filter(Exercise_time_period_coded == 1)
data_2 <- my_data %>% filter(Exercise_time_period_coded == 2)
data_3 <- my_data %>% filter(Exercise_time_period_coded == 3)
full.model0 <- rma.mv(yi, vi, slab = author, random = ~ 1 | studyid,  test = "t", method = "REML", data=data_0)
full.model0
full.model1 <- rma.mv(yi, vi, slab = author, random = ~ 1 | studyid,  test = "t", method = "REML", data=data_1)
full.model1
full.model2 <- rma.mv(yi, vi, slab = author, random = ~ 1 | studyid,  test = "t", method = "REML", data=data_2)
full.model2
full.model3 <- rma.mv(yi, vi, slab = author, random = ~ 1 | studyid,  test = "t", method = "REML", data=data_3)
full.model3
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
100 * sum(full.model2$sigma2) / (sum(full.model2$sigma2) + (full.model2$k-full.model2$p)/sum(diag(P)))
#计算全模式下的I2
W <- diag(1/full.model3$vi)
X <- model.matrix(full.model3)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(full.model3$sigma3) / (sum(full.model3$sigma3) + (full.model3$k-full.model3$p)/sum(diag(P)))

