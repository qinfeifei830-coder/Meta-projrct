library(tidyverse) # needed for 'glimpse'
library(dmetar)
library(meta)
library(metafor)


data(ThirdWave)
glimpse(ThirdWave)
m.gen <- metagen(TE = TE,
                 seTE = seTE,
                 studlab = Author,
                 data = ThirdWave,
                 sm = "SMD",
                 common = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK",
                 title = "Third Wave Psychotherapies")
summary(m.gen)

year <- c(2014, 1998, 2010, 1999, 2005, 2014, 
          2019, 2010, 1982, 2020, 1978, 2001,
          2018, 2002, 2009, 2011, 2011, 2013)


m.gen.reg <- metareg(m.gen, ~year)
m.gen.reg
bubble(m.gen.reg, studlab = TRUE)
metareg(m.gen, RiskOfBias)
#画RiskOfBias
library(tidyverse)
library(dmetar)
data(MVRegressionData)

glimpse(MVRegressionData)

MVRegressionData[,c("reputation", "quality", "pubyear")] %>% cor()
#检查高相关性的一种快速方法是计算所有连续变量的互相关矩阵。

library(tidyverse)
library(dmetar)
data(MVRegressionData)

glimpse(MVRegressionData)

library(PerformanceAnalytics)
#检查多重共线性
MVRegressionData[,c("reputation", "quality", "pubyear")] %>% 
  chart.Correlation()

#拟合多元元回归模型
m.qual <- rma(yi = yi,
              sei = sei,
              data = MVRegressionData,
              method = "ML",
              mods = ~ quality,
              test = "knha")

m.qual
#们将输入添加到 中，这一次，我们将输出另存为 。reputation+ reputationmodsm.qual.rep
m.qual.rep <- rma(yi = yi, 
                  sei = sei, 
                  data = MVRegressionData, 
                  method = "ML", 
                  mods = ~ quality + reputation, 
                  test = "knha")

m.qual.rep
#比较两种方法
anova(m.qual, m.qual.rep)



#建模交互
# Add factor labels to 'continent'
# 0 = Europe
# 1 = North America
levels(MVRegressionData$continent) = c("Europe", "North America")

# Fit the meta-regression model
m.qual.rep.int <- rma(yi = yi, 
                      sei = sei, 
                      data = MVRegressionData, 
                      method = "REML", 
                      mods = ~ pubyear * continent, 
                      test = "knha")

m.qual.rep.int


#排列测试
permutest(m.qual.rep)

#多模型推理
multimodel.inference(TE = "yi", 
                     seTE = "sei",
                     data = MVRegressionData,
                     predictors = c("pubyear", "quality", 
                                    "reputation", "continent"),
                     interaction = TRUE)










