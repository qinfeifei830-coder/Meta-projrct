# needed packages
library(ggplot2)
library(tidyverse)
library(readxl)
library(metafor)
library(dmetar)

# 2. Import data ====

df_strength <- read_excel("C:/Users/26069/Desktop/meta data/实施阻力训练中速度损失阈值的急性与慢性效应：系统综述、荟萃分析及文献批判性评估/Dataset - longitudinal studies.xlsx", 
                          sheet = "Muscle strength")

# 3. Calculate effect sizes ====计算效应量

strength <- escalc(measure = "SMCR", #标准化前后测均值变化量
                   m1i = MeanExperimentalPOST, 
                   m2i = MeanExperimentalPRE, 
                   sd1i = SDexperimentalPRE, 
                   ni = Nexperimental, 
                   ri = rExperimental, 
                   data = df_strength)

# 4. Multilevel mixed-effects meta-regression models (Only theoretically relevant, predetermined ones) ====
    #多级混合效应元回归模型（仅理论上相关的、预先确定的模型）
res1 <- rma.mv(yi = yi,
               V = vi,
               data = strength,
               random = ~ 1 | Study/es_id,
               method = "ML",
               test = "t",
               slab = Study, 
               mods = ~ VL + body + StrengthLevel + StudyDuration + load)

res2 <- rma.mv(yi = yi,
               V = vi,
               data = strength,
               random = ~ 1 | Study/es_id,
               method = "ML",
               test = "t",
               slab = Study, 
               mods = ~ VL + body + StrengthLevel + StudyDuration)

res3 <- rma.mv(yi = yi,
               V = vi,
               data = strength,
               random = ~ 1 | Study/es_id,
               method = "ML",
               test = "t",
               slab = Study, 
               mods = ~ VL*body*StrengthLevel + StudyDuration)

# 5. Test which model is the "best" ====测试哪个模型是“最佳”的
#  8.3.3.5 多模型推理这个更好在考虑到多模态问题
anova(res1, res2)
anova(res2, res3) 
# Complex model with interactions did not improve the fit 具有交互作用的复杂模型并没有改善拟合效果

# 6. Refit the model using REML method ====使用REML方法重新拟合模型

res2 <- rma.mv(yi = yi,
               V = vi,
               data = strength,
               random = ~ 1 | Study/es_id,
               method = "REML",
               test = "t",
               slab = Study, 
               mods = ~ VL + body + StrengthLevel + StudyDuration)

# Check whether multilevel structure is essential ====检查多级结构是否必不可少

l3.removed <- rma.mv(yi = yi,
                     V = vi,
                     data = strength,
                     random = ~ 1 | Study/es_id,
                     method = "REML",
                     test = "t",
                     slab = Study, 
                     mods = ~ VL + body + StrengthLevel + StudyDuration,
                     sigma2 = c(0, NA))

anova(res2, l3.removed) # It is important to keep the three level structure.保持三级结构很重要。

# Check correlations between continuous moderators ====检查连续调节变量之间的相关性
#检查多重共线性这个更让好啊
strength[,c("VL", "StrengthLevel", "StudyDuration")] %>% cor() # it's ok (<.43)

# 7. I2 heterogeneity by level calculation (note: this is reported for the model with moderators) ====
#按水平计算的I2异质性（注：这是针对含调节变量的模型报告的）
# The I2 for models with moderators represents estimates of how much of the unaccounted for variability is due to 
# 包含调节变量的模型的I²值代表了对未解释变异中由……引起的部分的估计。
# residual heterogeneity.
#残余异质性。
i2 <- var.comp(res2)

summary(i2)

plot(i2)

# R^2 for between (level 3) and within study (level 2) heterogeneity ====研究间（第3层）和研究内（第2层）异质性的R²
# One first has to fit the model without moderators for this purpose 为此，首先必须在没有调节变量的情况下拟合模型。

res1 <- rma.mv(yi = yi,
               V = vi,
               data = strength,
               random = ~ 1 | Study/es_id,
               method = "REML",
               test = "t",
               slab = Study)

100 * pmax(0, (res1$sigma2 - res2$sigma2) / res1$sigma2) # 48.97833, 92.06504

#代码拆解
#res1$sigma2 和 res2$sigma2
#这两个对象来自metafor包中rma或rma.mv拟合的随机效应模型结果，其中sigma2表示随机效应的方差成分。
#通常res1是包含更多随机效应或更复杂结构的模型，res2是简化模型（如移除了某个调节变量或随机效应项）。
#(res1$sigma2 - res2$sigma2) / res1$sigma2
#计算 “方差减少比例”：（原模型方差 - 简化模型方差）/ 原模型方差，衡量简化模型相比原模型的方差减少幅度。
#pmax(0, ...)
#确保结果不小于 0（如果简化模型方差反而更大，会返回 0，避免出现负的百分比）。
#100 * ...
#将比例转换为百分比。
#为什么有两个值？
#这是因为你的模型（res1和res2）是多水平随机效应模型（例如包含嵌套结构，如random = ~1 | Study/es_id），此时sigma2会返回多个层级的随机效应方差。：
#第一个值（48.97833）可能对应 “研究间（Study）” 随机效应的方差减少百分比；
#第二个值（92.06504）可能对应 “研究内效应量（es_id）” 随机效应的方差减少百分比。

# 8. Dose-response relationship for velocity loss and strength gains ====速度损失与力量增长的剂量-反应关系
# This dataframe represents three levels of effect sizes from the "raw" data这个数据框代表了来自“原始”数据的三个效应量水平
# 1) Individual study effect size; 2) average individual threshold effect size; 3) average group threshold effect size
#1) 个体研究效应量；2) 平均个体阈值效应量；3) 平均组阈值效应量

# wrangling needed for the figure only

#这段R代码使用dplyr包对`strength`数据框进行了一系列数据处理操作，主要目的是计算不同分组的平均值并创建一个新变量用于可视化：

#1. 首先按`VL`分组，计算每组中`yi`的平均值并存储在新变量`average_it_es`中
#2. 取消分组后，再按`threshold`分组，计算每组中`average_it_es`的平均值并存储在`average_gt_es`中
#3. 最后创建新变量`VL2`，根据`threshold`的不同取值赋予特定数值：
#- "Low threshold"对应5
#- "Moderate threshold"对应22.5
#- "High threshold"对应42.5
#4. 将`VL2`转换为数值型
#注释说明创建`VL2`是"仅为该图表需要进行的数据整理"，表明这一步是为了可视化目的而进行的特殊数据转换。
#最终处理结果存储在新数据框`strength2`中。

# 9. Sensitivity analysis and model diagnostics ====敏感性分析和模型诊断

# Externally studentised residuals on individual effect sizes (velocity loss threshold groups)
#个体效应量的外部学生化残差（速度损失阈值组）
rstudent.rma.mv(res2) # not a single group exceeds an absolute value of ± 3
#没有任何一个组的绝对值超过±3
# Hat values主要用途：识别 “高杠杆点”
#帽子值的核心作用是定位高杠杆点（High Leverage Points）—— 这类观测值因自变量极端，可能 “强行拉动” 回归拟合线，导致模型对整体数据的代表性下降。

hat <- data.frame(hatvalues.rma.mv(res2)) %>% 
  rename(hat = hatvalues.rma.mv.res2.) %>% 
  filter(hat > 2 * mean(hat)) # 4 groups could be labelled as leverage points# 4个组别可以被标记为杠杆点

# Cooks' distance (essentially combining outliers and leverage points into one) ====
#简言之，Cooks' distance 是回归诊断的 “放大镜”，帮助研究者定位那些可能 “扭曲” 回归结论的 “关键观测值”，避免因个别极端点导致模型偏差。
cooks <- cooks.distance.rma.mv(res2)
cooks %>% 
  cbind(strength$Study) %>% 
  subset(cooks > 3*mean(cooks)) %>% 
  View() # Fernandez-Ortega and Sanchez-Moreno seems to contain influential groups
## 费尔南德斯-奥尔特加和桑切斯-莫雷诺似乎包含有影响力的群体
# Visualise the above将上述内容可视化
plot(cooks.distance(res2), type="o", pch=19)
  
# Run the model without these two studies ====
#在不包含这两项研究的情况下运行该模型
res_inf <- rma.mv(yi = yi,
               V = vi,
               data = filter(strength, !Study %in% c("Fernandez-Ortega et al. 2020 [66]", "Sanchez-Moreno et al. 2020 [77]")),
               random = ~ 1 | Study/es_id,
               method = "REML",
               test = "t",
               slab = Study, 
               mods = ~ VL + body + StrengthLevel + StudyDuration) 

# overall results are robust to the exclusion of the "influential" studies
# 整体结果在排除“有影响力的”研究后仍然稳健

