library(metafor)
library(ggplot2)
library(smplot2)
library(readxl)
# 1. 读取数据 & 计算效应量
my_data <- read_excel("C:/Users/26069/Desktop/择时运动/血糖数据提取表改-2026.03.19-ZYW.xlsx", 
                      sheet = "急性干预Data")
df_es <- escalc(
  measure = "SMD",
  m1i = int_mean, sd1i = int_sd, n1i = int_samplesize,
  m2i = con_mean, sd2i = con_sd, n2i = con_samplesize,
  data = my_data
)
my_data <- df_es
# 2. 转为因子分组
my_data$Exercise_Intensity_coded <- as.factor(my_data$Exercise_time_period_coded)
# 3. 最终绘图（含灰色标准线）
ggplot(data = my_data, mapping = aes(x = Exercise_Intensity_coded, y = yi, fill = Exercise_Intensity_coded)) +
  # 先画灰色标准线（yi=0）
  geom_hline(
    yintercept = 0,
    color = "gray",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  sm_raincloud(
    vertical = FALSE,
    boxplot.params = list(color = NA, fill = NA),
    point.params = list(color = "black"),
    borders = F ) +
  theme_classic()+
  scale_fill_manual(values = c("#E69F00", "#009E73","#D55E00","#0072B2")) +
  theme(axis.title.y = element_blank()) +
  sm_forest(
    point_jitter_width = 0.12, 
    sep_level = -3,
    point.params = list(size = 0,color = NA),
    refLine = TRUE
    )

