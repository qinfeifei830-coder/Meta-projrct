# 加载所需包
library(metafor)
library(ggplot2)
library(readxl)



# ---------------------- 1. 拟合元回归模型 ----------------------
res.lin <- rma(yi, vi, mods = ~ Per_session_exercise_duration, data = res)
res.lin
predict(res.lin)

# ---------------------- 2. 生成预测数据 ----------------------
x_var <- res$ Per_session_exercise_duration
x_seq <- seq(min(x_var, na.rm = TRUE), max(x_var, na.rm = TRUE), length.out = 200)
pred <- predict(res.lin, newmods = x_seq)

# 提取预测值
y_pred <- pred$pred
y_ci_lb <- pred$ci.lb
y_ci_ub <- pred$ci.ub
y_pi_lb <- pred$pi.lb
y_pi_ub <- pred$pi.ub
size <-  my_data$int_samplesize
# 构建绘图数据框
plot_data <- data.frame(
  x = x_seq,
  y = y_pred,
  ci_lb = y_ci_lb,
  ci_ub = y_ci_ub,
  pi_lb = y_pi_lb,
  pi_ub = y_pi_ub
)

# ---------------------- 3. 提取模型统计量 ----------------------
intercept <- res.lin$b[1]
slope <- res.lin$b[2]
r2 <- res.lin$R2
p_val <- res.lin$pval[2]

# 公式文本
eq_text <- sprintf(
  "y = %.2f + %.4f x  R² = %.2f  P = %.3f",
  intercept, slope, r2, p_val
)

png(filename = "年龄.png", 
    width = 5000,       
    height = 3000,      
    res = 600,      
    bg = "white"     
)
# ---------------------- 4. ✅ 绘制与示例图完全一致的图 ----------------------
ggplot() +
  # 1. 预测区间（红色虚线）→ 和原图一致
  geom_ribbon(
    data = plot_data,
    aes(x = x, ymin = pi_lb, ymax = pi_ub),
    fill = "#f3f3f3", alpha = 0.15
  ) +
  geom_line(
    data = plot_data,
    aes(x = x, y = pi_lb),
    color = "#E63946", linetype = "dashed", linewidth = 0.7
  ) +
  geom_line(
    data = plot_data,
    aes(x = x, y = pi_ub),
    color = "#E63946", linetype = "dashed", linewidth = 0.7
  ) +
  
  # 2. 95% 置信区间（浅灰色填充）→ 原图风格
  geom_ribbon(
    data = plot_data,
    aes(x = x, ymin = ci_lb, ymax = ci_ub),
    fill = "#D8D8D8", alpha = 0.5
    
  ) +
  
  # 3. 拟合直线（红色实线）→ 原图核心样式
  geom_line(
    data = plot_data,
    aes(x = x, y = y),
    color = "#E63946", linewidth = 1.2
  ) +
  
  # 4. 散点颜色：紫→红→橙渐变（和你图片色条完全一致）
  geom_point(
    data = res,
    aes(x = Per_session_exercise_duration, y = yi, fill = Per_session_exercise_duration,size = size),
    size = 3.2, alpha = 0.9, shape = 21, color = "white", stroke = 0.3
  ) +
  
  # 👇 必须加这个！强制控制点的大小范围，否则ggplot不生效！
  scale_size_continuous(
    range = c(2, 8),  # 最小2，最大8，可自己调，越大差距越明显
    name = "Sample size",  # 图例标题
    guide = guide_legend(order = 2)  # 让样本量图例在颜色图例下面
  ) +
  # 5. 散点配色：深紫 → 红 → 浅橙（原图色号）
  scale_fill_gradientn(
    colors = c("#2D0A50", "#5A1A70", "#C83040", "#F27A4A", "#F9B78A"),
    name = "duration",  # 图例标题
  ) +
  
  # 6. 公式标注
  annotate(
    "text",
    x = min(x_var, na.rm = TRUE),
    y = max(res$yi, na.rm = TRUE),
    label = eq_text, hjust = 0, size = 4.5, fontface = "plain"
  ) +
  
  # 8. 标签 & 主题
  labs(
    x = "Single exercise duration(mins) ",
    y = "Hedges' g",
    #title = "Linear Model"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(linewidth = 0.8, color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.position = "right"
  )
dev.off()