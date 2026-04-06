# 加载所需包
library(metafor)
library(ggplot2)
library(viridisLite)

# ---------------------- 1. 拟合元回归模型（你的原代码） ----------------------
res.lin <- rma(yi, vi, mods = ~ Per_session_exercise_duration, data = res)

# ---------------------- 2. 生成预测数据（用于画渐变线） ----------------------
# 提取自变量
x_var <- res$Per_session_exercise_duration
# 生成连续x轴序列（200个点，保证渐变丝滑）
x_seq <- seq(min(x_var, na.rm = TRUE), max(x_var, na.rm = TRUE), length.out = 200)
# 用元回归模型预测y值和置信区间
pred <- predict(res.lin, newmods = x_seq, prediction = TRUE)
# 提取预测值、置信上下限
y_pred <- pred$pred
y_ci_lb <- pred$ci.lb
y_ci_ub <- pred$ci.ub

# ---------------------- 3. 构建绘图数据框 ----------------------
plot_data <- data.frame(
  x = x_seq,
  y = y_pred,
  ci_lb = y_ci_lb,
  ci_ub = y_ci_ub,
  pi_lb = y_pi_lb,  # 预测区间下限
  pi_ub = y_pi_ub   # 预测区间上限
)

# ---------------------- 4. 提取模型统计量（用于标注公式） ----------------------
# 提取截距、斜率、R²、P值
intercept <- res.lin$b[1]
slope <- res.lin$b[2]
r2 <- res.lin$R2
p_val <- res.lin$pval[2]  # 斜率的P值

# 格式化公式文本（保留3位小数，和示例图一致）
eq_text <- sprintf(
  "y = %.2f + %.4f x  R² = %.2f  P = %.3f",
  intercept, slope, r2, p_val
)

# ---------------------- 5. 绘制ggplot图（完美复刻示例） ----------------------
ggplot() +
  # 1. 置信区间阴影（灰色半透明，和示例一致）
  geom_ribbon(
    data = plot_data,
    aes(x = x, ymin = ci_lb, ymax = ci_ub),
    fill = "gray",
    alpha = 0.3
  ) +
  # 2. 渐变拟合线（红→黄→灰，和示例色条完全匹配）
  geom_segment(
    data = plot_data[-nrow(plot_data), ],
    aes(
      x = x,
      xend = lead(x),
      y = y,
      yend = lead(y),
      color = y
    ),
    linewidth = 2
  ) +
  # 3. 原始数据点（浅蓝色，和示例一致）
  geom_point(
    data = res,
    aes(x = Per_session_exercise_duration, y = yi),
    color = "#6baed6",  # 浅蓝色
    size = 3,
    alpha = 0.8
  ) +
  # 4. 渐变配色（和示例色条完全一致：红→橙→黄→灰）
  scale_color_gradientn(
    colors = c("#ff4500", "#ff8c00", "#ffd700", "#d3d3d3"),
    limits = range(y_pred),
    name = "Hedges'g"  # 图例标题
  ) +
  # 5. 添加公式标注（左上角，和示例位置一致）
  annotate(
    "text",
    x = min(x_var, na.rm = TRUE),
    y = max(res$yi, na.rm = TRUE) ,
    label = eq_text,
    hjust = 0,
    size = 5,
    fontface = "bold"
  ) +
  # 6. 坐标轴标签（和你原代码一致）
  labs(
    x = "Duration of single exercise session(min)",
    y = "Hedges'g",
    title = "Linear Model"
  ) +
  # 7. 主题美化（和示例风格一致）
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "right",  # 图例在右侧，和示例一致
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  # 8. 40分钟垂直参考线（保留你原代码的需求）
  geom_vline(
    xintercept = 40,
    linetype = "dashed",
    linewidth = 1,
    color = "black"
  )