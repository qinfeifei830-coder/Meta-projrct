library(grid)
library(forestploter)

# 读取 CSV 文件到 R 中
dt <- read.csv("C:/Users/26069/Desktop/游戏化运动对于青少年/整体亚组（复刻一区）/dt.csv")

# 查看读取的数据
View(dt)
names(dt)

# 如果N列中有数值，则在SHR列前添加空格进行缩进
dt$SHR <- ifelse(is.na(dt$N), 
                 paste0("   ", dt$SHR), 
                 dt$SHR)

# 查看修改后的数据
View(dt)

# 将NA值替换为空字符串
dt$N <- ifelse(is.na(dt$N), "", dt$N)

# 查看替换后的数据
View(dt)

# 计算标准误差se
dt$se <- (log(dt$hi) - log(dt$est))/1.96

# 查看添加标准误差后的数据
View(dt)

# 添加一个空白列用于森林图显示置信区间，并通过空格调整列宽
dt$` ` <- paste(rep(" ", 20), collapse = " ")

# 查看添加空白列后的数据
View(dt)

# 创建置信区间列用于显示
dt$`Unadjusted HR (95% CI)` <- ifelse(is.na(dt$se), "",
                                      sprintf("%.2f (%.2f-%.2f)",
                                              dt$est, dt$low, dt$hi))

# 查看添加置信区间列后的数据
View(dt)
names(dt)

# 写出数据框为 CSV 文件
write.csv(dt, file = "dt.csv")

# 读取 CSV 文件到 R 中
dt <- read.csv("C:/Users/26069/Desktop/游戏化运动对于青少年/整体亚组（复刻一区）/dt2.csv")
View(dt)

# 如果N列中有数值，则在SHR列前添加空格进行缩进
dt$SHR <- ifelse(is.na(dt$N), 
                 paste0("   ", dt$SHR), 
                 dt$SHR)

# 查看修改后的数据
View(dt)

# 将NA值替换为空字符串
dt$N <- ifelse(is.na(dt$N), "", dt$N)

# 查看替换后的数据
View(dt)

# 计算标准误差se
dt$se <- (log(dt$hi) - log(dt$est))/1.96

# 查看添加标准误差后的数据
View(dt)

# 添加一个空白列用于森林图显示置信区间，并通过空格调整列宽
dt$` ` <- paste(rep(" ", 20), collapse = " ")

# 查看添加空白列后的数据
View(dt)

# 定义森林图主题
tm <- forest_theme(base_size = 10,
                   refline_col = "black",
                   arrow_type = "closed",
                   footnote_gp = gpar(col = "blue", cex = 0.6))

# 创建森林图
p <- forest(dt[,c(2:3,11,10,7)],
            est = dt$est,
            lower = dt$low, 
            upper = dt$hi,
            sizes = dt$se,
            ci_column = 3,
            ref_line = 1,
            # arrow_lab = c("Placebo Better", "Treatment Better"),
            xlim = c(0, 5),
            ticks_at = c(0.5, 1, 2, 3,4),
            # footnote = "This is the demo data. Please feel free to change\nanything you want.",
            theme = tm)

# 打印森林图
plot(p)

p <- forest(dt[,c(2:3,11,10,7)],
            est = dt$est,
            lower = dt$low, 
            upper = dt$hi,
            sizes = dt$se+0.2,
            ci_column = 3,
            ref_line = 1,
            # arrow_lab = c("Placebo Better", "Treatment Better"),
            xlim = c(0, 5),
            ticks_at = c(0.5, 1, 2, 3,4),
            # footnote = "This is the demo data. Please feel free to change\nanything you want.",
            theme = tm)
plot(p)


# 将指定行的分组文本加粗
g <- edit_plot(p,
               row = c(1, 4, 8, 12, 17, 20,23,26,29),
               gp = gpar(fontface = "bold"))
# 显示修改后的图
g
## 此时得到Rplot1

#### 添加后半部分
# 读取 CSV 文件到 R 中
dt <- read.csv("dt2.csv")
View(dt)
# 如果N列中有数值，则在SHR列前添加空格进行缩进
dt$SHR <- ifelse(is.na(dt$N), 
                 paste0("   ", dt$SHR), 
                 dt$SHR)

# 查看修改后的数据
View(dt)

# 将NA值替换为空字符串
dt$N <- ifelse(is.na(dt$N), "", dt$N)

# 查看替换后的数据
View(dt)

# 计算标准误差se1
dt$se1 <- (log(dt$hi1) - log(dt$est1))/1.96

# 查看添加标准误差后的数据
View(dt)

# 添加一个空白列用于森林图显示置信区间，并通过空格调整列宽
dt$`Adjusted CI` <- paste(rep(" ", 20), collapse = " ")

# 查看添加空白列后的数据
View(dt)

# 将NA值替换为空字符串
dt$X <- paste(rep(" ", 20), collapse = " ")
View(dt)

# 重命名列名
colnames(dt)[colnames(dt) == "X"] <- "Unadjusted CI"
View(dt)
# 设置主题
tm <- forest_theme(base_size = 10,
                   refline_col = "black",
                   footnote_gp = gpar(col = "blue"),
                   legend_name = "GP",
                   legend_value = c("Trt 1", "Trt 2"))

# 创建森林图对象
p <- forest(dt[,c(1:2,8,9,6,16,13,14)],
            est = list(dt$est,
                       dt$est1),
            lower = list(dt$low,
                         dt$low1), 
            upper = list(dt$hi,
                         dt$hi1),
            ci_column = c(3, 6),
            ref_line = 1,
            # arrow_lab = c("Placebo Better", "Treatment Better"),
            nudge_y = 0.2,
            x_trans = "log",
            theme = tm)

plot(p)
## 此时得到Rplot2

# 将指定行的分组文本加粗
g <- edit_plot(p,
               row = c(1, 6, 11, 16, 21, 26),
               gp = gpar(fontface = "bold"))

g
## 此时得到Rplot3

# 调整坐标轴范围和刻度
p <- forest(dt[,c(1:2,8,9,6,16,13,14)],
            est = list(dt$est,
                       dt$est1),
            lower = list(dt$low,
                         dt$low1), 
            upper = list(dt$hi,
                         dt$hi1),
            ci_column = c(3, 6),
            ref_line = 1,
            xlim = c(0, 5),
            ticks_at = c(0.5,1,2,3,4),
            # arrow_lab = c("Placebo Better", "Treatment Better"),
            nudge_y = 0.2,
            x_trans = "log",
            theme = tm)

plot(p)
## 此时得到Rplot4

# 将指定行的分组文本加粗
g <- edit_plot(p,
               row = c(1, 6, 11, 16, 21, 26),
               gp = gpar(fontface = "bold"))
# 显示修改后的图
g


# 在标题部分的顶部添加下划线
g <- add_border(g, part = "header", row = 1, where = "top")
# 显示修改后的图
g

# 在标题部分的底部添加下划线
g <- add_border(g, part = "header", row = 1, where = "bottom")
g


# 在第31行的指定列添加上边框
g <- add_border(g, row = 31, col = 1:8, where = "top")
# 显示最终的图
plot(g)
