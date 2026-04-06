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

my_data<- read_excel("C:/Users/26069/Desktop/数据.xlsx")
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


res<-df_no_outliers

#线性回归(int_weeklyvolume)
res.lin <- rma(G, ViG, mods = ~ int_weeklyvolume, data=res )
res.lin
regplot(res.lin, las=1, digits=1, bty="l", 
        xlab="Length of exercise regimen(Wks)", ylab="Hedges'g")
res.cub <- rma(G, ViG,, mods = ~ poly(int_weeklyvolume, degree=3, raw=TRUE), data=res )
res.cub
xs <- seq(-4, 50, length=500)
sav <- predict(res.cub, newmods=unname(poly(xs, degree=3, raw=TRUE)))
regplot(res.cub, mod=2, pred=sav, xvals=xs , xlim=c(0,30),
        ylim=c(-4,4), las=1, digits=1, bty="l", xlab="Predictor", main="Cubic Polynomial Model")


res.rcs3<- rma(G, ViG,mods = ~ rcs(int_weeklyvolume, 3), data=res)
res.rcs3
knots <- attr(rcs(res$int_weeklyvolume, 3), "parms")
knots
sav <- predict(res.rcs3, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs3, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l", xlab="Predictor", main="Restricted Cubic Spline Model")
abline(v=knots, lty="dotted")
points(tmp)
#在视觉临界点处添加阈值线
abline(v=18, lty="dashed", lwd=2, col="black")


ns.knots <- knots[1:3]
ns.Boundary.knots <- knots[c(1,3)]
res.ns <- rma(G, ViG, mods = ~ ns(int_weeklyvolume, knots=ns.knots, Boundary.knots=ns.Boundary.knots), data=res)
res.ns

sav <- predict(res.ns, newmods=unname(ns(xs, knots=ns.knots, Boundary.knots=ns.Boundary.knots)))
tmp <- regplot(res.lin, pred=sav, xvals=xs, las=1, digits=1, bty="l", shade="gray80",
               xlab="Predictor", main="Natural Cubic Spline Model")
abline(v=ns.knots, lty="dotted")
abline(v=ns.Boundary.knots, lty="dotted")
points(tmp)


sm <- smoothCon(s(int_weeklyvolume, bs="tp", k=4), data=res, absorb.cons=TRUE)[[1]]
res.tps <- rma(G, ViG,, mods = ~ sm$X, data=res)
res.tps
sav <- predict(res.tps, newmods=PredictMat(sm, data.frame(int_weeklyvolume=xs)))
regplot(res.tps, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l",
        xlab="Predictor", main="Thin Plate Spline Model")


fitstats(res.lin, res.cub, res.rcs3, res.tps)
cols <- c("black", "dodgerblue", "firebrick", "forestgreen")
regplot(res.lin, las=1, digits=1, bty="l", ci=FALSE, lcol=cols[1], lwd=5,
        xlab="Predictor", main="Model Comparison")
sav <- predict(res.cub, newmods=unname(poly(xs, degree=3, raw=TRUE)))
lines(xs, sav$pred, lwd=5, col=cols[2])
sav <- predict(res.rcs3, newmods=rcspline.eval(xs, knots, inclx=TRUE))
lines(xs, sav$pred, lwd=5, col=cols[3])
sav <- predict(res.tps, newmods=PredictMat(sm, data.frame(xi=xs)))
lines(xs, sav$pred, lwd=5, col=cols[4])
legend("topleft", inset=.02, lty="solid", col=cols, lwd=5,
       legend=c("Linear Model", "Cubic Polynomial", "Restricted Cubic Spline Model", "Thin Plate Spline Model"))


#三样条4节点
res.rcs4 <- rma(G, ViG,mods = ~ rcs(int_weeklyvolume, 4), data=res)
res.rcs4
knots <- attr(rcs(res$int_weeklyvolume, 4), "parms")
knots
sav <- predict(res.rcs4, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs4, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l", xlab="Length of exercise regimen(Wks)", ylab="Hedges'g")
abline(v=knots, lty="dotted")
points(tmp)
#在视觉临界点处添加阈值线
abline(v=16, lty="dashed", lwd=2, col="black")
#三样条5节点
res.rcs5 <- rma(G, ViG,mods = ~ rcs(int_weeklyvolume, 5), data=res)
res.rcs5
knots <- attr(rcs(res$int_weeklyvolume, 5), "parms")
knots
png(filename = "总干涉时长.png", 
    width = 2500,       
    height = 2000,      
    res = 300,      
    bg = "white"     
)
sav <- predict(res.rcs5, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs5, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="o",  xlab="Length of exercise regimen(Wks)", ylab="Hedges'g",cex.lab=1.3)
abline(v=knots, lty="dotted")
points(tmp)
dev.off()
fitstats(res.lin, res.rcs4, res.rcs3, res.rcs5)

#线性回归(Frequency)
res.lin <- rma(G, ViG, mods = ~ Frequency, data=res )
res.lin
regplot(res.lin, las=1, digits=1, bty="l", 
        xlab="Predictor", main="Linear Model")
res.cub <- rma(G, ViG,, mods = ~ poly(Frequency, degree=3, raw=TRUE), data=res )
res.cub
xs <- seq(1, 10, length=500)
sav <- predict(res.cub, newmods=unname(poly(xs, degree=3, raw=TRUE)))
regplot(res.cub, mod=2, pred=sav, xvals=xs , xlim=c(1,10),
        ylim=c(-2,2), las=1, digits=1, bty="l", xlab="Predictor", main="Cubic Polynomial Model")


res.rcs3<- rma(G, ViG,mods = ~ rcs(Frequency, 3), data=res)
res.rcs3
knots <- attr(rcs(res$Frequency, 3), "parms")
knots
png(filename = "每周运动次数.png", 
    width = 2500,       
    height = 2000,      
    res = 300,      
    bg = "white"     
)
sav <- predict(res.rcs3, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs3, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l", xlab="Number of weekly exercise sessions(number of times)",ylab="Hedges'g", cex.lab=1.3)
abline(v=knots, lty="dotted")
points(tmp)
#在视觉临界点处添加阈值线
abline(v=5, lty="dashed", lwd=2, col="black")
dev.off()

ns.knots <- knots[1:3]
ns.Boundary.knots <- knots[c(1,3)]
res.ns <- rma(G, ViG, mods = ~ ns(Frequency, knots=ns.knots, Boundary.knots=ns.Boundary.knots), data=res)
res.ns

sav <- predict(res.ns, newmods=unname(ns(xs, knots=ns.knots, Boundary.knots=ns.Boundary.knots)))
tmp <- regplot(res.lin, pred=sav, xvals=xs, las=1, digits=1, bty="l", shade="gray80",
               xlab="Predictor", main="Natural Cubic Spline Model")
abline(v=ns.knots, lty="dotted")
abline(v=ns.Boundary.knots, lty="dotted")
points(tmp)


sm <- smoothCon(s(Frequency, bs="tp", k=4), data=res, absorb.cons=TRUE)[[1]]
res.tps <- rma(G, ViG,, mods = ~ sm$X, data=res)
res.tps
sav <- predict(res.tps, newmods=PredictMat(sm, data.frame(Frequency=xs)))
regplot(res.tps, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l",
        xlab="Predictor", main="Thin Plate Spline Model")


fitstats(res.lin, res.cub, res.rcs3, res.tps)
cols <- c("black", "dodgerblue", "firebrick", "forestgreen")
regplot(res.lin, las=1, digits=1, bty="l", ci=FALSE, lcol=cols[1], lwd=5,
        xlab="Predictor", main="Model Comparison")
sav <- predict(res.cub, newmods=unname(poly(xs, degree=3, raw=TRUE)))
lines(xs, sav$pred, lwd=5, col=cols[2])
sav <- predict(res.rcs3, newmods=rcspline.eval(xs, knots, inclx=TRUE))
lines(xs, sav$pred, lwd=5, col=cols[3])
sav <- predict(res.tps, newmods=PredictMat(sm, data.frame(xi=xs)))
lines(xs, sav$pred, lwd=5, col=cols[4])
legend("topleft", inset=.02, lty="solid", col=cols, lwd=5,
       legend=c("Linear Model", "Cubic Polynomial", "Restricted Cubic Spline Model", "Thin Plate Spline Model"))


#三样条4节点
res.rcs4 <- rma(G, ViG,mods = ~ rcs(Frequency, 4), data=res)
res.rcs4
knots <- attr(rcs(res$Frequency, 4), "parms")
knots
sav <- predict(res.rcs4, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs4, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l", xlab="Predictor", main="Restricted Cubic Spline Model")
abline(v=knots, lty="dotted")
points(tmp)
#在视觉临界点处添加阈值线
abline(v=16, lty="dashed", lwd=2, col="black")
#三样条5节点
res.rcs5 <- rma(G, ViG,mods = ~ rcs(Frequency, 5), data=res)
res.rcs5
knots <- attr(rcs(res$Frequency, 5), "parms")
knots
sav <- predict(res.rcs5, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs5, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l", xlab="Predictor", main="Restricted Cubic Spline Model")
abline(v=knots, lty="dotted")
points(tmp)

fitstats(res.lin, res.rcs4, res.rcs3, res.rcs5)

#线性回归(age)
res.lin <- rma(G, ViG, mods = age, data=res )
res.lin
png(filename = "年龄.png", 
    width = 2500,       
    height = 2000,      
    res = 300,      
    bg = "white"     
)
regplot(res.lin, las=1, digits=1, bty="o", 
        xlab="Age(year)", ylab="Hedges'g", cex.lab=1.3)
dev.off()
res.cub <- rma(G, ViG, mods = ~ poly(age, degree=3, raw=TRUE), data=res )
res.cub
xs <- seq(50, 100, length=500)
sav <- predict(res.cub, newmods=unname(poly(xs, degree=3, raw=TRUE)))
regplot(res.cub, mod=2, pred=sav, xvals=xs , xlim=c(50,90),
        ylim=c(-4,4), las=1, digits=1, bty="l", xlab="Age(year)", main="Cubic Polynomial Model")


res.rcs3<- rma(G, ViG,mods = ~ rcs(age, 3), data=res)
res.rcs3
knots <- attr(rcs(res$age, 3), "parms")
knots
sav <- predict(res.rcs3, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs3, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l", xlab="Predictor", main="Restricted Cubic Spline Model")
abline(v=knots, lty="dotted")
points(tmp)
#在视觉临界点处添加阈值线
abline(v=77.40, lty="dashed", lwd=2, col="black")


ns.knots <- knots[1:3]
ns.Boundary.knots <- knots[c(1,3)]
res.ns <- rma(G, ViG, mods = ~ ns(age, knots=ns.knots, Boundary.knots=ns.Boundary.knots), data=res)
res.ns

sav <- predict(res.ns, newmods=unname(ns(xs, knots=ns.knots, Boundary.knots=ns.Boundary.knots)))
tmp <- regplot(res.lin, pred=sav, xvals=xs, las=1, digits=1, bty="l", shade="gray80",
               xlab="Predictor", main="Natural Cubic Spline Model")
abline(v=ns.knots, lty="dotted")
abline(v=ns.Boundary.knots, lty="dotted")
points(tmp)


sm <- smoothCon(s(age, bs="tp", k=4), data=res, absorb.cons=TRUE)[[1]]
res.tps <- rma(G, ViG,, mods = ~ sm$X, data=res)
res.tps
sav <- predict(res.tps, newmods=PredictMat(sm, data.frame(age)))
regplot(res.tps, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l",
        xlab="Predictor", main="Thin Plate Spline Model")


fitstats(res.lin, res.cub, res.rcs3, res.tps)
cols <- c("black", "dodgerblue", "firebrick", "forestgreen")
regplot(res.lin, las=1, digits=1, bty="l", ci=FALSE, lcol=cols[1], lwd=5,
        xlab="Predictor", main="Model Comparison")
sav <- predict(res.cub, newmods=unname(poly(xs, degree=3, raw=TRUE)))
lines(xs, sav$pred, lwd=5, col=cols[2])
sav <- predict(res.rcs3, newmods=rcspline.eval(xs, knots, inclx=TRUE))
lines(xs, sav$pred, lwd=5, col=cols[3])
sav <- predict(res.tps, newmods=PredictMat(sm, data.frame(xi=xs)))
lines(xs, sav$pred, lwd=5, col=cols[4])
legend("topleft", inset=.02, lty="solid", col=cols, lwd=5,
       legend=c("Linear Model", "Cubic Polynomial", "Restricted Cubic Spline Model", "Thin Plate Spline Model"))


#三样条4节点
res.rcs4 <- rma(G, ViG,mods = ~ rcs(age, 4), data=res)
res.rcs4
knots <- attr(rcs(res$age, 4), "parms")
knots
sav <- predict(res.rcs4, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs4, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l", xlab="Predictor", main="Restricted Cubic Spline Model")
abline(v=knots, lty="dotted")
points(tmp)
#在视觉临界点处添加阈值线
abline(v=16, lty="dashed", lwd=2, col="black")
#三样条5节点
res.rcs5 <- rma(G, ViG,mods = ~ rcs(age, 5), data=res)
res.rcs5
knots <- attr(rcs(res$age, 5), "parms")
knots
sav <- predict(res.rcs5, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs5, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l", xlab="Predictor", main="Restricted Cubic Spline Model")
abline(v=knots, lty="dotted")
points(tmp)

fitstats(res.lin, res.rcs4, res.rcs3, res.rcs5)

#线性回归(Per_session_exercise_duration)
res.lin <- rma(G, ViG, mods = ~ Per_session_exercise_duration, data=res )
res.lin
regplot(res.lin, las=1, digits=1, bty="l", 
        xlab="Predictor", main="Linear Model")
res.cub <- rma(G, ViG,, mods = ~ poly(Per_session_exercise_duration, degree=3, raw=TRUE), data=res )
res.cub
xs <- seq(0, 150, length=500)
sav <- predict(res.cub, newmods=unname(poly(xs, degree=3, raw=TRUE)))
regplot(res.cub, mod=2, pred=sav, xvals=xs , xlim=c(15,150),
        ylim=c(-4,4), las=1, digits=1, bty="l", xlab="Predictor", main="Cubic Polynomial Model")


res.rcs3<- rma(G, ViG,mods = ~ rcs(Per_session_exercise_duration, 3), data=res)
res.rcs3
knots <- attr(rcs(res$Per_session_exercise_duration, 3), "parms")
knots
sav <- predict(res.rcs3, newmods=rcspline.eval(xs, knots, inclx=TRUE))
png(filename = "每次运动的时间.png", 
    width = 2500,       
    height = 2000,      
    res = 300,      
    bg = "white"     
)
tmp <- regplot(res.rcs3, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="o", xlab="Duration of exercise per session", ylab="Hedges's", cex.lab=1.3)
abline(v=knots, lty="dotted")
points(tmp)
dev.off()
#在视觉临界点处添加阈值线
abline(v=18, lty="dashed", lwd=2, col="black")


ns.knots <- knots[1:3]
ns.Boundary.knots <- knots[c(1,3)]
res.ns <- rma(G, ViG, mods = ~ ns(Per_session_exercise_duration, knots=ns.knots, Boundary.knots=ns.Boundary.knots), data=res)
res.ns

sav <- predict(res.ns, newmods=unname(ns(xs, knots=ns.knots, Boundary.knots=ns.Boundary.knots)))
tmp <- regplot(res.lin, pred=sav, xvals=xs, las=1, digits=1, bty="l", shade="gray80",
               xlab="Predictor", main="Natural Cubic Spline Model")
abline(v=ns.knots, lty="dotted")
abline(v=ns.Boundary.knots, lty="dotted")
points(tmp)


sm <- smoothCon(s(Per_session_exercise_duration, bs="tp", k=4), data=res, absorb.cons=TRUE)[[1]]
res.tps <- rma(G, ViG,, mods = ~ sm$X, data=res)
res.tps
sav <- predict(res.tps, newmods=PredictMat(sm, data.frame(Per_session_exercise_duration=xs)))
regplot(res.tps, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l",
        xlab="Predictor", main="Thin Plate Spline Model")


fitstats(res.lin, res.cub, res.rcs3, res.tps)
cols <- c("black", "dodgerblue", "firebrick", "forestgreen")
regplot(res.lin, las=1, digits=1, bty="l", ci=FALSE, lcol=cols[1], lwd=5,
        xlab="Predictor", main="Model Comparison")
sav <- predict(res.cub, newmods=unname(poly(xs, degree=3, raw=TRUE)))
lines(xs, sav$pred, lwd=5, col=cols[2])
sav <- predict(res.rcs3, newmods=rcspline.eval(xs, knots, inclx=TRUE))
lines(xs, sav$pred, lwd=5, col=cols[3])
sav <- predict(res.tps, newmods=PredictMat(sm, data.frame(xi=xs)))
lines(xs, sav$pred, lwd=5, col=cols[4])
legend("topleft", inset=.02, lty="solid", col=cols, lwd=5,
       legend=c("Linear Model", "Cubic Polynomial", "Restricted Cubic Spline Model", "Thin Plate Spline Model"))


#三样条4节点
res.rcs4 <- rma(G, ViG,mods = ~ rcs(Per_session_exercise_duration, 4), data=res)
res.rcs4
knots <- attr(rcs(res$Per_session_exercise_duration, 4), "parms")
knots
sav <- predict(res.rcs4, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs4, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l", xlab="Predictor", main="Restricted Cubic Spline Model")
abline(v=knots, lty="dotted")
points(tmp)
#在视觉临界点处添加阈值线
abline(v=16, lty="dashed", lwd=2, col="black")
#三样条5节点
res.rcs5 <- rma(G, ViG,mods = ~ rcs(Per_session_exercise_duration, 5), data=res)
res.rcs5
knots <- attr(rcs(res$Per_session_exercise_duration, 5), "parms")
knots
sav <- predict(res.rcs5, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs5, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l", xlab="Predictor", main="Restricted Cubic Spline Model")
abline(v=knots, lty="dotted")
points(tmp)

fitstats(res.lin, res.rcs4, res.rcs3, res.rcs5)

#线性回归(Exercise_time_per_week)
res.lin <- rma(G, ViG, mods = ~ Exercise_time_per_week, data=res )
res.lin
regplot(res.lin, las=1, digits=1, bty="l", 
        xlab="Predictor", main="Linear Model")
res.cub <- rma(G, ViG,, mods = ~ poly(Exercise_time_per_week, degree=3, raw=TRUE), data=res )
res.cub
xs <- seq(0, 650, length=500)
sav <- predict(res.cub, newmods=unname(poly(xs, degree=3, raw=TRUE)))
png(filename = "每周运动时长.png", 
    width = 2500,       
    height = 2000,      
    res = 300,      
    bg = "white"     
)
regplot(res.cub, mod=2, pred=sav, xvals=xs , xlim=c(40,650),
        ylim=c(-4,4), las=1, digits=1, bty="o", xlab="Total weekly exercise duration", ylab="Hedges'g", cex.lab=1.3)
dev.off()

res.rcs3<- rma(G, ViG,mods = ~ rcs(Exercise_time_per_week, 3), data=res)
res.rcs3
knots <- attr(rcs(res$Exercise_time_per_week, 3), "parms")
knots
sav <- predict(res.rcs3, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs3, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l", xlab="Predictor", main="Restricted Cubic Spline Model")
abline(v=knots, lty="dotted")
points(tmp)
#在视觉临界点处添加阈值线
abline(v=18, lty="dashed", lwd=2, col="black")


ns.knots <- knots[1:3]
ns.Boundary.knots <- knots[c(1,3)]
res.ns <- rma(G, ViG, mods = ~ ns(Exercise_time_per_week, knots=ns.knots, Boundary.knots=ns.Boundary.knots), data=res)
res.ns

sav <- predict(res.ns, newmods=unname(ns(xs, knots=ns.knots, Boundary.knots=ns.Boundary.knots)))
tmp <- regplot(res.lin, pred=sav, xvals=xs, las=1, digits=1, bty="l", shade="gray80",
               xlab="Predictor", main="Natural Cubic Spline Model")
abline(v=ns.knots, lty="dotted")
abline(v=ns.Boundary.knots, lty="dotted")
points(tmp)


sm <- smoothCon(s(Exercise_time_per_week, bs="tp", k=4), data=res, absorb.cons=TRUE)[[1]]
res.tps <- rma(G, ViG,, mods = ~ sm$X, data=res)
res.tps
sav <- predict(res.tps, newmods=PredictMat(sm, data.frame(Exercise_time_per_week=xs)))
regplot(res.tps, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l",
        xlab="Predictor", main="Thin Plate Spline Model")


fitstats(res.lin, res.cub, res.rcs3, res.tps)
cols <- c("black", "dodgerblue", "firebrick", "forestgreen")
regplot(res.lin, las=1, digits=1, bty="l", ci=FALSE, lcol=cols[1], lwd=5,
        xlab="Predictor", main="Model Comparison")
sav <- predict(res.cub, newmods=unname(poly(xs, degree=3, raw=TRUE)))
lines(xs, sav$pred, lwd=5, col=cols[2])
sav <- predict(res.rcs3, newmods=rcspline.eval(xs, knots, inclx=TRUE))
lines(xs, sav$pred, lwd=5, col=cols[3])
sav <- predict(res.tps, newmods=PredictMat(sm, data.frame(xi=xs)))
lines(xs, sav$pred, lwd=5, col=cols[4])
legend("topleft", inset=.02, lty="solid", col=cols, lwd=5,
       legend=c("Linear Model", "Cubic Polynomial", "Restricted Cubic Spline Model", "Thin Plate Spline Model"))


#三样条4节点
res.rcs4 <- rma(G, ViG,mods = ~ rcs(Exercise_time_per_week, 4), data=res)
res.rcs4
knots <- attr(rcs(res$Exercise_time_per_week, 4), "parms")
knots
sav <- predict(res.rcs4, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs4, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l", xlab="Predictor", main="Restricted Cubic Spline Model")
abline(v=knots, lty="dotted")
points(tmp)
#在视觉临界点处添加阈值线
abline(v=16, lty="dashed", lwd=2, col="black")
#三样条5节点
res.rcs5 <- rma(G, ViG,mods = ~ rcs(Exercise_time_per_week, 5), data=res)
res.rcs5
knots <- attr(rcs(res$Exercise_time_per_week, 5), "parms")
knots
sav <- predict(res.rcs5, newmods=rcspline.eval(xs, knots, inclx=TRUE))
tmp <- regplot(res.rcs5, mod=2, pred=sav, xvals=xs, las=1, digits=1, bty="l", xlab="Predictor", main="Restricted Cubic Spline Model")
abline(v=knots, lty="dotted")
points(tmp)

fitstats(res.lin, res.rcs4, res.rcs3, res.rcs5)


res[,c("age", "Per_session_exercise_duration", "Frequency","Exercise_time_per_week","int_weeklyvolume")] %>% cor()
#检查高相关性的一种快速方法是计算所有连续变量的互相关矩阵。
multimodel.inference(TE = "G", 
                     seTE = "SeG",
                     data = res,
                     predictors = c("age", "Per_session_exercise_duration", "Frequency","Exercise_time_per_week","int_weeklyvolume"),
                     interaction = TRUE)

multimodel.inference(TE = "G", 
                     seTE = "SeG",
                     data = res,
                     predictors = c("age", "Per_session_exercise_duration", "Frequency","int_weeklyvolume"),
                     interaction = TRUE)

