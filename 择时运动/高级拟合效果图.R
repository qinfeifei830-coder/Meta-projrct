library(tidyverse)
library(ggpmisc)
library(metafor)
library(readxl)

my_data<- read_excel("C:/Users/26069/Desktop/择时运动/血糖数据提取表改-2026.03.19-ZYW.xlsx", 
                     sheet = "急性干预Data")

df_es <- escalc(
  measure = "SMD",
  m1i = int_mean, sd1i = int_sd, n1i = int_samplesize,
  m2i = con_mean, sd2i = con_sd, n2i = con_samplesize,
  data = my_data
)

my_data<- df_es

ggplot(data = my_data, aes(x = Exercise_Intensity, y = yi)) +
  geom_smooth(method = "lm", se=T, 
              color="black", formula = y ~ x) +
  geom_point()
my_data$predict<-predict(lm(yi~Exercise_Intensity, data = my_data))
ggplot(data = my_data, aes(x = Exercise_Intensity, y = yi)) +
  geom_line(aes(x=Exercise_Intensity,y=predict,color=predict),size=2) +
  geom_point()

ggplot(data = my_data, aes(x = Exercise_Intensity, y = yi)) +
geom_smooth(method = "lm", se=T, 
aes(color=..y..), formula = y ~ x) +
geom_point()

ggplot(data = my_data, aes(x = Exercise_Intensity, y = yi)) +
geom_smooth(method = "lm", se=T, aes(color=..y..), formula = y ~ x) +
geom_point()+
scale_color_gradient2(low = "#FF420E",mid="#FFD320",high = "#0084D1",midpoint = 0)

ggplot(data = my_data, aes(x = Exercise_Intensity, y =yi)) +
geom_smooth(method = "lm", se=T,aes(color=..y..), formula = y ~ x) +
geom_point()+
scale_color_gradientn(values = seq(0,1,0.2), colours = c('#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c'))

ggplot(data = my_data, aes(x = Exercise_Intensity, y = yi)) +
geom_point(size=3,alpha=1,pch=21,fill="#9ecae1",color="gray90")+
geom_smooth(method = "lm", se=T, aes(color=..y..), formula = y ~ x) +
scale_color_gradient2(low = "#FF420E",mid="#FFD320",high = "#0084D1",midpoint = 0)+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label..,  ..p.value.label..,sep = "~~~")), parse = TRUE) +
coord_cartesian(ylim = c(-3.2,2.7),xlim = c(4.0,8.5))+
labs(x="exercise intensity",y="Hedges'g")+
scale_y_continuous(breaks = c(-3.2,-2.0,-0.5,0,1.2,2.7))+
scale_x_continuous(breaks = c(4.0,4.3,5.0,5.5,6.5,8.5))+
theme_bw()+
theme(panel.grid = element_blank(),axis.text=element_text(color="black"),legend.title = element_blank())

