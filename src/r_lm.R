getwd()

##清理空间，读取数据
rm(list = ls())#清空当前目录内容
mydata <- read.csv("data/xy.csv")#读取数据

##检查数据，包括是否具有线性关系，有无异常值，服从正态分布等
head(mydata) #查看数据
plot(mydata$y ~ mydata$x, data = mydata, main="y ~ x")#散点图检查y与x关系pattern
par(mfrow=c(1, 2)) #绘制boxplot图前的格局设置（1排2列）
boxplot(mydata$x, main="x", sub=paste("Outlier rows: ", 
                    boxplot.stats(mydata$x)$out))#查看离群值,并将离群值坐标副标
boxplot(mydata$y, main="y", sub=paste("Outlier rows: ", 
                                      boxplot.stats(mydata$y)$out))
library(e1071)  # 用于检测正态分布
par(mfrow=c(1, 2))  
plot(density(mydata$x), main="Density Plot: x", ylab="Frequency", 
   sub=paste("Skewness:", round(e1071::skewness(mydata$x), 2)))  # 检查偏倚并绘图
polygon(density(mydata$x), col="red")
plot(density(mydata$y), main="Density Plot: y", ylab="Frequency", 
   sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  
polygon(density(mydata$y), col="red")

##计算相关系数并建模
cor(mydata$x, mydata$y) #计算相关系数
linearMod <- lm(y ~ x, data= mydata)  # 构建线性模型
print(linearMod)

##模型显著性检验，如线性关系是否成立等
summary(linearMod) #模型概要


