library(car)
library(lmtest)

##1.进行回归分析
bandwidth<-read.csv("wsms/bindwidth/cpuioload/111.161.18.208.csv")
reg_data<-bandwidth[,c(3,7)]
reg<-lm(inouttraffic.out111.161.18.208~cpu.usage111.161.18.208,data = reg_data)
plot(reg)
plot(reg_data$cpu.usage111.161.18.208,reg_data$inouttraffic.out111.161.18.208)
abline(reg,col="blue")
summary(reg)
####相关性非常显著，回归效果初看起来也非常好。

##2.进行DW检验
dw<-dwtest(reg)
dw
####DW检验显示存在自相关。不符合回归分析的基本假设。


##3.对两个变量序列进行单位根检验
urt.inouttraffic<-ur.df(reg_data$inouttraffic.out111.161.18.208,type="none",selectlags = "AIC")
summary(urt.inouttraffic)
####在5%的置信区间下，带宽变量序列存在单位根
###对带宽序列进行一阶差分后再进行单位根检验
d_inouttraffic.out111.161.18.208<-diff(reg_data$inouttraffic.out111.161.18.208)
urt.d_inouttraffic.out111.161.18.208<-ur.df(d_inouttraffic.out111.161.18.208,type = "none",selectlags = "AIC")
summary(urt.d_inouttraffic.out111.161.18.208)
###一阶差分后的序列是平稳序列。



##对cpu序列进行单位根检验
urt.cpu<-ur.df(reg_data$cpu.usage111.161.18.208,type="none",selectlags = "AIC")
summary(urt.cpu)
####cpu序列不存在单位根

##########
####两个序列并不是同阶单整。貌似不适合做协整检验。
#########

##4.EG两步协整检验
###第一步：残差单位根检验
error<-residuals(reg)
urt.resid<-ur.df(error,type="none",selectlags = "AIC")
summary(urt.resid)
###残差平稳，存在协整关系。

###第二步：建立误差修正模型

##5.构造VAR模型，VEC模型，进行格兰杰因果关系检验


##6.对带宽数据建立ARMA模型？











