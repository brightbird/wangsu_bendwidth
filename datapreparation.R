library(xts)

bandwidth<-read.csv("wsms/bindwidth/cpuioload/111.161.18.208.csv")
starttime<-bandwidth$time[1]
endtime<-nrow(bandwidth)*60000+starttime-60000
bandwidth<-transform(bandwidth,obs=seq(from=starttime,to=endtime,by=60000))
#width<-transform(bandwidth)
bandwidth<-as.xts(bandwidth[,-c(1,11)],order.by =as.POSIXct(bandwidth$obs/1000,origin = "1970-01-01"))

str(bandwidth)
View(head(bandwidth))

save(bandwidth,file = "data/bandwidth.rda")
#load("data/bandwidth.rda")

#dygraph
library(dygraphs)

load("data/bandwidth.rda")

dygraph(bandwidth)

##1）对“inouttraffic.out111.161.18.208”进行日差分，消除日内变动趋势规律。
###但是这个分析未必有意义，因为差分后的序列含义不明；另外，带宽是一个连续的过程，
###进行隔日的差分意义不如一阶差分意义明确。

diff_outtraffic<-diff(bandwidth$inouttraffic.out111.161.18.208-lag(bandwidth$inouttraffic.out111.161.18.208,k=24*60))
View(diff_outtraffic)
diff_outtraffic[is.na(diff_outtraffic)]<-0

dygraph(diff_outtraffic)
##show inouttraffic.out111.161.18.208 and diff_outtraffic on the same plot.

diff_outtraffic<-cbind(diff_outtraffic,bandwidth$inouttraffic.out111.161.18.208)
names(diff_outtraffic)<-c("diff_outtraffic","outtraffic")

View(diff_outtraffic)

dygraph(diff_outtraffic) %>%
    dyRangeSelector() 


##2）test with bcp package
library(bcp)
pp_value<-sapply(diff_outtraffic, function(x){
    bcp.value<-bcp(x,w0=0.2,p0=0.2,burnin=50,mcmc=500,return.mcmc = FALSE)
    bcp.value$posterior.prob
})

pp_value<-as.xts(pp_value,order.by =as.POSIXct(bandwidth$obs/1000,origin = "1970-01-01"))
names(pp_value)<-c("diff_outtraffic.pp","outtraffic.pp")
View(pp_value)


diff_outtraffic_pp<-cbind(diff_outtraffic$diff_outtraffic,pp_value$diff_outtraffic.pp)

dygraph(diff_outtraffic,main = "diff_outtraffic",group = "pp_cal")
dygraph(pp_value,main = "pp_value",group = "pp_cal")

dygraph(diff_outtraffic_pp) %>%
    dySeries("diff_outtraffic.pp", axis = 'y2') %>%
    dyRangeSelector()



##3）探究各个变量的相关性
library(corrplot)
M<-cor(bandwidth)
corrplot.mixed(M)

###结果显示，cpu利用率与带宽存在很强的正相关关系，0.98。
###但是这是正常机器得出的结果，需要在异常情况出现的数据上验证。

##4）探究存在宕机情况的数据的规律
bandwidth.anormal<-read.csv("anormal_data20150715/resp/153.36.236.23")
View(bandwidth.anormal)

##4.1)异常数据相关性探讨
library(corrplot)
cor_anomal<-cor(bandwidth.anormal)
corrplot.mixed(cor_anomal)
###从结果可知，cpu利用率与带宽依然存在很强的正相关关系，0.93.

##4.2）dygraph观察数据

starttime<-bandwidth.anormal$time[1]
endtime<-nrow(bandwidth.anormal)*60000+starttime-60000
bandwidth.anormal<-transform(bandwidth.anormal,obs=seq(from=starttime,to=endtime,by=60000))
bandwidth.anormal<-as.xts(bandwidth.anormal[,-1],order.by =as.POSIXct(bandwidth.anormal$obs/1000,origin = "1970-01-01"))

View(head(bandwidth.anormal))
dygraph(bandwidth.anormal[,c("inouttraffic.out153.36.236.23","inouttraffic.in153.36.236.23","resptime.resp153.36.236.23" )]) %>%
    dySeries("resptime.resp153.36.236.23",axis="y2") %>%
    dyRangeSelector()

save(bandwidth.anormal,file = "data/bandwidth.anormal.rda")
#######################
####---观察另一文件下的响应时间（真实响应时间）的规律，探究异常情况-----####
resp.anormal<-read.csv("anormal_data20150715/cpu/153.36.236.23/153.36.236.23.txt",sep = " ",header = FALSE)
View(resp.anormal)


