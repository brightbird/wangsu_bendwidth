library(dplyr)
library(ggplot2)
library(dygraphs)
library(xts)

resp.path<-"data/resp"
cpu.path<-"data/cpu"

source("table_clean.R")

monitor.table<-table_clean("data/monitor/153.36.236.23",table_class = "M")
View(monitor.table)


resp.table<-table_clean("data/requests_and_response/153.36.236.23",table_class = "R")
View(resp.table)

##将各端口的request加总，然后差分
requests.add<-group_by(resp.table,time) %>%
    summarise(requests=sum(requests),
              resp.time=mean(resp.time)
              )
View(requests.add)
#按时间排序


##做差分
diff.data<-tail(requests.add[,-c(1,3)],-1)-head(requests.add[,-c(1,3)],-1)
View(diff.data)
##增加一行，用均值填补
col_mean<-mean(diff.data$requests,na.rm = TRUE)
diff.data<-rbind(diff.data,col_mean)

    #         ##去掉时间，求diff
    #         diff.data<-tail(casted.data[,-1],-1)-head(casted.data[,-1],-1)
    #         ##增加一行，用均值填补
    #         col_mean<-sapply(diff.data,function(x){
    #             mean(x,na.rm = TRUE)}
    #             )
    #         diff.data<-rbind(diff.data,col_mean)
    #         ##增加一列时间
    #         diff.data<-cbind(diff.data,time=casted.data$time)
    
    
##1.合并数据集：按时间进行inner join
merged_153.36.236.23<-merge(monitor.table,resp.table,by="time")
View(merged_153.36.236.23)
str(merged_153.36.236.23)

##在每个时间点求均值。（即得到平均每个端口的水平值）
merged_153.36.236.23<-na.omit(merged_153.36.236.23)
avg_153.36.236.23<-group_by(merged_153.36.236.23,time) %>%
    summarise(requests=mean(requests),
              resp.time=mean(resp.time),
              traffic.out=mean(traffic.out),
              cpu.usage=mean(cpu.usage),
              avg_requests=mean(avg_requests))
View(avg_153.36.236.23)

##进行相关性分析
library(corrplot)
cor<-cor(avg_153.36.236.23[,-1])
corrplot.mixed(cor)


##进行图表展示

dy_avg_153.36.236.23<-as.xts(avg_153.36.236.23[,-1],order.by =avg_153.36.236.23$time)

dygraph(dy_avg_153.36.236.23) %>%
    dySeries("avg_requests", axis = 'y2') %>%
    dyRangeSelector()



