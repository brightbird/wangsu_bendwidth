###该函数用于更改变量名。
###增加一个变量，以标记数据所属的服务器,方便把几个服务器的数据合并到一张表。
###参数‘path’输入表格所在的路径；
###参数table_class用于判断表格类型，"M"表示监控数据“monitor”，"R"表示请求数和相应时间数据“requests_and_response”

table_clean<-function(path,table_class){
   require(xts)
    require(reshape2)
    
   if(table_class=="M"){
        original.data<-read.csv(path,sep=",",header = TRUE,stringsAsFactors = FALSE)
        names(original.data)<-c("time","traffic.in","traffic.out",
                                "channel.in","channel.out","local_resp.time",
                                "cpu.usage","io.wait","load.av","c")
        #从文件名获得服务器的ip
        server.name<-basename(path)
        #增加一列变量以标记数据所来自的服务器
        original.data<-transform(original.data,server.ip=server.name)
        #将time转化为标准时间
        original.data<-transform(original.data,
                            time=as.POSIXct(time/1000,origin = "1970-01-01"))
        
        #返回数据集
        return(original.data)
        
    }else if(table_class=="R"){
        original.data<-read.csv(path,sep="",header = FALSE,stringsAsFactors = FALSE)
        names(original.data)<-c("server.ip","port.number","hit.time",
                                "miss.time","hit.rate","requests",
                                "T1","T2","time")
       #增加一个变量，计算综合响应时间
        original.data<-transform(original.data,
                                 resp.time=hit.time*hit.rate+miss.time*(1-hit.rate))
        
        #清除端口port.number和T3同时相同重复的观测（）
        duplicate.index<-duplicated(original.data[,c("port.number","time")])
        original.data<-original.data[!duplicate.index,]
        
        #将T1，T2，T3转化为标准时间
        original.data<-transform(original.data,
                                 T1=as.POSIXct(T1/1000,origin="1970-01-01"),
                                 T2=as.POSIXct(T2/1000,origin="1970-01-01"),
                                 time=as.POSIXct(time/1000,origin="1970-01-01"))
        
        
        
        
#         #运用差分把原来表示累加值的request转化。
#         test.data<-original.data[,c("time","port.number","requests")]
#         ##先进行dcast（可能产生了NA）
#         casted.data<-dcast(test.data,formula = time~port.number, value.var="requests")
#         ##对NA用中位数填补
#         
#         ##去掉时间，求diff
#         diff.data<-tail(casted.data[,-1],-1)-head(casted.data[,-1],-1)
#         ##增加一行，用均值填补
#         col_mean<-sapply(diff.data,function(x){
#             mean(x,na.rm = TRUE)}
#             )
#         diff.data<-rbind(diff.data,col_mean)
#         ##增加一列时间
#         diff.data<-cbind(diff.data,time=casted.data$time)
#         ##melt
#         melted.data<-melt(diff.data,id.vars = c("time"),
#                           variable.name = "port.number",
#                           value.name = "avg_requests")
#         ##排序，与原数据合并
#         sorted_original.data<-original.data[with(original.data,order(port.number,time)),]
#         #View(sorted_original.data)
#         sorted_melted.data<-melted.data[with(melted.data,order(port.number,time)),]
#         clean.data<-merge(sorted_original.data,sorted_melted.data,by=c("time","port.number"))
#         #View(clean.data)
#        #sorted_data<-original.data[with(original.data,order(port.number,time)),]

        
        #返回数据集
        return(original.data)
  
    }else{
        stop("Please set the parameter 'table_class',
             which shoulb be 'M'(handle resp table) or 'R'(handle cpu talbe)")
    }
}