###该函数用于更改变量名。
###增加一个变量，以标记数据所属的服务器,方便把几个服务器的数据合并到一张表。
###参数‘path’输入表格所在的路径；
###参数table_class用于判断表格类型，"M"表示监控数据“monitor”，"R"表示请求数和相应时间数据“requests_and_response”

table.clean<-function(path,table_class){
   require(xts)
    
   if(table_class=="M"){
        original.data<-read.csv(path,sep=",",header = TRUE)
        names(original.data)<-c("time","traffic.in","traffic.out",
                                "channel.in","channel.out","resp.time",
                                "cpu.usage","io.wait","load.av")
        #从文件名获得服务器的ip
        server.name<-basename(path)
        #增加一列变量以标记数据所来自的服务器
        original.data<-tranform(original.data,server.ip=server.name)
        #将time转化为标准时间
        original.data<-transform(original.data,
                            time=as.POSIXct(time,origin = "1970-01-01"))
        
        #返回数据集
        return(original.data)
        
    }else if(table_class=="R"){
        original.data<-read.csv(path,sep=" ",header = FALSE)
        names(original.data)<-c("server.ip","port.number","hit.time",
                                "miss.time","hit.rate","requests",
                                "T1","T2","T3")
       #增加一个变量，计算综合响应时间
        original.data<-transform(original.data,
                                 resp.time=hit.time*hit.rate+miss.time*(1-hit.rate))
        
        #清除端口port.number和T3同时相同重复的观测（）
        duplicate.index<-duplicated(original.data[,c("port.number","T3")])
        original.data<-original.data[duplicate.index,]
   
        #将T1，T2，T3转化为标准时间
        original.data<-transform(original.data,
                                 T1=as.POSIXct(T1,origin="1970-01-01"),
                                 T2=as.POSIXct(T2,origin="1970-01-01"),
                                 T3=as.POSIXct(T3,origin="1970-01-01"))
        
        #返回数据集
        return(original.data)
  
    }else{
        stop("Please set the parameter 'table_class',
             which shoulb be 'M'(handle resp table) or 'R'(handle cpu talbe)")
    }
}