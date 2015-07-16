###该函数用于更改变量名。
###增加一个变量，以标记数据所属的服务器,方便把几个服务器的数据合并到一张表。
###参数‘path’输入表格所在的路径；
###参数table_class用于判断表格类型，"M"表示监控数据“monitor”，"R"表示请求数和相应时间数据“requests_and_response”

table.clean<-function(path,table_class){
  
   if(table_class=="M"){
        original.data<-read.csv(path,sep=",",header = TRUE)
        names(original.data)<-c("time","traffic.in","traffic.out",
                                "channel.in","channel.out","resp.time",
                                "cpu.usage","io.wait","load.av")
        #从文件名获得服务器的ip
        server.name<-basename(path)
        #增加一列变量以标记数据所来自的服务器
        original.data<-tranform(original.data,server.ip=server.name)
        
    }else if(table_class=="R"){
        original.data<-read.csv(path,sep=" ",header = FALSE)
        names(original.data)<-c("server.ip","port.number","hit.time",
                                "miss.time","hit.rate","requests",
                                "T1","T2","T3")
        #从文件名获得服务器的ip
        file.name<-basename(path)
        server.name<-strsplit(file.name, "\\.")[[1]]
        
    }else{
        stop("Please set the parameter 'table_class',
             which shoulb be 'M'(handle resp table) or 'R'(handle cpu talbe)")
    }
}