exp_smoother<-function(time, data, unit, alpha){ 
  #time is time column
  #data is data column (can be multiple columns?)
  #unit of time, i.e. "days"
  
  require(zoo)
  require(forecast)
  
  ds<-data.frame(time=time, data=data)
  data.unit<-ddply(ds, .(time), summarise, avg = mean(data))
  
  data.zoo<-zoo(data.unit$avg,data.unit$time)
  fill <- merge(data.zoo, zoo(order.by=seq(start(data.zoo), end(data.zoo), by=unit)))
  fill <- na.spline(fill)
  fill.df <- data.frame(dates=index(fill), weight=coredata(fill))
  
  data.smooth<-HoltWinters(fill, alpha=alpha, beta=FALSE, gamma=FALSE, l.start=data.zoo[start(data.zoo)])
  
  final.smooth<-data.frame(data.smooth$fitted)
  final.smooth$time<-as.Date(as.numeric(time(data.smooth$fitted)))
  final.smooth<-final.smooth[,c("time", "xhat")]
  colnames(final.smooth)<-c("time","data")
  
  return(final.smooth)  
}