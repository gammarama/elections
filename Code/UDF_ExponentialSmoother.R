exp_smoother<-function(time, data, unit){ 
  #time is time column
  #data is data column (can be multiple columns?)
  #unit of time, i.e. "days"
  
  require(zoo)
  require(forecast)
  
  ds<-data.frame(time=time, data=data)
  data.unit<-ddply(ds, .(factor(time)), summarise, avg = mean(data))
  
  data.zoo<-zoo(data.unit$avg,data.unit$time)
  fill <- merge(data.zoo, zoo(order.by=seq(start(data.zoo), end(data.zoo), by=unit)))
  fill <- na.spline(x)
  fill.df <- data.frame(dates=index(fill), weight=coredata(fill))
  
  data.smooth<-HoltWinters(fill, alpha=.1, beta=FALSE, gamma=FALSE, l.start=data.unit[1])
  
  data.smooth$fitted
  
}