library("ggplot2")
data1<-data.frame(var=rnorm(10,mean=0.5,sd=1))
data1$type<-"1"
data2<-data.frame(var=rnorm(10,mean=0.5,sd=0.3))
data2$type<-"2"
data<-rbind(data1,data2)
data$var2<-(data$var-min(data$var))/(max(data$var)-min(data$var) )
ggplot(data,aes(x=var2,fill=type))+geom_dotplot()+ facet_grid(type~.)
