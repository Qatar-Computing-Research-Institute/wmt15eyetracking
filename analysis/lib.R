library(ggplot2)
library(stringr)
#library(Gmisc)
library(plyr)
library(reshape2)
library(qgraph)

std.err<-function(x)
{
  return(sqrt(var(x)/length(x)))
}

norm<-function(x)
{
  #    return (10*( (x-min(x))/(max(x)-min(x))) +3)
  return (30*( (x)/(sum(x))) +3)  
}
minmax<-function(x)
{
  return ( (x-min(x))/(max(x)-min(x)))
}

norm_minmax<-function(x)
{
  
  min_max<-ddply(x,.(user),summarize,min=min(score), max=max(score))
  
  data2<-merge(x,min_max,by="user")
  data2$norm_score <- (data2$score- data2$min)/(data2$max - data2$min)
  return(data2)
  
}
plot_weighted_transition<-function(x){
  # x<-subset(x,region !="Start")
  x$region<-factor(x$region)
  x$region2<-factor(x$region2)
  
  #    times<-aggregate(x$time,list("region"=x$region),sum)
  aggs<-ddply(x,c("region","region2"),summarize,"time"=sum(norm_score))
  times<-ddply(x,c("region"),summarize,"time"=sum((time+0.001)*norm_score/duration))
  
  transition_mtrx <- acast(aggs,region ~region2,sum)
  groups<-factor(str_split_fixed(levels(aggs$region),"_",2)[,1])
  #print(transition_mtrx)
  #   times$rel<-times$x/sum(times$x)
  transm<-transition_mtrx/sum(transition_mtrx)
  cuts=1/length(aggs$region)
  #transm<- t(apply(transition_mtrx, 1, function(x)(x/(sum(x)))))
  #plot.new()
  qgraph(transm,cut=cuts,esize=3,edge.width=2,asize=1.0,groups=groups,legend = F,vsize=norm(times$time),layout="groups",gray=T,vTrans=150,label.color="gray20",borders = T,directed = TRUE, diag=F)
  #  diag(transm)<-times$rel
  
}
plot_weighted_transition_inv<-function(x){
  # x<-subset(x,region !="Start")
  x$region<-factor(x$region)
  x$region2<-factor(x$region2)
  
  #    times<-aggregate(x$time,list("region"=x$region),sum)
  aggs<-ddply(x,c("region","region2"),summarize,"time"=sum(1-norm_score))
  times<-ddply(x,c("region"),summarize,"time"=sum((time+0.001)*(1-norm_score)/duration))
  
  transition_mtrx <- acast(aggs,region ~region2,sum)
  groups<-factor(str_split_fixed(levels(aggs$region),"_",2)[,1])
  #print(transition_mtrx)
  #   times$rel<-times$x/sum(times$x)
  transm<-transition_mtrx/sum(transition_mtrx)
  cuts=1/length(aggs$region)
  #transm<- t(apply(transition_mtrx, 1, function(x)(x/(sum(x)))))
  #plot.new()
  qgraph(transm,cut=cuts,esize=3,edge.width=2,asize=1.0,groups=groups,legend = F,vsize=norm(times$time),layout="groups",gray=T,vTrans=150,label.color="gray20",borders = T,directed = TRUE, diag=F)
  #  diag(transm)<-times$rel
  
}
plot_transition<-function(x){
  # x<-subset(x,region !="Start")
  x$region<-factor(x$region)
  x$region2<-factor(x$region2)
  
  #    times<-aggregate(x$time,list("region"=x$region),sum)
  aggs<-ddply(x,c("region","region2"),summarize,"time"=length(time))
  times<-ddply(x,c("region"),summarize,"time"=sum((time+0.001)/duration))
  
  transition_mtrx <- acast(aggs,region ~region2,sum)
  groups<-factor(str_split_fixed(levels(aggs$region),"_",2)[,1])
  #print(transition_mtrx)
  #   times$rel<-times$x/sum(times$x)
  transm<-transition_mtrx/sum(transition_mtrx)
  cuts=1/length(aggs$region)
  #transm<- t(apply(transition_mtrx, 1, function(x)(x/(sum(x)))))
  #plot.new()
  qgraph(transm,cut=cuts,esize=3,edge.width=2,asize=1.0,groups=groups,legend = F,vsize=norm(times$time),layout="groups",gray=T,vTrans=150,label.color="gray20",borders = T,directed = TRUE, diag=F)
  #  diag(transm)<-times$rel
  
}

get_data_analysis5<-function()
{
  data1<-read.table("../../data-5+trans+seqs+transit+fixations.dat",h=T)
  #data1<-subset(data0,data0$user!="user3")
  data1$usr_type <-factor(data1$usr_type,labels=c("mono","biling"))
  data1$trial<-paste(data1$game_id,data1$id,sep = "_")
  data1$region<-factor(data1$region,levels=c("divref0","divref1","divref2","divtrn0","divsrc2","divsrc1","divsrc0","start"))
  levels(data1$region)<-c("Ref_prev","Ref","Ref_next","Transl","Src_next","Src","Src_prev","Out")
  data1$region2<-factor(data1$region2,levels=c("divref0","divref1","divref2","divtrn0","divsrc2","divsrc1","divsrc0","end"))
  levels(data1$region2)<-c("Ref_prev","Ref","Ref_next","Transl","Src_next","Src","Src_prev","Out")
  
  return(data1)
}
get_data_analysis4<-function()
{
  data1<-read.table("../../data/data-4+trans+seqs+transit+fixations.dat",h=T)
  #data1<-subset(data0,data0$user!="user3")
  data1$usr_type <-factor(data1$usr_type,labels=c("mono","biling"))
  data1$trial<-paste(data1$game_id,data1$id,sep = "_")
  data1$region<-factor(data1$region,levels=c("divref0","divref1","divref2","divtrn0","divsrc2","divsrc1","divsrc0","start"))
  levels(data1$region)<-c("Ref_prev","Ref","Ref_next","Transl","Src_next","Src","Src_prev","Out")
  data1$region2<-factor(data1$region2,levels=c("divref0","divref1","divref2","divtrn0","divsrc2","divsrc1","divsrc0","end"))
  levels(data1$region2)<-c("Ref_prev","Ref","Ref_next","Transl","Src_next","Src","Src_prev","Out")
  
  return(data1)
}

get_data_trans_4<-function()
{
  data1<-read.table("../../data/data-4.1+trans.dat",h=T,row.names=NULL)
  data2<-read.table("../../data/wmt12.spanish_quality",h=T)
  data1$usr_type <-factor(data1$usr_type,labels=c("mono","biling"))
  data1$delta_s<-data1$divtrn0-data1$divsrc1
  data1$delta_r<-data1$divtrn0-data1$divref1
  colnames(data2)[3]<-"hscore"
  data1<-merge(data1,data2,by.x=c("q_type","id"),by.y=c("type","segmentID"))
  data1$id<-factor(data1$id)
  data1<-subset(data1,user!="user40")
  return(data1)
}

get_data_trans_human_noties<-function()
{
  data1<-read.table("../../data/data-4.1+trans.dat",h=T,row.names=NULL)
  data2<-read.table("../../data/wmt12.spanish_quality",h=T)
  data3<-read.table("../../data/wmt12.RNK_results.filtered.human.noties",h=T)
  data1$usr_type <-factor(data1$usr_type,labels=c("mono","biling"))
  data1$delta_s<-data1$divtrn0-data1$divsrc1
  data1$delta_r<-data1$divtrn0-data1$divref1
  data2<-merge(data2,data3,by.x=c("sysID", "segmentID"),by.y=c("sysID", "segmentID"))
  colnames(data2)[4]<-"hscore"
  data1<-merge(data1,data2,by.x=c("q_type","id"),by.y=c("type","segmentID"))
  data1$id<-factor(data1$id)
  data1<-subset(data1,user!="user40")
  return(data1)
}