rm(list=ls())
setwd("~/qcri/QCRI/EyeTracking/results/xml")
source('lib.R')

#Brief description of fields
#qtype: either min or max, depending if the scores are for a translation corresponding to the worst or best translation for a given source sentence
# id: our internal source sentence id (1 to 150) 
# len_type: length group to which the corresponding source sentece belongs (short, mid, long)
# expUserID: Users, odd numbers= monolingual, even numbers=monolingual
# user: Factor with user ID (same as expUserID)
# game_type: Type of Scenario: src, src+tgt, tgt
# user_type:  type of user. Either bilingual =2, or monolingual=1
# duration: total duration in seconds of the task
# game_id: ids for each unique block of 20 tasks performed by users
# slang: source lang id (es)
# tlang: target lang id (en)
#--------
# score: manual evaluation score given by users
# task_num: order of the evalution task within the task block.
# slack: unused time 
# total: effective time
#------
# divref0: time spent in ref (prev)
# divref1: time spent in ref (current)
# divref2: time spent in ref (next)
#... etc
# divref.divref: transitions intra-region
# divref.divsrc: transitions between-regions


# HScore: gold score cmputed as the mix of DiscoTK and human judgments
data<-get_data_trans_human_noties()


#prepare de data
data2_melted<-melt(data2,measure.vars = colnames(data2)[16:22],variable.name="regions",value.name="region_time")


mean(data2$total) #total mean


#1 Total game time by length

tmp<-ddply(data2_melted,.(game_type,len_type,usr_type),summarize,time=mean(total),se=std.err(total))

tmp2<-ddply(data2_melted,.(game_type,usr_type),summarize,avg=mean(total),se=sd(total)/sqrt(length(total)))
time_length_user<-dcast(tmp,(game_type+usr_type~len_type),value.var = c("time"))

#table in paper
xtable(merge(time_length_user,tmp2))

####Images for press
data_img1<-ddply(data2_melted,.(game_type,usr_type),summarize,time=mean(total),se=std.err(total))

#Time per region/user
pdf("~/qcri/QCRI/EyeTracking/WMT2015/presentation/images/results_1.pdf")
ggplot(data_img1,aes(y=time,fill=usr_type,x=game_type,ymin=time-se,ymax=time+se)) + geom_bar(stat="identity", position=position_dodge(width=0.9))+ geom_errorbar(position=position_dodge(width=0.9), width=0.4)+coord_cartesian(ylim=c(15,35))+theme(text=element_text(size = rel(4)),legend.text=element_text(size=rel(4)),legend.title=element_blank(),strip.text.x = element_text(size = rel(4.5)),axis.text=element_text(size=rel(1),color="gray10"))+xlab("Scenario")
dev.off()


#Time by sent type/ user
pdf("~/qcri/QCRI/EyeTracking/WMT2015/presentation/images/results_2.pdf")
data_img2<-ddply(data2_melted,.(len_type,usr_type),summarize,time=mean(total),se=std.err(total))
ggplot(data_img2,aes(y=time,fill=usr_type,x=len_type,ymin=time-se,ymax=time+se)) + geom_bar(stat="identity", position=position_dodge(width=0.9))+ geom_errorbar(position=position_dodge(width=0.9), width=0.4)+coord_cartesian(ylim=c(10,45))+theme(text=element_text(size = rel(4)),legend.text=element_text(size=rel(4)),legend.title=element_blank(),strip.text.x = element_text(size = rel(4.5)),axis.text=element_text(size=rel(1),color="gray10"))+xlab("Sentence length group")
dev.off()

###########
tmpa<-ddply(data2,.(game_type),summarize,time=mean(total))

#Both user types take longer the more information they have to analyze
#users spend less time evaluating a translation when presented with less information

#bilinguals are consistently faster than monolinguals

#2 Proportional time per game mid_length
regions_time<-ddply(data2_melted,.(regions,game_type,usr_type),summarize,prop=mean(region_time/total))

pp_game<-dcast(regions_time,(game_type+usr_type~regions))
pp_game$tot_R<-pp_game$divref0+pp_game$divref1+pp_game$divref2
pp_game$tot_S<-pp_game$divsrc0+pp_game$divsrc1+pp_game$divsrc2
pp_game$not_T<-pp_game$tot_R+pp_game$tot_S

#Table for paper
xtable(pp_game[,c(-8:-3)])

#composing the expected time
tmpb<-ddply(pp_game,.(game_type),summarize,trn=mean(divtrn0))
tmpc<-merge(tmpa,tmpb)
tmpc$exptime<-tmpc$time*tmpc$trn
ddply(data2,.(game_type),summarize,mean(divtrn0))

#model using mixed effects
#we take into account the andom effects of the users
#and take into account the random effects of the game type
# qtype and len_type and usr_type as fixed effects

library(lme4)
# null model without the user effect
usr_type_model.null<-lmer((total) ~ len_type+game_type+ (1|user), data=data2,REML=FALSE)
game_type_model.null<-lmer((total) ~ usr_type * len_type+ (1|user), data=data2,REML=FALSE)
#alternative model with the user effect
full_model.h1<-lmer((total) ~ usr_type * len_type+game_type+ (1|user), data=data2,REML=FALSE)
summary(full_model.h1)
#significance of the model
anova(usr_type_model.null,full_model.h1)
anova(game_type_model.null,full_model.h1)
# It is significant to 90%
coef(usr_type_model.h1)
hist(residuals(usr_type_model.h1))


#Time spent in each scenario
#pp_game
pdf("~/qcri/QCRI/EyeTracking/WMT2015/presentation/images/results_3.pdf",height=5,width=7)
data_img3<-melt(pp_game[,c(1,2,9,10,11)])
ggplot(data_img3,aes(y=value,x=usr_type,fill=variable,group=usr_type))+geom_bar(stat="identity", position=position_fill())+facet_grid(.~game_type)+ylab("proportion of time spent")+xlab("scenarios and type of users")+theme(text=element_text(size = rel(4)),axis.text=element_text(size=rel(1),color="gray10"),legend.text=element_text(size=rel(4)),legend.title=element_blank(),strip.text.x = element_text(size = rel(4.5)) )+ scale_fill_discrete(breaks=c("tot_S","tot_R","divtrn0"),labels=c("src","ref","trans"))
dev.off()


### scores
#### images for press


#Scores
sent_scores<-ddply(data2,.(id,q_type),summarize,mean_norm=mean(norm_score))
sent_scores_group<-ddply(data2,.(id,q_type,usr_type),summarize,mean_norm_type=mean(norm_score))
data4<-merge(data2,sent_scores,by=c("id","q_type"))
data4<-merge(data4,sent_scores_group,by=c("id","q_type","usr_type"))
data4$cost=(data4$norm_score*100-data4$mean_norm*100)**2
data4$cost_group=(data4$norm_score*100-data4$mean_norm_type*100)**2
data4$hum_cost=(data4$norm_score*100-data4$hscore)**2
data4$hum_cost2=(data4$norm_score*100-data4$HScore)**2



#Score consistency
### img4
tab<-ddply(data4,.(game_type,usr_type),summarize,group_var=sqrt(mean(cost_group)),error=sqrt(mean(hum_cost)))
#Paper table
xtable(tab)
pdf("~/qcri/QCRI/EyeTracking/WMT2015/presentation/images/results_4.pdf",height=5,width=7)
ggplot(tab,aes(x=game_type,y=group_var,fill=usr_type))+geom_bar(stat="identity",position=position_dodge())+xlab("Scenario")+ylab("Variance")+coord_cartesian(ylim=c(13,17))+theme(text=element_text(size = rel(4)),legend.text=element_text(size=rel(4)),legend.title=element_blank(),strip.text.x = element_text(size = rel(4.5)),axis.text=element_text(size=rel(1),color="gray10"))
dev.off()



# img: time spent vs. experience
ggplot(data2,aes(x=task_order,y=total))+geom_boxplot()


### NOT USED

data3<-ddply(data4,.(task_num,game_type,usr_type),summarize, cost= sqrt(mean(hum_cost) ),corr=cor(norm_score,hscore))
ggplot(data3,aes(x=task_num,y=cost))+geom_line()+geom_hline(h=0)+facet_grid(game_type~.)+geom_smooth(method="lm",se=F,col="black",lty=2)+theme_bw()+theme(text=element_text(family="Georgia"))+ylab(expression(tau[c]))

tmpd<-ddply(data3,.(game_type,usr_type),summarize,cor(task_num,cost))


xtable(dcast(tmpd,game_type~usr_type))
fonts()


xtable(summary(lm(data=data3,cost~usr_type+task_num+game_type)))

ddply(ddply(subset(data4,task_num > 15),.(task_num,game_type,usr_type),summarize, cost= sqrt(mean(hum_cost) ),corr=cor(norm_score,hscore)),.(game_type,usr_type),summarize,cor(task_num,cost))

data2$task_order<- as.factor(data2$task_num+20*(as.numeric(data2$game_type )-1))

