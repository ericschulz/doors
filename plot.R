library(ggplot2)
library(plyr)
library(cowplot)
library(ggsignif)
library(ggthemes)
library(RColorBrewer)

dexp1<-read.csv("exp1.csv")
dexp2<-read.csv("exp2.csv")
dexp3<-read.csv("exp3.csv")

#standard errors
se<-function(x){sd(x)/sqrt(length(x))}

#data summary function
data_summary <- function(x) {
  m <- mean(x, na.rm=TRUE)
  sem <-sd(x, na.rm=TRUE)/sqrt(sum(!is.na(x)))
  ymin<-m-1.96*sem
  ymax<-m+1.96*sem
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#position dodge
pd<-position_dodge(0.1)

#my palette
mypalette<-brewer.pal(7,"Dark2")

#standard errors
se<-function(x){sd(x)/sqrt(length(x))}

#data summary function
data_summary <- function(x) {
  m <- mean(x, na.rm=TRUE)
  sem <-sd(x, na.rm=TRUE)/sqrt(sum(!is.na(x)))
  ymin<-m-1.96*sem
  ymax<-m+1.96*sem
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#binomial function
mybinomfunc<-function(x, mark){
  b1<-binom.test(sum(x==mark), length(x), p=1/2, conf.level=0.95)
  dout<-data.frame(state=mark, p=as.numeric(b1$estimate), clow=b1$conf.int[1], cup=b1$conf.int[2])
  return(dout)
}

#position dodge
pd1<-position_dodge(0.1)
pd2<-position_dodge(0.01)

#my palette
mypalette<-brewer.pal(2,"Dark2")

#point data frame
dpoint<-data.frame(Condition=dexp1$condition, doors=dexp1$ndoors)

#mark conditions
dpoint$Condition<-factor(dpoint$Condition, levels=c("1-animal", "8-animals"))

#summarized points
dm<-ddply(dpoint, ~Condition, summarize, m=mean(doors), se=se(doors))

#reshape summary data frame
dsum<-data.frame(m=dm$m,Conditionl=dm$Condition,ymin=dm$m-dm$se, ymax=dm$m+dm$se)

#let's plot it
p1<-ggplot(dpoint, aes(x =Condition, y = doors, fill=Condition, color=Condition))+
  #dot plots
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=4, binwidth=0.5, position=pd1, alpha=0.5) +
  #summary function
  stat_summary(fun.data=data_summary, color="grey30", size=1, alpha=0.75)+
  #minimal theme
  theme_minimal()+ 
  #change text
  theme(text = element_text(size=20,  family="sans"))+
  #colors and fill
  scale_fill_manual(values =mypalette)+
  scale_color_manual(values = mypalette)+
  #write Bayes Factor on top
  geom_signif(comparisons=list(c("1-animal", "8-animals")), annotations="BF=50.8",
              y_position = 50, tip_length = 0, vjust=-0.1, col="black", size=1.1, textsize=6) +
  #label axes
  xlab("Condition")+ylab("Doors opened")+
  #scale of ticks
  scale_y_continuous(breaks=c(0,10,20,30,40,50), limits = c(0,55))+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #title
  ggtitle("Persistence")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1))

p1

#point data frame
dpoint<-data.frame(Condition=dexp1$condition, latency=dexp1$latency)

#mark conditions
dpoint$Condition<-factor(dpoint$Condition, levels=c("1-animal", "8-animals"))

#summarized points
dm<-ddply(dpoint, ~Condition, summarize, m=mean(latency), se=se(latency))

#reshape summary data frame
dsum<-data.frame(m=dm$m,Conditionl=dm$Condition,ymin=dm$m-dm$se, ymax=dm$m+dm$se)

p2<-ggplot(dpoint, aes(x =Condition, y = latency, fill=Condition, color=Condition))+
  #dot plot
  geom_dotplot(binaxis='y', stackdir='center',stackratio=2, dotsize=4, binwidth=2, position=pd2, alpha=0.5) +
  #limits
  ylim(c(0, 220))+
  #summary function
  stat_summary(fun.data=data_summary, color="grey30", size=1, alpha=0.75)+
  #theme and fonts
  theme_minimal()+ theme(text = element_text(size=20,  family="sans"))+
  #colors and fill
  scale_fill_manual(values =mypalette)+
  scale_color_manual(values = mypalette)+
  #add Bayes Factor on top
  geom_signif(comparisons=list(c("1-animal", "8-animals")), annotations="BF=55.1",
              y_position = 200, tip_length = 0, vjust=-0.1, col="black", size=1.1, textsize=6) +
  #label axes
  xlab("Condition")+ylab("Latency in s")+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #title
  ggtitle("First signs of leaving")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1))

p2


#subset of data
dc1<-subset(dexp1, condition=="1-animal")
dc2<-subset(dexp1, condition=="8-animals")

#apply binomial function
dp<-rbind(mybinomfunc(dc1$completion, 1), mybinomfunc(dc2$completion, 1))
#labels state
dp$state<-c("1-animal", "8-animals")

#let's plot it
p3 <- ggplot(dp, aes(y=p, x=as.factor(state), fill=as.factor(state))) +
  #summarize as a bar
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", color='black', width=0.6) + 
  #points
  geom_point()+
  #error bars
  geom_errorbar(aes(ymin=clow, ymax=cup),color='black', width = .33, position=position_dodge((width=0.9))) +
  #label axes
  ylab("Proportion")+ xlab("Condition")+ 
  #theme and font change
  theme_minimal()+ theme(text = element_text(size=20,  family="sans"))+
  #change color
  scale_fill_manual(values = mypalette)+
  #now legend
  theme(strip.background=element_blank(), legend.key=element_rect(color=NA), legend.position="none")+
  guides(color=FALSE, shape=FALSE)+
  #add Bayes Factor above
  geom_signif(comparisons=list(c("1-animal", "8-animals")), annotations="BF=207.9",
              y_position = 0.91, tip_length = 0, vjust=-0.1, col="black", size=1.1, textsize=6) +
  #change y limits
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1.05)) +
  #change title
  ggtitle("Completion")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1))

p3


#point data frame
dpoint<-data.frame(Condition=dexp2$condition, doors=dexp2$ndoors)

#mark conditions
dpoint$Condition<-factor(dpoint$Condition, levels=c("2-animals", "8-animals"))

#summarized points
dm<-ddply(dpoint, ~Condition, summarize, m=mean(doors), se=se(doors))

#reshape summary data frame
dsum<-data.frame(m=dm$m,Conditionl=dm$Condition,ymin=dm$m-dm$se, ymax=dm$m+dm$se)

#let's plot it
p4<-ggplot(dpoint, aes(x =Condition, y = doors, fill=Condition, color=Condition))+
  #dot plots
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=4, binwidth=0.5, position=pd1, alpha=0.5) +
  #summary function
  stat_summary(fun.data=data_summary, color="grey30", size=1, alpha=0.75)+
  #minimal theme
  theme_minimal()+ 
  #change text
  theme(text = element_text(size=20,  family="sans"))+
  #colors and fill
  scale_fill_manual(values =mypalette)+
  scale_color_manual(values = mypalette)+
  #write Bayes Factor on top
  geom_signif(comparisons=list(c("2-animals", "8-animals")), annotations="BF=89.5",
              y_position = 50, tip_length = 0, vjust=-0.1, col="black", size=1.1, textsize=6) +
  #label axes
  xlab("Condition")+ylab("Doors opened")+
  #scale of ticks
  scale_y_continuous(breaks=c(0,10,20,30,40,50), limits = c(0,55))+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #title
  ggtitle("Persistence")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1))

p4

#point data frame
dpoint<-data.frame(Condition=dexp2$condition, latency=dexp2$latency)

#mark conditions
dpoint$Condition<-factor(dpoint$Condition, levels=c("2-animals", "8-animals"))

#summarized points
dm<-ddply(dpoint, ~Condition, summarize, m=mean(latency), se=se(latency))

#reshape summary data frame
dsum<-data.frame(m=dm$m,Conditionl=dm$Condition,ymin=dm$m-dm$se, ymax=dm$m+dm$se)

p5<-ggplot(dpoint, aes(x =Condition, y = latency, fill=Condition, color=Condition))+
  #dot plot
  geom_dotplot(binaxis='y', stackdir='center',stackratio=2, dotsize=4, binwidth=2, position=pd2, alpha=0.5) +
  #limits
  scale_y_continuous(limits = c(0,260), breaks = seq(0, 200, by = 50)) +  #summary function
  stat_summary(fun.data=data_summary, color="grey30", size=1, alpha=0.75)+
  #theme and fonts
  theme_minimal()+ theme(text = element_text(size=20,  family="sans"))+
  #colors and fill
  scale_fill_manual(values =mypalette)+
  scale_color_manual(values = mypalette)+
  #add Bayes Factor on top
  geom_signif(comparisons=list(c("2-animals", "8-animals")), annotations="BF=6.4",
              y_position = 235, tip_length = 0, vjust=-0.1, col="black", size=1.1, textsize=6) +
  #label axes
  xlab("Condition")+ylab("Latency in s")+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #title
  ggtitle("First signs of leaving")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1))

p5

#subset of data
dc1<-subset(dexp2, condition=="2-animals")
dc2<-subset(dexp2, condition=="8-animals")

#apply binomial function
dp<-rbind(mybinomfunc(dc1$completion, 1), mybinomfunc(dc2$completion, 1))
#labels state
dp$state<-c("2-animals", "8-animals")

#let's plot it
p6 <- ggplot(dp, aes(y=p, x=as.factor(state), fill=as.factor(state))) +
  #summarize as a bar
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", color='black', width=0.6) + 
  #points
  geom_point()+
  #error bars
  geom_errorbar(aes(ymin=clow, ymax=cup),color='black', width = .33, position=position_dodge((width=0.9))) +
  #label axes
  ylab("Proportion")+ xlab("Condition")+ 
  #theme and font change
  theme_minimal()+ theme(text = element_text(size=20,  family="sans"))+
  #change color
  scale_fill_manual(values = mypalette)+
  #now legend
  theme(strip.background=element_blank(), legend.key=element_rect(color=NA), legend.position="none")+
  guides(color=FALSE, shape=FALSE)+
  #add Bayes Factor above
  geom_signif(comparisons=list(c("2-animals", "8-animals")), annotations="BF=5.7",
              y_position = 1.01, tip_length = 0, vjust=-0.1, col="black", size=1.1, textsize=6) +
  #change y limits
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1.15), breaks = seq(0, 1, by = 0.25)) +
  #change title
  ggtitle("Completion")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1))

p6

#point data frame
dpoint<-data.frame(Condition=dexp3$condition, doors=dexp3$ndoors)

#mark conditions
dpoint$Condition<-factor(dpoint$Condition, levels=c("1-animal", "8-animals"))

#summarized points
dm<-ddply(dpoint, ~Condition, summarize, m=mean(doors), se=se(doors))

#reshape summary data frame
dsum<-data.frame(m=dm$m,Conditionl=dm$Condition,ymin=dm$m-dm$se, ymax=dm$m+dm$se)

#let's plot it
p7<-ggplot(dpoint, aes(x =Condition, y = doors, fill=Condition, color=Condition))+
  #dot plots
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=4, binwidth=0.5, position=pd1, alpha=0.5) +
  #summary function
  stat_summary(fun.data=data_summary, color="grey30", size=1, alpha=0.75)+
  #minimal theme
  theme_minimal()+ 
  #change text
  theme(text = element_text(size=20,  family="sans"))+
  #colors and fill
  scale_fill_manual(values =mypalette)+
  scale_color_manual(values = mypalette)+
  #write Bayes Factor on top
  geom_signif(comparisons=list(c("1-animal", "8-animals")), annotations="BF=34.9",
              y_position = 45, tip_length = 0, vjust=-0.1, col="black", size=1.1, textsize=6) +
  #label axes
  xlab("Condition")+ylab("Doors opened")+
  #scale of ticks
  scale_y_continuous(breaks=c(0,10,20,30,40,50), limits = c(0,50))+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #title
  ggtitle("Persistence")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1))

p7

point data frame
dpoint<-data.frame(Condition=dexp3$condition, latency=dexp3$latency)

#mark conditions
dpoint$Condition<-factor(dpoint$Condition, levels=c("1-animal", "8-animals"))

#summarized points
dm<-ddply(dpoint, ~Condition, summarize, m=mean(latency), se=se(latency))

#reshape summary data frame
dsum<-data.frame(m=dm$m,Conditionl=dm$Condition,ymin=dm$m-dm$se, ymax=dm$m+dm$se)

p8<-ggplot(dpoint, aes(x =Condition, y = latency, fill=Condition, color=Condition))+
  #dot plot
  geom_dotplot(binaxis='y', stackdir='center',stackratio=2, dotsize=4, binwidth=2, position=pd2, alpha=0.5) +
  #limits
  ylim(c(0, 220))+
  #summary function
  stat_summary(fun.data=data_summary, color="grey30", size=1, alpha=0.75)+
  #theme and fonts
  theme_minimal()+ theme(text = element_text(size=20,  family="sans"))+
  #colors and fill
  scale_fill_manual(values =mypalette)+
  scale_color_manual(values = mypalette)+
  #add Bayes Factor on top
  geom_signif(comparisons=list(c("1-animal", "8-animals")), annotations="BF=13.0",
              y_position = 200, tip_length = 0, vjust=-0.1, col="black", size=1.1, textsize=6) +
  #label axes
  xlab("Condition")+ylab("Latency in s")+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #title
  ggtitle("First signs of leaving")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1))

p8

#subset of data
dc1<-subset(dexp3, condition=="1-animal")
dc2<-subset(dexp3, condition=="8-animals")

#apply binomial function
dp<-rbind(mybinomfunc(dc1$completion, 1), mybinomfunc(dc2$completion, 1))
#labels state
dp$state<-c("1-animal", "8-animals")

#let's plot it
p9 <- ggplot(dp, aes(y=p, x=as.factor(state), fill=as.factor(state))) +
  #summarize as a bar
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", color='black', width=0.6) + 
  #points
  geom_point()+
  #error bars
  geom_errorbar(aes(ymin=clow, ymax=cup),color='black', width = .33, position=position_dodge((width=0.9))) +
  #label axes
  ylab("Proportion")+ xlab("Condition")+ 
  #theme and font change
  theme_minimal()+ theme(text = element_text(size=20,  family="sans"))+
  #change color
  scale_fill_manual(values = mypalette)+
  #now legend
  theme(strip.background=element_blank(), legend.key=element_rect(color=NA), legend.position="none")+
  guides(color=FALSE, shape=FALSE)+
  #add Bayes Factor above
  geom_signif(comparisons=list(c("1-animal", "8-animals")), annotations="BF=124.9",
              y_position = 0.91, tip_length = 0, vjust=-0.1, col="black", size=1.1, textsize=6) +
  #change y limits
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1.05)) +
  #change title
  ggtitle("Completion")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1))
p9

pdf('doors.pdf', width=13, height=10)
plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol=3)
dev.off()
