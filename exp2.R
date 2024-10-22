library(ggplot2)
library(plyr)
library(BayesFactor)
d<-read.csv("/home/hanshalbe/doors/Study_doors_OnlyIncluded.csv")

d<-data.frame(condition=d$data__condition__id, ndoors=d$data__doors__doorsOpened, time=d$time_first_signs_leavingMS, complete=d$over_time, age=d$Age)
head(dat)


library(lsr)
ddply(d, ~data__condition__id, summarize, m=mean(data__doors__doorsOpened))
t.test(subset(d, condition=="8-animals")$ndoors, subset(d, condition!="8-animals")$ndoors, alternative = c("greater"), var.equal = TRUE)
ttestBF(subset(d, condition=="8-animals")$ndoors, subset(d, condition!="8-animals")$ndoors, nullInterval=c(-Inf,0))
cohensD(subset(d, condition=="8-animals")$ndoors, subset(d, condition!="8-animals")$ndoors)

t.test(na.omit(subset(d, condition=="8-animals")$time), na.omit(subset(d, condition!="8-animals")$time), alternative = c("greater"), var.equal = TRUE)
ttestBF(na.omit(subset(d, condition=="8-animals")$time), na.omit(subset(d, condition!="8-animals")$time) , nullInterval=c(-Inf,0))

chisq.test(table(d$complete,d$condition=="8-animals"))
names(d)
contingencyTableBF(table(d$complete,d$condition=="8-animals"), sampleType = "jointMulti", fixedMargin = "rows")

library(MCMCpack)
names(d)
m<-MCMCregress(ndoors~age+c(condition=="8-animals"), data=d)
ms<-summary(m)
summary(m)


dp<-data.frame(Condition=d$condition , doors=d$ndoors)
dp$Condition<-factor(dp$Condition, levels=c("2-animals",  "8-animals"))

se<-function(x){sd(x)/sqrt(length(x))}
dm<-ddply(dp, ~Condition, summarize, m=mean(doors), se=se(doors))
dpoint<-dp
dsum<-data.frame(m=dm$m,Conditionl=dm$Condition,ymin=dm$m-dm$se, ymax=dm$m+dm$se)
library(ggthemes)

#function that outputs mean, lower limit and upper limit of 95% CI
data_summary <- function(x) {
  m <- mean(x, na.rm=TRUE)
  sem <-sd(x, na.rm=TRUE)/sqrt(sum(!is.na(x)))
  ymin<-m-1.95*sem
  ymax<-m+1.95*sem
  return(c(y=m,ymin=ymin,ymax=ymax))
}
library(ggsignif)
library(ggthemes)
pd2<-position_dodge(0.1)

library(wesanderson)

wes_palette("Zissou")
library(RColorBrewer)
mypalette<-brewer.pal(7,"Dark2")
p1<-ggplot(dp, aes(x =Condition, y = doors, fill=Condition, color=Condition))+
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=4, binwidth=0.5, position=pd2, alpha=0.5) +
  stat_summary(fun.data=data_summary, color="grey30", size=1, alpha=0.75)+
  theme_minimal()+ theme(text = element_text(size=20,  family="sans"))+
  #colors and fill
  scale_fill_manual(values =mypalette[c(1,2)])+
  scale_color_manual(values = mypalette[c(1,2)])+
  geom_signif(comparisons=list(c("2-animals",  "8-animals")), annotations="BF=171.2",
              y_position = 50, tip_length = 0, vjust=-0.1, col="black", size=1.1, textsize=6) +
  xlab("Condition")+ylab("Doors opened")+
  scale_y_continuous(breaks=c(0,10,20,30,40,50), limits = c(0,55))+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #labe x-axis
  #scale_x_continuous(breaks = c(1,2), labels = c("Compositional", "Spectral"))+
  ggtitle("(a) Persistence")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1))

p1



dp<-data.frame(Condition=d$condition , duration=d$time/1000)
dp<-na.omit(dp)
dp$Condition<-factor(dp$Condition, levels=c("2-animals",  "8-animals"))

se<-function(x){sd(x)/sqrt(length(x))}
dm<-ddply(dp, ~Condition, summarize, m=mean(duration), se=se(duration))
dpoint<-dp
dsum<-data.frame(m=dm$m,Conditionl=dm$Condition,ymin=dm$m-dm$se, ymax=dm$m+dm$se)
library(ggthemes)

#function that outputs mean, lower limit and upper limit of 95% CI
data_summary <- function(x) {
  m <- mean(x, na.rm=TRUE)
  sem <-sd(x, na.rm=TRUE)/sqrt(sum(!is.na(x)))
  ymin<-m-1.95*sem
  ymax<-m+1.95*sem
  return(c(y=m,ymin=ymin,ymax=ymax))
}
library(ggsignif)
library(ggthemes)
pd2<-position_dodge(0.01)

p2<-ggplot(dp, aes(x =Condition, y = duration, fill=Condition, color=Condition))+
  geom_dotplot(binaxis='y', stackdir='center',stackratio=2, dotsize=5, binwidth=3, position=pd2, alpha=0.5) +
  #ylim(c(0, 220))+
  stat_summary(fun.data=data_summary, color="grey30", size=1, alpha=0.75)+
  theme_minimal()+ theme(text = element_text(size=20,  family="sans"))+
  #colors and fill
  scale_fill_manual(values =mypalette[c(1,2)])+
  scale_color_manual(values = mypalette[c(1,2)])+
  geom_signif(comparisons=list(c("2-animals",  "8-animals")), annotations="BF=3.4",
              y_position = 550, tip_length = 0, vjust=-0.1, col="black", size=1.1, textsize=6) +
  xlab("Condition")+ylab("Latency in s")+ylim(c(0, 600))+
  #no legend
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #labe x-axis
  #scale_x_continuous(breaks = c(1,2), labels = c("Compositional", "Spectral"))+
  ggtitle("(b) First sign of leaving")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1))

p2


mybinomfunc<-function(x, mark){
  b1<-binom.test(sum(x==mark), length(x), p=1/2, conf.level=0.9)
  dout<-data.frame(state=mark, p=as.numeric(b1$estimate), clow=b1$conf.int[1], cup=b1$conf.int[2])
  return(dout)
}

d$playedtoend<-d$complete
dc1<-subset(d, condition=="8-animals")
dc2<-subset(d, condition!="8-animals")

dp<-rbind(mybinomfunc(dc1$playedtoend, 1), mybinomfunc(dc2$playedtoend, 1))
dp$state<-c("8-animals", "2-animals")

p3 <- ggplot(dp, aes(y=p, x=as.factor(state), fill=as.factor(state))) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", color='black', width=0.6) + 
  geom_point()+
  geom_errorbar(aes(ymin=clow, ymax=cup),color='black', width = .33, position=position_dodge((width=0.9))) +
  ylab("Proportion")+#scale_fill_manual(values = c(mcol[c(1,2,3,4)]))+
  xlab("Condition")+ 
  theme_minimal()+ theme(text = element_text(size=20,  family="sans"))+
  scale_fill_manual(values = mypalette[c(1,2)])+
  theme(strip.background=element_blank(), legend.key=element_rect(color=NA), legend.position="none")+
  guides(color=FALSE, shape=FALSE)+
  geom_signif(comparisons=list(c("2-animals",  "8-animals")), annotations="BF=2.3",
              y_position = 0.97, tip_length = 0, vjust=-0.1, col="black", size=1.1, textsize=6) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1.08)) +
  ggtitle("(c): Completion")+
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1))
p3


m<-MCMCregress(ndoors~age+c(condition=="8-animals"), data=d)
ms<-summary(m)
ms<-summary(m)

dd<-data.frame(mu=ms$statistics[2:3,1], se=ms$statistics[2:3,2])

p4<- ggplot(data = data.frame(x = c(0, 18)), aes(x)) + xlab("Posterior estimate") +
  scale_y_continuous(breaks = NULL)+ggtitle("(d) Posterior effects")+
  theme_minimal()+theme(text = element_text(size=20,  family="sans"))+
  ylab("Density")+
  geom_area(stat = "function", fun = dnorm, args = list(mean = dd$mu[1], sd = dd$se[1]), fill = "#FB6467FF", alpha=0.6)+
  geom_area(stat = "function", fun = dnorm, args = list(mean = dd$mu[2], sd = dd$se[2]), fill =  "#526E2DFF", alpha=0.6)+
  geom_point(y = 0.02, x =dd$mu[1]) +
  geom_point(y = 0.01, x =dd$mu[2]) +
  geom_errorbarh(aes(y = 0.02,  xmin = dd$mu[1]-1.96*dd$se[1], xmax = dd$mu[1]+1.96*dd$se[1],height = 0 ))+
  geom_errorbarh(aes(y = 0.01,  xmin = dd$mu[2]-1.96*dd$se[2], xmax = dd$mu[2]+1.96*dd$se[2],height = 0 ))+
  annotate("text", x =dd$mu[1]+1.6, y = 0.31, label = "Age", size=6)+
  annotate("text", x = dd$mu[2]+0.5, y = 0.17, label = "Condition", size=6)+
  theme(legend.position="none", strip.background=element_blank(), legend.key=element_rect(color=NA))+
  #labe x-axis
  #scale_x_continuous(breaks = c(1,2), labels = c("Compositional", "Spectral"))+
  geom_vline(xintercept = 0, linetype=2, size=1.5)+
  scale_y_continuous(expand = c(0,0)) +
  #various theme changes including reducing white space and adding axes
  theme(axis.line.x = element_line(color="grey20", size = 1),
        axis.line.y = element_line(color="grey20", size = 1))

p4

library(gridExtra)

pdf("/home/hanshalbe/doors/behavioralplot2.pdf", width=17.8, height=4)
grid.arrange(p1, p2, p3,p4, nrow=1)
dev.off()
