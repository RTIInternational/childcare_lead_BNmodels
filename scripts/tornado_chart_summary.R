#Generate tornado charts for select variables across all models

#Author: Riley E. Mulhern, PhD <rmulhern@rti.org>

#Date: February 16, 2023

rm(list=ls())

#### Load libraries and set WD -------------------------------------------------
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggpubr)

cbPalette <- c("#56B4E9", "#CC79A7","#999999", "#E69F00",  "#009E73", "#F0E442", "#0072B2", "#D55E00")
cbPalette2 <- c("#56B4E9","#009E73","#CC79A7","#999999", "#E69F00",   "#F0E442", "#0072B2", "#D55E00")

#Get working directory
getwd() #This is where the outputs from this script will be saved. To set a different working directory use setwd().
        #The working directory must be set to the file path where the main BN model script outputs were saved.

#Load plot data
maxabove1<-read.csv("cpt.plotdata_x2_maxabove1.csv")
maxabove5<-read.csv("cpt.plotdata_x2_maxabove5.csv")
maxabove10<-read.csv("cpt.plotdata_x2_maxabove10.csv")
maxabove15<-read.csv("cpt.plotdata_x2_maxabove15.csv")

perc90above1<-read.csv("cpt.plotdata_x2_perc90above1.csv")
perc90above5<-read.csv("cpt.plotdata_x2_perc90above5.csv")
perc90above10<-read.csv("cpt.plotdata_x2_perc90above10.csv")
perc90above15<-read.csv("cpt.plotdata_x2_perc90above15.csv")

maxabove1<-cbind(rep("Max>1",length(maxabove1$X)),maxabove1)
maxabove5<-cbind(rep("Max>5",length(maxabove5$X)),maxabove5)
maxabove10<-cbind(rep("Max>10",length(maxabove10$X)),maxabove10)
maxabove15<-cbind(rep("Max>15",length(maxabove15$X)),maxabove15)

perc90above1<-cbind(rep("P90>1",length(perc90above1$X)),perc90above1)
perc90above5<-cbind(rep("P90>5",length(perc90above5$X)),perc90above5)
perc90above10<-cbind(rep("P90>10",length(perc90above10$X)),perc90above10)
perc90above15<-cbind(rep("P90>15",length(perc90above15$X)),perc90above15)

#Load label data
maxabove1_lab<-read.csv("tornado.labeldata_x2_maxabove1.csv")
maxabove5_lab<-read.csv("tornado.labeldata_x2_maxabove5.csv")
maxabove10_lab<-read.csv("tornado.labeldata_x2_maxabove10.csv")
maxabove15_lab<-read.csv("tornado.labeldata_x2_maxabove15.csv")

perc90above1_lab<-read.csv("tornado.labeldata_x2_perc90above1.csv")
perc90above5_lab<-read.csv("tornado.labeldata_x2_perc90above5.csv")
perc90above10_lab<-read.csv("tornado.labeldata_x2_perc90above10.csv")
perc90above15_lab<-read.csv("tornado.labeldata_x2_perc90above15.csv")

#List plot data
list<-list(maxabove1,maxabove5,maxabove10,maxabove15,
           perc90above1,perc90above5,perc90above10,perc90above15)
colnames<-c("model_name","index","variable","probability","level")
list_rename<-lapply(list,setNames,colnames)
all<-do.call(rbind,list_rename)

performance_summary<-read.csv("performance_summary_all.csv")

#Prepare data for processing
all.plotdata<-all%>%
  filter(variable!="target")%>%
  group_by(model_name,variable)%>%
  summarise(min=min(probability),
            max=max(probability),
            range=max-min)

##HEAD START
headstart<-all.plotdata%>%
  filter(variable=="head_start")%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)%>%
  ggplot(aes(y=reorder(model_name,range)))+
  geom_linerange(aes(xmin=min,xmax=max),
                 color="black",
                 size=.9)+
  scale_x_continuous(limits=c(0,1),expand=c(0,0))+
    geom_point(aes(x=min,
                 fill="Not Head Start",
                 shape="Not Head Start"),
             color="black",
             size=3)+
  geom_point(aes(x=max,
                 fill="Head Start",
                 shape="Head Start"),
             color="black",
             size=3)+
  geom_point(aes(x=prior,
                 fill="Prior",
                 shape="Prior"),
             color="black",
             size=3,
             alpha=0.7)+
  labs(title="Head Start status",x="",y="Model",fill="",shape="")+
  geom_text_repel(aes(x=max,label=round(max,2)),size=3,nudge_y=0.1)+
  geom_text_repel(aes(x=min,label=round(min,2)),size=3,nudge_y=-0.1)+
  theme_bw()+
  theme(legend.position=c(0.8,0.15),
        legend.background = element_rect(fill="white",color="black"),
        legend.title=element_blank(),
        axis.title=element_text(face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_shape_manual(values=c(21,21,23),breaks=c("Not Head Start","Head Start","Prior"))+
  scale_fill_manual(values=cbPalette,breaks=c("Not Head Start","Head Start","Prior"))
headstart

headstart.calc<-all.plotdata%>%
  filter(variable=="head_start")%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)%>%
  mutate(perc_inc_risk=(max-prior)/prior)%>%
  summarise(avg_inc_risk=mean(perc_inc_risk),
            max_inc_risk=max(perc_inc_risk))
headstart.calc

##FREE REDUCED LUNCH
freeredlunch.data<-all.plotdata%>%
  filter(variable=="PER_FREE")%>%
  mutate(min_state=all[which(all$probability==min),],
         max_state=all[which(all$probability==max),])%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)

levels_freeredlunch<-data.frame(model_name=freeredlunch.data$model_name,
                            max_level=c(">96%",">92%",">96%",">93%",">93%",">95%",">90%",">95%"),
                            min_level=c("50-96%","37-92%","37-96%","28-93%","<36%","32-95%","34-90%","<49%"))

freeredlunch.data<-freeredlunch.data%>%
  merge(y=levels_freeredlunch,by="model_name",all.x=TRUE)

freeredlunch<-freeredlunch.data%>%
  ggplot(aes(y=reorder(model_name,range)))+
  geom_linerange(aes(xmin=min,xmax=max),
                 color="black",
                 size=.9)+
  scale_x_continuous(limits=c(0,1),expand=c(0,0))+
  geom_point(aes(x=min,
                 fill="Lowest risk",
                 shape="Lowest risk"),
             color="black",
             size=3)+
  geom_point(aes(x=max,
                 fill="Highest risk",
                 shape="Highest risk"),
             color="black",
             size=3)+
  geom_point(aes(x=prior,
                 fill="Prior",
                 shape="Prior"),
             color="black",
             size=3,
             alpha=0.7)+
  labs(title="Free/reduced lunch",x="",y="Model",fill="",shape="")+
  geom_text_repel(aes(x=max,label=max_level),
                  size=3,nudge_y=-0.1,nudge_x=0.05)+
  geom_text_repel(aes(x=max,label=round(max,2)),
                  size=3,nudge_y=0.1)+
  geom_text_repel(aes(x=min,label=round(min,2)),size=3,nudge_y=0.1)+
  geom_text_repel(aes(x=min,label=min_level),
                  size=3,nudge_y=-0.1)+
  theme_bw()+
  theme(legend.position=c(0.8,0.15),
        legend.background = element_rect(fill="white",color="black"),
        legend.title=element_blank(),
        axis.title=element_text(face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_shape_manual(values=c(21,21,23),breaks=c("Lowest risk","Highest risk","Prior"))+
  scale_fill_manual(values=cbPalette,breaks=c("Lowest risk","Highest risk","Prior"))
freeredlunch

freeredlunch.calc<-freeredlunch.data%>%
  mutate(perc_inc_risk=(max-prior)/prior)%>%
  summarise(avg_inc_risk=mean(perc_inc_risk),
            max_inc_risk=max(perc_inc_risk))
freeredlunch.calc

##FIXTURE CHANGE
fix_change.data<-all%>%
  filter(variable=="Y_N_FIXTURE_CHG")%>%
  select(!"index")%>%
  spread(key=level,value=probability)%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)%>%
  mutate(range=dk-yes)

fix_change<-fix_change.data%>%
  mutate(xmin=ifelse(yes>no,no,yes))%>%
  ggplot(aes(y=reorder(model_name,range)))+
  geom_linerange(aes(xmin=xmin,xmax=dk),
                 color="black",
                 size=.9)+
  scale_x_continuous(limits=c(0,1),expand=c(0,0))+
  geom_point(aes(x=yes,
                 fill="Past fixture change - Yes",
                 shape="Past fixture change - Yes"),
             color="black",
             size=3)+
  geom_point(aes(x=dk,
                 fill="Past fixture change - Don't know",
                 shape="Past fixture change - Don't know"),
             color="black",
             size=3)+
  geom_point(aes(x=no,
                 fill="Past fixture change - No",
                 shape="Past fixture change - No"),
             color="black",
             size=3)+
  geom_point(aes(x=prior,
                 fill="Prior",
                 shape="Prior"),
             color="black",
             size=3,
             alpha=0.7)+
  labs(x="Probability of exceeding the model target",y="Model",fill="",shape="")+
  geom_text_repel(aes(x=dk,label=round(dk,2)),size=3,nudge_y=0.1)+
  geom_text_repel(aes(x=xmin,label=round(yes,2)),size=3,nudge_y=-0.1)+
  theme_bw()+
  theme(legend.position="right",
        axis.title=element_text(face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_shape_manual(values=c(21,21,21,23),breaks=c("Past fixture change - Yes","Past fixture change - No","Past fixture change - Don't know","Prior"))+
  scale_fill_manual(values=cbPalette2,breaks=c("Past fixture change - Yes","Past fixture change - No","Past fixture change - Don't know","Prior"))
fix_change

ggsave("tornado_summary_fixture_change.png",plot=fix_change,height=4,width=7,units="in",dpi=600)

fixture.change.calc<-fix_change.data%>%
  mutate(perc_inc_risk=(dk-prior)/prior)%>%
  summarise(avg_inc_risk=mean(perc_inc_risk),
            max_inc_risk=max(perc_inc_risk))
fixture.change.calc

##WATER SAMPLES
nsamples.data<-all.plotdata%>%
  filter(variable=="nsamples")%>%
  mutate(min_state=all[which(all$probability==min),],
         max_state=all[which(all$probability==max),])%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)

levels_nsamples<-data.frame(model_name=nsamples.data$model_name,
                            max_level=c(">=22 samples",">=39 samples",">=33 samples",">=37 samples",">=33 samples",">=31 samples",">=31 samples",">=4 samples"),
                            min_level=c("<4 samples","<6 samples","<7 samples","<5 samples","<5","<4","<4","<4"))

nsamples.data<-nsamples.data%>%
  merge(y=levels_nsamples,by="model_name",all.x=TRUE)

nsamples<-nsamples.data%>%
  ggplot(aes(y=reorder(model_name,range)))+
  geom_linerange(aes(xmin=min,xmax=max),
                 color="black",
                 size=.9)+
  scale_x_continuous(limits=c(0,1),expand=c(0,0))+
  geom_point(aes(x=min,
                 fill="Fewer samples (smaller facility)",
                 shape="Fewer samples (smaller facility)"),
             color="black",
             size=3)+
  geom_point(aes(x=max,
                 fill="More samples (larger facility)",
                 shape="More samples (larger facility)"),
             color="black",
             size=3)+
  geom_point(aes(x=prior,
                 fill="Prior",
                 shape="Prior"),
             color="black",
             size=3,
             alpha=0.7)+
  labs(title="Number of water samples",x="",y="Model",fill="",shape="")+
  geom_text_repel(aes(x=max,label=max_level),
                      size=3,nudge_y=-0.1,nudge_x=0.05)+
  geom_text_repel(aes(x=max,label=round(max,2)),
                  size=3,nudge_y=0.1)+
  geom_text_repel(aes(x=min,label=round(min,2)),size=3,nudge_y=0.1)+
  geom_text_repel(aes(x=min,label=min_level),
                  size=3,nudge_y=-0.1)+
  theme_bw()+
  theme(legend.position=c(0.71,0.15),
        legend.background = element_rect(fill="white",color="black"),
        legend.title=element_blank(),
        axis.title=element_text(face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_shape_manual(values=c(21,21,23),breaks=c("Fewer samples (smaller facility)","More samples (larger facility)","Prior"))+
  scale_fill_manual(values=cbPalette,breaks=c("Fewer samples (smaller facility)","More samples (larger facility)","Prior"))
nsamples

nsamples.calc<-nsamples.data%>%
  filter(model_name=="P90>15")%>%
  mutate(perc_inc_risk=(max-prior)/prior)%>%
  summarise(avg_inc_risk=mean(perc_inc_risk),
            max_inc_risk=max(perc_inc_risk))
nsamples.calc

##HOME BASED
home<-all.plotdata%>%
  filter(variable=="home_based")%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)%>%
  ggplot(aes(y=reorder(model_name,range)))+
  geom_linerange(aes(xmin=min,xmax=max),
                 color="black",
                 size=.9)+
  scale_x_continuous(limits=c(0,1),expand=c(0,0))+
  geom_point(aes(x=min,
                 fill="Home-based",
                 shape="Home-based"),
             color="black",
             size=3)+
  geom_point(aes(x=max,
                 fill="Not home-based",
                 shape="Not home-based"),
             color="black",
             size=3)+
  geom_point(aes(x=prior,
                 fill="Prior",
                 shape="Prior"),
             color="black",
             size=3,
             alpha=0.7)+
  labs(x="Probability of exceeding the model target",y="Model",fill="",shape="")+
  geom_text_repel(aes(x=max,label=round(max,2)),size=3,nudge_y=0.1)+
  geom_text_repel(aes(x=min,label=round(min,2)),size=3,nudge_y=-0.1)+
  theme_bw()+
  theme(legend.position="top",
        axis.title=element_text(face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_shape_manual(values=c(21,21,23),breaks=c("Home-based","Not home-based","Prior"))+
  scale_fill_manual(values=cbPalette,breaks=c("Home-based","Not home-based","Prior"))
home

home.calc<-all.plotdata%>%
  filter(variable=="home_based")%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)%>%
  mutate(perc_dec_risk=(min-prior)/prior)%>%
  summarise(avg_dec_risk=mean(perc_dec_risk),
            max_dec_risk=max(perc_dec_risk))
home.calc


## SCHOOL
school<-all.plotdata%>%
  filter(variable=="school")%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)%>%
  ggplot(aes(y=reorder(model_name,range)))+
  geom_linerange(aes(xmin=min,xmax=max),
                 color="black",
                 size=.9)+
  scale_x_continuous(limits=c(0,1),expand=c(0,0))+
  geom_point(aes(x=min,
                 fill="Not school-based",
                 shape="Not school-based"),
             color="black",
             size=3)+
  geom_point(aes(x=max,
                 fill="School-based",
                 shape="School-based"),
             color="black",
             size=3)+
  geom_point(aes(x=prior,
                 fill="Prior",
                 shape="Prior"),
             color="black",
             size=3,
             alpha=0.7)+
  labs(x="Probability of exceeding the model target",y="Model",fill="",shape="")+
  geom_text_repel(aes(x=max,label=round(max,2)),size=3,nudge_y=0.1)+
  geom_text_repel(aes(x=min,label=round(min,2)),size=3,nudge_y=-0.1)+
  theme_bw()+
  theme(legend.position="top",
        axis.title=element_text(face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_shape_manual(values=c(21,21,23),breaks=c("Not school-based","School-based","Prior"))+
  scale_fill_manual(values=cbPalette,breaks=c("Not school-based","School-based","Prior"))
school

school.calc<-all.plotdata%>%
  filter(variable=="school")%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)%>%
  mutate(perc_inc_risk=(max-prior)/prior)%>%
  summarise(avg_inc_risk=mean(perc_inc_risk),
            max_inc_risk=max(perc_inc_risk))
school.calc

ggarrange(home,school,
          ncol=2,nrow=1,
          align="hv",
          labels=c("A","B"))

ggsave("tornado_summary_home_school.png",plot=last_plot(),height=4.5,width=9,units="in",dpi=600)

### WATER TYPE
wtype<-all.plotdata%>%
  filter(variable=="type_binary")%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)%>%
  ggplot(aes(y=reorder(model_name,range)))+
  geom_linerange(aes(xmin=min,xmax=max),
                 color="black",
                 size=.9)+
  scale_x_continuous(limits=c(0,1),expand=c(0,0))+
  geom_point(aes(x=min,
                 fill="Surface water",
                 shape="Surface water"),
             color="black",
             size=3)+
  geom_point(aes(x=max,
                 fill="Groundwater",
                 shape="Groundwater"),
             color="black",
             size=3)+
  geom_point(aes(x=prior,
                 fill="Prior",
                 shape="Prior"),
             color="black",
             size=3,
             alpha=0.7)+
  labs(title="Source water type",x="Posterior probability of exceeding the model target",y="Model",fill="",shape="")+
  geom_text_repel(aes(x=max,label=round(max,2)),size=3,nudge_y=0.1)+
  geom_text_repel(aes(x=min,label=round(min,2)),size=3,nudge_y=-0.1)+
  theme_bw()+
  theme(legend.position=c(0.8,0.15),
        legend.background = element_rect(fill="white",color="black"),
        legend.title=element_blank(),
        axis.title=element_text(face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_shape_manual(values=c(21,21,23),breaks=c("Surface water","Groundwater","Prior"))+
  scale_fill_manual(values=cbPalette,breaks=c("Surface water","Groundwater","Prior"))
wtype

wtype.calc<-all.plotdata%>%
  filter(variable=="type_binary")%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)%>%
  mutate(perc_inc_risk=(max-prior)/prior,
         perc_dec_risk=(min-prior)/prior)%>%
  summarise(avg_inc_risk=mean(perc_inc_risk),
            max_inc_risk=max(perc_inc_risk),
            avg_dec_risk=mean(perc_dec_risk))
wtype.calc

##PHOSPHATE ADDITION
phosphate.data<-all.plotdata%>%
  filter(variable=="Phos_binary")%>%
  mutate(min_state=all[which(all$probability==min),],
         max_state=all[which(all$probability==max),])%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)

phosphate<-phosphate.data%>%
  #mutate(xmin=ifelse(yes>no,no,yes))%>%
  ggplot(aes(y=reorder(model_name,range)))+
  geom_linerange(aes(xmin=min,xmax=max),
                 color="black",
                 size=.9)+
  scale_x_continuous(limits=c(0,1),expand=c(0,0))+
  geom_point(aes(x=min,
                 fill="Phosphate addition",
                 shape="Phosphate addition"),
             color="black",
             size=3)+
  geom_point(aes(x=max,
                 fill="No phosphate addition",
                 shape="No phosphate addition"),
             color="black",
             size=3)+
  geom_point(aes(x=prior,
                 fill="Prior",
                 shape="Prior"),
             color="black",
             size=3,
             alpha=0.7)+
  labs(x="Probability of exceeding the model target",y="Model",fill="",shape="")+
  geom_text_repel(aes(x=max,label=round(max,2)),
                  size=3,nudge_y=0.1)+
  geom_text_repel(aes(x=min,label=round(min,2)),size=3,nudge_y=-0.1)+
  theme_bw()+
  theme(legend.position="top",
        axis.title=element_text(face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_shape_manual(values=c(21,21,21,23),breaks=c("Phosphate addition","No phosphate addition","Prior"))+
  scale_fill_manual(values=cbPalette,breaks=c("Phosphate addition","No phosphate addition","Prior"))
phosphate

phosphate.calc<-all.plotdata%>%
  filter(variable=="Phos_binary")%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)%>%
  mutate(perc_inc_risk=(max-prior)/prior,
         perc_dec_risk=(min-prior)/prior)%>%
  summarise(avg_inc_risk=mean(perc_inc_risk),
            max_inc_risk=max(perc_inc_risk),
            avg_dec_risk=mean(perc_dec_risk))
phosphate.calc

##PH ADJUSTMENT
ph.data<-all.plotdata%>%
  filter(variable=="ph_binary")%>%
  mutate(min_state=all[which(all$probability==min),],
         max_state=all[which(all$probability==max),])%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)

ph<-ph.data%>%
  ggplot(aes(y=reorder(model_name,range)))+
  geom_linerange(aes(xmin=min,xmax=max),
                 color="black",
                 size=.9)+
  scale_x_continuous(limits=c(0,1),expand=c(0,0))+
  geom_point(aes(x=min,
                 fill="pH adjustment",
                 shape="pH adjustment"),
             color="black",
             size=3)+
  geom_point(aes(x=max,
                 fill="No pH adjustment",
                 shape="No pH adjustment"),
             color="black",
             size=3)+
  geom_point(aes(x=prior,
                 fill="Prior",
                 shape="Prior"),
             color="black",
             size=3,
             alpha=0.7)+
  labs(x="Probability of exceeding the model target",y="Model",fill="",shape="")+
  geom_text_repel(aes(x=max,label=round(max,2)),
                  size=3,nudge_y=0.1)+
  geom_text_repel(aes(x=min,label=round(min,2)),size=3,nudge_y=-0.1)+
  theme_bw()+
  theme(legend.position="top",
        axis.title=element_text(face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_shape_manual(values=c(21,21,21,23),breaks=c("pH adjustment","No pH adjustment","Prior"))+
  scale_fill_manual(values=cbPalette,breaks=c("pH adjustment","No pH adjustment","Prior"))
ph

ph.calc<-all.plotdata%>%
  filter(variable=="ph_binary")%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)%>%
  mutate(perc_inc_risk=(max-prior)/prior,
         perc_dec_risk=(min-prior)/prior)%>%
  summarise(avg_inc_risk=mean(perc_inc_risk),
            max_inc_risk=max(perc_inc_risk),
            avg_dec_risk=mean(perc_dec_risk))
ph.calc

ggarrange(phosphate, ph,
          ncol=2,nrow=1,
          align="hv",
          labels=c("A","B"))

ggsave("tornado_summary_ph_phosphate.png",plot=last_plot(),height=4.5,width=9,units="in",dpi=600)


##LCR AL EXCEEDANCE
lcr.data<-all.plotdata%>%
  filter(variable=="any_lcr_exceedance")%>%
  mutate(min_state=all[which(all$probability==min),],
         max_state=all[which(all$probability==max),])%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)

lcr<-lcr.data%>%
  ggplot(aes(y=reorder(model_name,range)))+
  geom_linerange(aes(xmin=min,xmax=max),
                 color="black",
                 size=.9)+
  scale_x_continuous(limits=c(0,1),expand=c(0,0))+
  geom_point(aes(x=min,
                 fill="Action level violation in past 5 years",
                 shape="Action level violation in past 5 years"),
             color="black",
             size=3)+
  geom_point(aes(x=max,
                 fill="LCR not applicable (private well)",
                 shape="LCR not applicable (private well)"),
             color="black",
             size=3)+
  geom_point(aes(x=prior,
                 fill="Prior",
                 shape="Prior"),
             color="black",
             size=3,
             alpha=0.7)+
  labs(x="Posterior probability of exceeding the model target",y="Model",fill="",shape="")+
  geom_text_repel(aes(x=max,label=round(max,2)),
                  size=3,nudge_y=0.1)+
  geom_text_repel(aes(x=min,label=round(min,2)),size=3,nudge_y=0.1)+
  theme_bw()+
  theme(legend.position="right",
        #legend.background = element_rect(fill="white",color="black"),
        legend.title=element_blank(),
        axis.title=element_text(face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_shape_manual(values=c(21,21,23),breaks=c("Action level violation in past 5 years","LCR not applicable (private well)","Prior"))+
  scale_fill_manual(values=cbPalette,breaks=c("Action level violation in past 5 years","LCR not applicable (private well)","Prior"))
lcr

ggsave("tornado_summary_any_lcr_exceedance.png",plot=lcr,height=4,width=7,units="in",dpi=600)

lcr.calc<-all.plotdata%>%
  filter(variable=="any_lcr_exceedance")%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)%>%
  mutate(perc_inc_risk=(max-prior)/prior,
         perc_dec_risk=(min-prior)/prior)%>%
  summarise(avg_inc_risk=mean(perc_inc_risk),
            max_inc_risk=max(perc_inc_risk),
            avg_dec_risk=mean(perc_dec_risk))
lcr.calc

## CONNECTIONS
connections_index<-all.plotdata%>%
  filter(variable=="connections_cat")%>%
  mutate(min_state=all[which(all$probability==min),],
         max_stat=all[which(all$probability==max),])

connections<-all.plotdata%>%
  filter(variable=="connections_cat")%>%
  merge(y=performance_summary,by="model_name",all.x=TRUE)%>%
  ggplot(aes(y=reorder(model_name,range)))+
  geom_linerange(aes(xmin=min,xmax=max),
                 color="black",
                 size=.9)+
  scale_x_continuous(limits=c(0,1),expand=c(0,0))+
  geom_point(aes(x=min,
                 fill="Large/Very Large water systems",
                 shape="Large/Very Large water systems"),
             color="black",
             size=3)+
  geom_point(aes(x=max,
                 fill="Very Small/Small/Medium water systems",
                 shape="Very Small/Small/Medium water systems"),
             color="black",
             size=3)+
  geom_point(aes(x=prior,
                 fill="Prior",
                 shape="Prior"),
             color="black",
             size=3,
             alpha=0.7)+
  labs(x="Probability of exceeding the model target",y="Model",fill="",shape="")+
  geom_text_repel(aes(x=max,label=round(max,2)),size=3)+
  geom_text_repel(aes(x=min,label=round(min,2)),size=3)+
  theme_bw()+
  theme(legend.position="top",
        axis.title=element_text(face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_shape_manual(values=c(21,21,23),breaks=c("Large/Very Large water systems","Very Small/Small/Medium water systems","Prior"))+
  scale_fill_manual(values=c("blue","red","white"),breaks=c("Large/Very Large water systems","Very Small/Small/Medium water systems","Prior"))
connections




