#Generate summary of BN model improvements compared to alternative heuristics

#Author: Riley E. Mulhern, PhD <rmulhern@rti.org>

#Date: February 16, 2023

rm(list=ls())

#### Load libraries and set WD -------------------------------------------------
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggrepel)

cbPalette <- c("#999999", "#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Get working directory
getwd() #This is where the outputs from this script will be saved. To set a different working directory use setwd().
        #The working directory must be set to the file path where the main BN model script outputs were saved.

#Improvement data
perc90above1<-read.csv("improvement_data_x2_perc90above1.csv")
perc90above5<-read.csv("improvement_data_x2_perc90above5.csv")
perc90above10<-read.csv("improvement_data_x2_perc90above10.csv")
perc90above15<-read.csv("improvement_data_x2_perc90above15.csv")

maxabove1<-read.csv("improvement_data_x2_maxabove1.csv")
maxabove5<-read.csv("improvement_data_x2_maxabove5.csv")
maxabove10<-read.csv("improvement_data_x2_maxabove10.csv")
maxabove15<-read.csv("improvement_data_x2_maxabove15.csv")

maxabove1<-cbind(rep("Max>1",length(maxabove1$X)),maxabove1)
maxabove5<-cbind(rep("Max>5",length(maxabove5$X)),maxabove5)
maxabove10<-cbind(rep("Max>10",length(maxabove10$X)),maxabove10)
maxabove15<-cbind(rep("Max>15",length(maxabove15$X)),maxabove15)

perc90above1<-cbind(rep("P90>1",length(perc90above1$X)),perc90above1)
perc90above5<-cbind(rep("P90>5",length(perc90above5$X)),perc90above5)
perc90above10<-cbind(rep("P90>10",length(perc90above10$X)),perc90above10)
perc90above15<-cbind(rep("P90>15",length(perc90above15$X)),perc90above15)

list<-list(maxabove1,maxabove5,maxabove10,maxabove15,
           perc90above1,perc90above5,perc90above10,perc90above15)
colnames<-c("model_name","index","heuristic",
            "mean_sens","upper_sens","lower_sens",
            "mean_pred.pos","upper_pred.pos","lower_pred.pos",
            "mean_F1","mean_F2","model_F1","model_F2","mean_perc_improve_F1","mean_perc_improve_F2",
            "model_n.eq","model_sens.eq",
            "mean_perc_improve_sens","mean_perc_reduce_n")
list_rename<-lapply(list,setNames,colnames)

all<-do.call(rbind,list_rename)
all<-all%>%
  mutate(model_name=factor(model_name,levels=c("Max>1","P90>1","Max>5","P90>5","Max>10","P90>10","Max>15","P90>15")),
         mod_type=factor(ifelse(str_detect(model_name,"Max")==1,"Max models","P90 models")))

#Summarise F-score metrics
summary<-all%>%
  group_by(heuristic)%>%
  summarise(meanF1=mean(mean_F1,na.rm=TRUE),
            minF1=min(mean_F1,na.rm=TRUE),
            maxF1=max(mean_F1,na.rm=TRUE),
            meanF2=mean(mean_F2,na.rm=TRUE),
            minF2=min(mean_F2,na.rm=TRUE),
            maxF2=max(mean_F2,na.rm=TRUE),
            meanpercimproveF1=mean(mean_perc_improve_F1,na.rm=TRUE),
            meanpercimproveF2=mean(mean_perc_improve_F2,na.rm=TRUE))
summary
write.csv(summary,"heuristic F score summary.csv")
mean(all$mean_perc_improve_F1,na.rm=TRUE)
mean(all$mean_perc_improve_F2,na.rm=TRUE)

#Compare F scores
model_F.scores<-all%>%
  group_by(model_name)%>%
  summarise(mean_F1=max(model_F1),
            mean_F2=max(model_F2))%>%
  mutate(heuristic=c("BN model"))
mean(model_F.scores$mean_F1)
mean(model_F.scores$mean_F2)
write.csv(model_F.scores,"model_F.scores.csv")

F1<-all%>%
  select(c("model_name","heuristic","mean_F1","mean_F2"))%>%
  rbind(model_F.scores)%>%
  ggplot(aes(x=heuristic,y=mean_F1))+
  geom_boxplot(outlier.shape = NA,
               aes(fill=heuristic),
               alpha=0.3,
               width=0.5)+
  geom_jitter(aes(shape=model_name),width=0.05,
              #color="black",
              #shape=21,
              size=1.5,
              alpha=0.8)+
  theme_bw()+
  theme(axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        axis.text=element_text(angle=45, hjust=1))+
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8))+
  scale_fill_manual(values=cbPalette)+
  guides(fill="none")+
  labs(y="F1 Score",x="Sampling heuristic",shape="Target")+
  ylim(0,1)
F1

F2<-all%>%
  select(c("model_name","heuristic","mean_F1","mean_F2"))%>%
  rbind(model_F.scores)%>%
  ggplot(aes(x=heuristic,y=mean_F2))+
  geom_boxplot(outlier.shape = NA,
               aes(fill=heuristic),
               alpha=0.3,
               width=0.5)+
  geom_jitter(aes(shape=model_name),width=0.05,
              #color="black",
              #shape=21,
              size=1.5,
              alpha=0.8)+
  theme_bw()+
  theme(axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        axis.text=element_text(angle=45, hjust=1))+
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8))+
  scale_fill_manual(values=cbPalette)+
  guides(fill="none")+
  labs(y="F2 Score",x="Sampling heuristic",shape="Target")+
  ylim(0,1)
F2

ggarrange(F1,F2,
          labels=c("A","B"),label.y=0.98,label.x=0.88,
          font.label=list(size=18),
          common.legend = TRUE)

ggsave("F1, F2 score comparison plot.png",plot=last_plot(),height=4.5,width=7,units="in",dpi=600)


#Plot Sampling Reduction and Sensitivity Improvement
improvesens_mod<-all%>%
  mutate(mod_type=factor(ifelse(str_detect(model_name,"Max")==1,"Max models","P90 models")))%>%
  ggplot(aes(x=mod_type,y=mean_perc_improve_sens*100))+
  geom_boxplot(outlier.shape = NA,
               aes(fill=heuristic),
               alpha=0.3,
               width=0.5)+
  geom_point(aes(shape=model_name,group=heuristic),
             position = position_dodge(width=0.5),
             #color="black",
             #shape=21,
             size=2,
             alpha=0.8)+
  theme_bw()+
  theme(axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"))+
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8))+
  scale_fill_manual(values=cbPalette)+
  scale_y_continuous(limits=c(-60,100),breaks=c(-50,-25,0,25,50,75,100))+
  labs(y="% sensitivity impovement",x="Sampling heuristic",shape="Model",fill="Heuristic")+
  geom_hline(yintercept = 0,linetype="dashed")+
  stat_compare_means()
improvesens_mod

reducen_mod<-all%>%
  mutate(mod_type=factor(ifelse(str_detect(model_name,"Max")==1,"Max models","P90 models")))%>%
  ggplot(aes(x=mod_type,y=mean_perc_reduce_n*100))+
  geom_boxplot(outlier.shape = NA,
               aes(fill=heuristic),
               alpha=0.3,
               width=0.5)+
  geom_point(aes(shape=model_name,group=heuristic),
             position = position_dodge(width=0.5),
             #color="black",
             #shape=21,
             size=2,
             alpha=0.8)+
  theme_bw()+
  theme(axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"))+
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8))+
  scale_fill_manual(values=cbPalette)+
  scale_y_continuous(limits=c(-60,100),breaks=c(-50,-25,0,25,50,75,100))+
  labs(y="% reduction in centers sampled",x="Sampling heuristic",shape="Model",fill="Heuristic")+
  geom_hline(yintercept = 0,linetype="dashed")+
  stat_compare_means()
reducen_mod

ggarrange(improvesens_mod,reducen_mod,
          labels=c("A","B"),label.y=0.98,label.x=0.88,
          font.label=list(size=18),
          common.legend = TRUE,legend="right")

ggsave("improvement plot_mod.png", plot=last_plot(),height=4,width=9,units="in",dpi=600)

#Overall summary
#By model target
all%>%
  group_by(mod_type)%>%
  summarise(mean_perc_reduce=mean(mean_perc_reduce_n),
            med_perc_reduce=median(mean_perc_reduce_n),
            min_perc_reduce=min(mean_perc_reduce_n),
            max_perc_reduce=max(mean_perc_reduce_n),
            mean_perc_improve=mean(mean_perc_improve_sens),
            med_perc_improve=median(mean_perc_improve_sens),
            min_perc_improve=min(mean_perc_improve_sens),
            max_perc_improve=max(mean_perc_improve_sens))

#By heuristic and target
all%>%
  group_by(heuristic,mod_type)%>%
  summarise(mean_perc_reduce=mean(mean_perc_reduce_n),
            med_perc_reduce=median(mean_perc_reduce_n),
            min_perc_reduce=min(mean_perc_reduce_n),
            max_perc_reduce=max(mean_perc_reduce_n),
            mean_perc_improve=mean(mean_perc_improve_sens),
            med_perc_improve=median(mean_perc_improve_sens),
            min_perc_improve=min(mean_perc_improve_sens),
            max_perc_improve=max(mean_perc_improve_sens))

#By individual model
all%>%
  group_by(model_name)%>%
  summarise(mean_perc_reduce=mean(mean_perc_reduce_n),
            med_perc_reduce=median(mean_perc_reduce_n),
            min_perc_reduce=min(mean_perc_reduce_n),
            max_perc_reduce=max(mean_perc_reduce_n),
            mean_perc_improve=mean(mean_perc_improve_sens),
            med_perc_improve=median(mean_perc_improve_sens),
            min_perc_improve=min(mean_perc_improve_sens),
            max_perc_improve=max(mean_perc_improve_sens))
