#Generate summary of predicting performance of BN networks

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
cbPalette2 <- c("#E69F00", "#E69F00","#56B4E9", "#56B4E9")# "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Get working directory
getwd() #This is where the outputs from this script will be saved. To set a different working directory use setwd().
        #The working directory must be set to the file path where the main BN model script outputs were saved.

#Performance summary data
perc90above1<-read.csv("performance_summary_x2_perc90above1.csv")
perc90above5<-read.csv("performance_summary_x2_perc90above5.csv")
perc90above10<-read.csv("performance_summary_x2_perc90above10.csv")
perc90above15<-read.csv("performance_summary_x2_perc90above15.csv")

maxabove1<-read.csv("performance_summary_x2_maxabove1.csv")
maxabove5<-read.csv("performance_summary_x2_maxabove5.csv")
maxabove10<-read.csv("performance_summary_x2_maxabove10.csv")
maxabove15<-read.csv("performance_summary_x2_maxabove15.csv")

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
colnames<-c("model_name","index","target","prior","auroc_train","auroc_test","auroc_kfold","auroc_kfold_max","auroc_kfold_min","aupr_train","aupr_test","aupr_kfold","aupr_kfold_max","aupr_kfold_min")
list_rename<-lapply(list,setNames,colnames)

all<-do.call(rbind,list_rename)
all<-all%>%
  mutate(model_name=factor(model_name,levels=c("Max>1","P90>1","Max>5","P90>5","Max>10","P90>10","Max>15","P90>15")))

model_F.scores<-read.csv("model_F.scores.csv")

model_F.scores%>%
  summarise(meanF1=mean(mean_F1),
            minF1=min(mean_F1),
            maxF1=max(mean_F1),
            meanF2=mean(mean_F2),
            minF2=min(mean_F2),
            maxF2=max(mean_F2))

#Overall performance summary table for each model
all<-merge(x=all,y=model_F.scores,by="model_name",all.x=TRUE)
write.csv(all,"performance_summary_all.csv")


#Visualize performance relationship with prior
gather<-all%>%
  mutate(aupr_diff_from_prior=aupr_kfold-prior)%>%
  select(!c("X","heuristic"))%>%
  gather(key="measure",value="result",auroc_train:aupr_diff_from_prior)

measures<-c("auroc_kfold","aupr_kfold","mean_F2")

gather%>%
  filter(measure %in% measures)%>%
  mutate(measure=factor(measure,levels=c("aupr_kfold","auroc_kfold","mean_F2"),
                        labels=c("K-fold AU-PR","K-fold AU-ROC","Max. F2-score")))%>%
  ggplot(aes(x=prior,y=result))+
  geom_point(aes(fill=measure),
             color="black",
             shape=21,
             alpha=0.4,
             size=4)+
  geom_text_repel(aes(label=model_name),size=2.2)+
  geom_smooth(method="lm",aes(color=measure),se=TRUE,alpha=0.2)+
  stat_cor(method = "pearson",label.x=0.42,label.y=c(0.15,0.2,0.25),
           size=3,
           aes(color=measure))+
  theme_bw()+
  theme(axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        axis.text=element_text(angle=45, hjust=1),
        plot.margin=margin(0.1,0.1,1,0.1,unit="in"),
        legend.position = "top")+
  #ylim(0,1)+
  labs(x="Prior probability of target",y="Model performance",fill="",color="")+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  coord_cartesian(ylim = c(0, 1), # This focuses the x-axis on the range of interest
                  clip = 'off')+
  annotate("segment", x = 0.5, xend = 0.1, y = -.25, yend = -.25,
           colour = "black", size = 0.6, arrow = arrow(type="closed",length=unit(0.3,"cm")))+
  annotate("text",x=0.3,y=-0.3,label="Increasing class imbalance",fontface="italic")

ggsave("performance relationship to prior.png", plot=last_plot(),height=6,width=5,units="in",dpi=600)

#Visualize AU-ROC and AU-PR performance by model target (Max or P90)
kfold_range<-gather%>%
  filter(measure %in% c("auroc_test","aupr_test","auroc_kfold_max","auroc_kfold_min","aupr_kfold_max","aupr_kfold_min"))%>%
  mutate(measure=case_when(grepl("aupr_kfold", measure) ~ "aupr_kfold",
                           grepl("auroc_kfold", measure) ~ "auroc_kfold",
                           TRUE ~ measure),
         model_type=case_when(grepl("max",target) ~ "Max",
                              grepl("perc",target) ~ "P90"))%>%
  mutate(measure=factor(measure,levels=c("aupr_kfold","aupr_test","auroc_kfold","auroc_test"),
                        labels=c("K-fold mean AU-PR","Test set AU-PR","K-fold mean AU-ROC","Test set AU-ROC")))

errorbars<-gather%>%
  filter(measure %in% c("auroc_test","aupr_test","auroc_kfold_max","auroc_kfold_min","aupr_kfold_max","aupr_kfold_min"))%>%
  mutate(measure=case_when(grepl("aupr_kfold", measure) ~ "aupr_kfold",
                      grepl("auroc_kfold", measure) ~ "auroc_kfold",
                      TRUE ~ measure),
         model_type=case_when(grepl("max",target) ~ "Max",
                         grepl("perc",target) ~ "P90"))%>%
  mutate(measure=factor(measure,levels=c("aupr_kfold","aupr_test","auroc_kfold","auroc_test"),
                        labels=c("K-fold mean AU-PR","Test set AU-PR","K-fold mean AU-ROC","Test set AU-ROC")))%>%
  group_by(measure,model_type)%>%
  summarise(ymean=mean(result),
            ymin=min(result),
            ymax=max(result))%>%
  mutate(ymean=case_when(measure=="Test set AU-PR" | measure=="Test set AU-ROC" ~ as.numeric(NA),
                         TRUE ~ ymean))

gather%>%
  mutate(model_type=case_when(grepl("max",target) ~ "Max",
                              grepl("perc",target) ~ "P90"))%>%
  mutate(test_type=case_when(grepl("test",measure) ~ "test",
                            grepl("kfold",measure) ~ "kfold"))%>%
  #filter(measure=="auroc_kfold" | measure=="aupr_kfold")%>%
  filter(measure=="auroc_kfold" | measure=="aupr_kfold" | measure=="auroc_test" | measure=="aupr_test")%>%
  mutate(measure=factor(measure,levels=c("aupr_kfold","aupr_test","auroc_kfold","auroc_test"),
                        labels=c("K-fold mean AU-PR","Test set AU-PR","K-fold mean AU-ROC","Test set AU-ROC")))%>%
  ggplot()+
  geom_crossbar(data=errorbars,
                aes(x=model_type,y=ymean,ymin=ymin,ymax=ymax,group=measure),
                width=0.65,
                #size=4,
                color="white",
                fill="lightgrey",
                alpha=0.4,#width=0,
                position = position_dodge(0.75))+
  geom_boxplot(aes(x=model_type,y=result,fill=measure,alpha=measure))+
  geom_point(aes(x=model_type,y=result,shape=model_name,group=measure),
             position=position_dodge(width=0.75),show.legend=NA)+
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8))+
  ylim(0,1)+
  labs(x="Model target type",y="Model performance",shape="Model target",fill="Performance measure",
       alpha="Performance measure")+
  theme_bw()+
  theme(axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        axis.text=element_text(angle=45, hjust=1),
        legend.position = "right")+
  scale_fill_manual(values=cbPalette2,labels=c("K-fold mean AU-PR","Test set AU-PR","K-fold mean AU-ROC","Test set AU-ROC"))+
  scale_alpha_manual(values=c(1,0.5,1,0.5),labels=c("K-fold mean AU-PR","Test set AU-PR","K-fold mean AU-ROC","Test set AU-ROC"))+
  scale_color_manual(values=c("white","white","white","white"))+
  #scale_color_manual(values=c("lightgrey","white","white","lightgrey"))
  guides(color=FALSE)

ggsave("compare performance by model type.png",plot=last_plot(),width=7,height=5,units="in",dpi=600)
