#Summarize frequency of significant variables in all models

#Author: Riley E. Mulhern, PhD <rmulhern@rti.org>

#Date: December 22, 2022

rm(list=ls())

#### Load libraries and set WD -------------------------------------------------
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Get working directory 
getwd() #This is where the outputs from this script will be saved. To set a different working directory use setwd().
#The working directory must be set to the file path where the main BN model script outputs were saved. 

#### Load significant variables from each model --------------------------------
maxabove1<-read.csv("sigvars_maxabove1.csv")
maxabove5<-read.csv("sigvars_maxabove5.csv")
maxabove10<-read.csv("sigvars_maxabove10.csv")
maxabove15<-read.csv("sigvars_maxabove15.csv")

perc90above1<-read.csv("sigvars_perc90above1.csv")
perc90above5<-read.csv("sigvars_perc90above5.csv")
perc90above10<-read.csv("sigvars_perc90above10.csv")
perc90above15<-read.csv("sigvars_perc90above15.csv")

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
colnames<-c("model","index","variable")
list_rename<-lapply(list,setNames,colnames)

all<-do.call(rbind,list_rename)

#Generate a frequency count for each variable 
summary_vars<-all%>%
  filter(variable!="target")%>%
  group_by(variable)%>%
  summarise(count=n())%>%
  mutate(variable=factor(variable,levels=c("chloramines",
                                           "coagulation",
                                           "connections_cat",
                                           "cws",
                                           "fixture_year_cat",
                                           "head_start",
                                           "home_based",
                                           "lcr_over1",
                                           "LCR15_0.1_bin",
                                           "MEDIAN_hh_income_1mile",
                                           "nsamples",
                                           "PER_FREE",
                                           "PER_NON_WHITE",
                                           "perc_filtered",
                                           "ph_binary",
                                           "Phos_binary",
                                           "private_well",
                                           "purchased",
                                           "ruca_cat",
                                           "school",
                                           "TOTAL_ENROLL",
                                           "type_binary",
                                           "WASTE_SYSTEM",
                                           "wells",
                                           "Y_N_FIXTURE_CHG",
                                           "year_began_operating_cat"),
                         labels=c("Chloramination",
                                  "Coagulation",
                                  "# connections of water system",
                                  "Community water system",
                                  "Year of past faucet fixture change",
                                  "Head Start",
                                  "Home-based",
                                  "# LCR samples above 1 ppb",
                                  "LCR 90th percentile above 15 ppb",
                                  "Median household income 1 mile radius",
                                  "# samples",
                                  "% free/reduced lunch enrollment",
                                  "% non-White enrollment",
                                  "% taps filtered",
                                  "pH adjustment",
                                  "Phosphate addition",
                                  "Private well",
                                  "Purchased water",
                                  "Urbanicity",
                                  "School-based",
                                  "Total enrollment",
                                  "Source water type",
                                  "On-site wastewater system",
                                  "Number of wells in water system network",
                                  "Past faucet fixture change",
                                  "Year center began operating")))

#Generate a frequency count for each model
summary_models<-all%>%
  filter(variable!="target")%>%
  group_by(model)%>%
  summarise(count=n())

#Variable frequency plot                               
vars_plot<-all%>%
  filter(variable!="target")%>%
  mutate(variable=factor(variable,levels=c("chloramines",
                                           "coagulation",
                                           "connections_cat",
                                           "cws",
                                           "fixture_year_cat",
                                           "head_start",
                                           "home_based",
                                           "lcr_over1",
                                           "LCR15_0.1_bin",
                                           "MEDIAN_hh_income_1mile",
                                           "nsamples",
                                           "PER_FREE",
                                           "PER_NON_WHITE",
                                           "perc_filtered",
                                           "ph_binary",
                                           "Phos_binary",
                                           "private_well",
                                           "purchased",
                                           "ruca_cat",
                                           "school",
                                           "TOTAL_ENROLL",
                                           "type_binary",
                                           "WASTE_SYSTEM",
                                           "wells",
                                           "Y_N_FIXTURE_CHG",
                                           "year_began_operating_cat"),
                         labels=c("Chloramination",
                                  "Coagulation",
                                  "# connections of water system",
                                  "Community water system",
                                  "Year of past faucet fixture change",
                                  "Head Start",
                                  "Home-based",
                                  "# LCR samples above 1 ppb",
                                  "LCR 90th percentile above 15 ppb",
                                  "Median household income 1 mile radius",
                                  "# samples",
                                  "% free/reduced lunch enrollment",
                                  "% non-White enrollment",
                                  "% taps filtered",
                                  "pH adjustment",
                                  "Phosphate addition",
                                  "Private well",
                                  "Purchased water",
                                  "Urbanicity",
                                  "School-based",
                                  "Total enrollment",
                                  "Source water type",
                                  "On-site wastewater system",
                                  "Number of wells in water system network",
                                  "Past faucet fixture change",
                                  "Year center began operating")),
         model=factor(model,levels=c("Max>1","P90>1","Max>5","P90>5","Max>10","P90>10","Max>15","P90>15")))%>%
  merge(y=summary_vars,by="variable",all.x=TRUE)%>%
  ggplot(aes(x=reorder(variable,-count)))+
  geom_bar(stat="count",
           color="black",
           aes(fill=model),
           alpha=0.4,
           width=0.7)+
  #geom_text(aes(x=variable,y=count,label=model,group=model))
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"))+
  scale_y_continuous(expand=c(0,0.1))+
  labs(x="Predictor variable",y="Count of appearance out of all 8 models",fill="Model")+
  scale_fill_manual(values=palette)
  #scale_x_discrete(labels = function(variable) str_wrap(variable, width = 25))
vars_plot

#Model frequency plot
models_plot<-summary_models%>%
  mutate(model=factor(model,levels=c("Max>1","P90>1","Max>5","P90>5","Max>10","P90>10","Max>15","P90>15")))%>%
  ggplot(aes(x=model,y=count))+
  geom_bar(stat="identity",
           color="black",
           aes(fill=model),
           width=0.7,
           alpha=0.4)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        legend.position="blank",
        plot.background=element_rect(color="black",fill="white"))+
  scale_y_continuous(expand=c(0,0.2))+
  labs(x="Model",y="# of predictors")+
  scale_fill_manual(values=palette)
models_plot

inset_plot<-vars_plot+
  annotation_custom(grob=ggplotGrob(models_plot),
                    ymin=3.5,ymax=7.8,xmin="# LCR samples above 1 ppb",xmax="% taps filtered")
inset_plot
ggsave("sigvars_model summary plot.png",plot=last_plot(),height=6.5,width=8.5,units="in",dpi=600)

#Generate summary table of all variables included in each model
variable<-c("chloramines",
          "coagulation",
          "connections_cat",
          "cws",
          "fixture_year_cat",
          "head_start",
          "home_based",
          "lcr_over1",
          "LCR15_0.1_bin",
          "MEDIAN_hh_income_1mile",
          "nsamples",
          "PER_FREE",
          "PER_NON_WHITE",
          "perc_filtered",
          "ph_binary",
          "Phos_binary",
          "private_well",
          "purchased",
          "ruca_cat",
          "school",
          "TOTAL_ENROLL",
          "type_binary",
          "WASTE_SYSTEM",
          "wells",
          "Y_N_FIXTURE_CHG",
          "year_began_operating_cat")
names<-c("Chloramination",
         "Coagulation",
         "# connections of water system",
         "Community water system",
         "Year of past faucet fixture change",
         "Head Start",
         "Home-based",
         "# LCR samples above 1 ppb",
         "LCR 90th percentile above 15 ppb",
         "Median household income 1 mile radius",
         "# samples",
         "% free/reduced lunch enrollment",
         "% non-White enrollment",
         "% taps filtered",
         "pH adjustment",
         "Phosphate addition",
         "Private well",
         "Purchased water",
         "Urbanicity",
         "School-based",
         "Total enrollment",
         "Source water type",
         "On-site wastewater system",
         "Number of wells in water system network",
         "Past faucet fixture change",
         "Year center began operating")
dict<-data.frame(variable,names)

summary_table<-all%>%
  merge(y=dict,by="variable",all.x=TRUE,sort=FALSE)

write.csv(summary_table,"sig_vars_summary_table.csv")

