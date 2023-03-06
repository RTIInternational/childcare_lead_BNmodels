#Generate clean versions of all network structures

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
library(bnlearn)
library(visNetwork)

#Get working directory
getwd() #This is where the outputs from this script will be saved. To set a different working directory use setwd().
#The working directory must be set to the file path where the main BN model script outputs were saved.

plot.network <- function(structure, color, ht = "600px"){
  nodes.uniq <- unique(c(structure$arcs[,1], structure$arcs[,2]))
  nodes <- data.frame(id = nodes.uniq,
                      label = nodes.uniq,
                      color = color, #change color of nodes
                      shadow = TRUE)
  edges <- data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = NA,
                      smooth = TRUE,
                      shadow = TRUE,
                      color = "black")
  return(visNetwork(nodes, edges, height = ht, width = "100%"))
}

#### Load significant variables from each model --------------------------------
vars_maxabove1<-read.csv("sigvars.x2_maxabove1.csv")
vars_maxabove5<-read.csv("sigvars.x2_maxabove5.csv")
vars_maxabove10<-read.csv("sigvars.x2_maxabove10.csv")
vars_maxabove15<-read.csv("sigvars.x2_maxabove15.csv")

vars_perc90above1<-read.csv("sigvars.x2_perc90above1.csv")
vars_perc90above5<-read.csv("sigvars.x2_perc90above5.csv")
vars_perc90above10<-read.csv("sigvars.x2_perc90above10.csv")
vars_perc90above15<-read.csv("sigvars.x2_perc90above15.csv")

# Load arcs from each model ----------------------------------------------------
arcs_maxabove1<-read.csv("bn_arcs_x2_maxabove1.csv")
arcs_maxabove5<-read.csv("bn_arcs_x2_maxabove5.csv")
arcs_maxabove10<-read.csv("bn_arcs_x2_maxabove10.csv")
arcs_maxabove15<-read.csv("bn_arcs_x2_maxabove15.csv")

arcs_perc90above1<-read.csv("bn_arcs_x2_perc90above1.csv")
arcs_perc90above5<-read.csv("bn_arcs_x2_perc90above5.csv")
arcs_perc90above10<-read.csv("bn_arcs_x2_perc90above10.csv")
arcs_perc90above15<-read.csv("bn_arcs_x2_perc90above15.csv")

# Create empty networks --------------------------------------------------------
bn_maxabove1<-empty.graph(vars_maxabove1$x)
bn_maxabove5<-empty.graph(vars_maxabove5$x)
bn_maxabove10<-empty.graph(vars_maxabove10$x)
bn_maxabove15<-empty.graph(vars_maxabove15$x)

bn_perc90above1<-empty.graph(vars_perc90above1$x)
bn_perc90above5<-empty.graph(vars_perc90above5$x)
bn_perc90above10<-empty.graph(vars_perc90above10$x)
bn_perc90above15<-empty.graph(vars_perc90above15$x)

# Define each network structure ------------------------------------------------
arcs(bn_maxabove1)<-arcs_maxabove1[,2:3]
arcs(bn_maxabove5)<-arcs_maxabove5[,2:3]
arcs(bn_maxabove10)<-arcs_maxabove10[,2:3]
arcs(bn_maxabove15)<-arcs_maxabove15[,2:3]

arcs(bn_perc90above1)<-arcs_perc90above1[,2:3]
arcs(bn_perc90above5)<-arcs_perc90above5[,2:3]
arcs(bn_perc90above10)<-arcs_perc90above10[,2:3]
arcs(bn_perc90above15)<-arcs_perc90above15[,2:3]

# Node name dictionary ---------------------------------------------------------
levels=c("any_lcr_exceedance",
           "chloramines",
           "coagulation",
           "connections_cat",
           "fixture_year_cat",
           "head_start",
           "home_based",
           "med_hh_income_cbg",
           "nsamples",
           "PER_FREE",
           "PER_NON_WHITE",
           "perc_filtered",
           "perc_hs_higher_cbg",
           "perc_non_white_cbg",
           "ph_binary",
           "Phos_binary",
           "ruca_cat",
           "school",
           "TOTAL_ENROLL",
           "type_binary",
           "WASTE_SYSTEM",
           "Y_N_FIXTURE_CHG",
           "year_began_operating_cat")
names=c("Past LCR exceedance",
        "Chloramination",
        "Coagulation",
        "# connections of water system",
        "Year of past faucet fixture change",
        "Head Start",
        "Home-based",
        "Block group median household income",
        "# samples",
        "% free/reduced lunch enrollment",
        "% non-White enrollment",
        "% taps filtered",
        "Block group educational attainment",
        "Block group % non-White",
        "pH adjustment",
        "Phosphate addition",
        "Urbanicity",
        "School-based",
        "Total enrollment",
        "Source water type",
        "On-site wastewater system",
        "Past faucet fixture change",
        "Year center began operating")
dict<-data.frame(levels,names)

# Rename nodes -------------------------------------------------------------
names_bn_maxabove1<-data.frame(nodes(bn_maxabove1))%>%
  rename("levels"="nodes.bn_maxabove1.")%>%
  merge(y=dict,by="levels",all.x=TRUE,sort=FALSE)%>%
  mutate(names=ifelse(levels=="target","Max>=1",names))
rename_maxabove1<-rename.nodes(bn_maxabove1,names_bn_maxabove1$names)

names_bn_maxabove5<-data.frame(nodes(bn_maxabove5))%>%
  rename("levels"="nodes.bn_maxabove5.")%>%
  merge(y=dict,by="levels",all.x=TRUE,sort=FALSE)%>%
  mutate(names=ifelse(levels=="target","Max>=5",names))
rename_maxabove5<-rename.nodes(bn_maxabove5,names_bn_maxabove5$names)

names_bn_maxabove10<-data.frame(nodes(bn_maxabove10))%>%
  rename("levels"="nodes.bn_maxabove10.")%>%
  merge(y=dict,by="levels",all.x=TRUE,sort=FALSE)%>%
  mutate(names=ifelse(levels=="target","Max>=10",names))
rename_maxabove10<-rename.nodes(bn_maxabove10,names_bn_maxabove10$names)

names_bn_maxabove15<-data.frame(nodes(bn_maxabove15))%>%
  rename("levels"="nodes.bn_maxabove15.")%>%
  merge(y=dict,by="levels",all.x=TRUE,sort=FALSE)%>%
  mutate(names=ifelse(levels=="target","Max>=15",names))
rename_maxabove15<-rename.nodes(bn_maxabove15,names_bn_maxabove15$names)

names_bn_perc90above1<-data.frame(nodes(bn_perc90above1))%>%
  rename("levels"="nodes.bn_perc90above1.")%>%
  merge(y=dict,by="levels",all.x=TRUE,sort=FALSE)%>%
  mutate(names=ifelse(levels=="target","P90>=1",names))
rename_perc90above1<-rename.nodes(bn_perc90above1,names_bn_perc90above1$names)

names_bn_perc90above5<-data.frame(nodes(bn_perc90above5))%>%
  rename("levels"="nodes.bn_perc90above5.")%>%
  merge(y=dict,by="levels",all.x=TRUE,sort=FALSE)%>%
  mutate(names=ifelse(levels=="target","P90>=5",names))
rename_perc90above5<-rename.nodes(bn_perc90above5,names_bn_perc90above5$names)

names_bn_perc90above10<-data.frame(nodes(bn_perc90above10))%>%
  rename("levels"="nodes.bn_perc90above10.")%>%
  merge(y=dict,by="levels",all.x=TRUE,sort=FALSE)%>%
  mutate(names=ifelse(levels=="target","P90>=10",names))
rename_perc90above10<-rename.nodes(bn_perc90above10,names_bn_perc90above10$names)

names_bn_perc90above15<-data.frame(nodes(bn_perc90above15))%>%
  rename("levels"="nodes.bn_perc90above15.")%>%
  merge(y=dict,by="levels",all.x=TRUE,sort=FALSE)%>%
  mutate(names=ifelse(levels=="target","P90>=15",names))
rename_perc90above15<-rename.nodes(bn_perc90above15,names_bn_perc90above15$names)


#Plot each network -------------------------------------------------------------
plot.network(rename_maxabove1,"#A3A3DB")
plot.network(rename_maxabove5,"#D1D1ED")
plot.network(rename_maxabove10,"#FFFFFF")
plot.network(rename_maxabove15,"#FFBFBF")

plot.network(rename_perc90above1,"#BABAE3")
plot.network(rename_perc90above5,"#E8E8F6")
plot.network(rename_perc90above10,"#FFDFDF")
plot.network(rename_perc90above15,"#FF9F9F")
