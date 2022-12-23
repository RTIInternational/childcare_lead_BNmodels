#Title: BAYESIAN NETWORK MODELS TO PREDICT WATER LEAD RISK IN NORTH CAROLINA CHILD CARE FACILITIES

#Author: Riley E. Mulhern, PhD <rmulhern@rti.org>

#Date: December 22, 2022

rm(list=ls()) #Clear environment

#### Load libraries ------------------------------------------------------------
library(dplyr) #data wrangling
library(tidyverse) #data wrangling
library(ggplot2) #plotting
library(ggrepel) #plotting
library(gRain) #plotting
library(bnlearn) #learn Bayesian network structure
library(visNetwork) #plot network structure
library(Rgraphviz) #plot network structure
library(arulesCBA) #discretize continuous variables
library(ForestDisc) #random forest discretization of continuous variables
library(ROCR) #evaluate performance using ROC curve
library(purrr) #compile ROC values from nested lists
library(caret) #confusion matrix
library(mice) #missing data

cbPalette <- c("#E69F00","#56B4E9","#999999","#009E73", "#D55E00", "#CC79A7","#0072B2","#F0E442") #color blind color palette
cbPalette2 <- c("#E69F00","#56B4E9","#009E73","#D55E00","#0072B2","#F0E442","#CC79A7","#999999")

#Get working directory 
getwd() #This is where the outputs from this script will be saved. To set an different working directory use setwd().

#### Load data -----------------------------------------------------------------
#Import data set from Github:
data<-read.csv("https://github.com/RTIInternational/childcare_lead_BNmodels/raw/main/data/Mulhern%20et%20al_Improved%20decision%20making%20for%20water%20lead%20testing_DATA.csv")

###SET TARGET NODE
target<-"maxabove1" #Options = maxabove1, maxabove5, maxabove10, maxabove15, perc90above1, perc90above5, perc90above10, perc90above15

#Define numeric model variables
num_vars<-c("PER_FREE",
               "PER_NON_WHITE",
               "TOTAL_ENROLL",
               "nsamples",
               "perc_filtered",
               "wells",
               "lcr_mean",
               "lcr_med",
               "lcr_max",
               "lcr_over15",
               "lcr_over10",
               "lcr_over1",
               "MEDIAN_hh_income_1mile",
               "PC_non_white_hh_1mile",
               "MEDIAN_hh_income_0.5mile",
               "PC_non_white_hh_0.5mile",
               "MEDIAN_hh_income_0.25mile",
               "PC_non_white_hh_0.25mile")

#Define categorical model variables
cat_vars<-c("OWN_OR_LEASE",
                "head_start",
                "school",
                "home_based",
                "franchised",
                "BUILT_cat",
                "opzone",
                "cws",
                "private_well",
                "WASTE_SYSTEM",
                "Y_N_FIXTURE_CHG",
                "fixture_year_cat",
                "year_began_operating_cat",
                "LCR15_0.1_bin",
                "purchased",
                "type_binary",
                "ph_binary",
                "Phos_binary",
                "chloramines",
                "coagulation",
                "connections_cat",
                "ruca_cat")

#### Set seed and target node --------------------------------------------------
set.seed(32)
data<-data%>%
  select(all_of(c(target,num_vars,cat_vars)))%>% #select only target node and predictor variables
  mutate(across(all_of(c(target,cat_vars)),factor))%>% #define target and categorical variables as factors
  rename(target=paste(target))

#### Split into test and train data sets ---------------------------------------
data$id<-1:nrow(data)

train_data<-data%>%
  sample_frac(0.80) #retain 80% of data set for training, 20% for testing

test_data<-anti_join(data,train_data,by="id")

#### Impute missing data, handle test/train partitions separately---------------
#Train data
md.pattern(train_data,rotate.names = TRUE) #view missing data
temp_train<-mice(train_data,m=1,maxit=5,meth="cart",seed=500) #run imputation, cart=classification and regression trees
train_data_complete<-complete(temp_train,1)
md.pattern(train_data_complete,rotate.names = TRUE) #confirm no missing values

#Test data
md.pattern(test_data,rotate.names = TRUE) #view missing data
temp_test<-mice(test_data,m=1,maxit=5,meth="cart",seed=500) #run imputation, cart=classification and regression trees
test_data_complete<-complete(temp_test,1)
md.pattern(test_data_complete,rotate.names = TRUE) #confirm no missing values

#### Discretize data for machine learning -----------------------------------------
      #supervised discretization of continuous variables 
train_num_disc_list<-train_data_complete%>%
  select(all_of(c(num_vars,"target")))%>%
  mutate(across(all_of(num_vars),as.numeric))%>%
  ForestDisc(data=.,id_target=19,max_splits=2,opt_meth = "SLSQP")

train_num_disc<-train_num_disc_list[["data_disc"]]%>%
  mutate(across(all_of(num_vars),factor)) #drop any factor levels with no observations, required for bnlearn
summary(train_num_disc)

train_complete_disc<-train_data_complete%>%
  select(all_of(c(cat_vars)))%>%
  cbind(train_num_disc)
summary(train_complete_disc)

#Assign cutpoints identified for the training set to the test set (required step for BN models)
test_complete_disc<-test_data_complete%>%
  mutate(PER_FREE=cut(PER_FREE,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[1]],Inf)),
         PER_NON_WHITE=cut(PER_NON_WHITE,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[2]],Inf)),
         TOTAL_ENROLL=cut(TOTAL_ENROLL,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[3]],Inf)),
         nsamples=cut(nsamples,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[4]],Inf)),
         perc_filtered=cut(perc_filtered,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[5]],Inf)),
         wells=cut(wells,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[6]],Inf)),
         lcr_mean=cut(lcr_mean,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[7]],Inf)),
         lcr_med=cut(lcr_med,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[8]],Inf)),
         lcr_max=cut(lcr_max,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[9]],Inf)),
         lcr_over15=cut(lcr_over15,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[10]],Inf)),
         lcr_over10=cut(lcr_over10,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[11]],Inf)),
         lcr_over1=cut(lcr_over1,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[12]],Inf)),
         MEDIAN_hh_income_1mile=cut(MEDIAN_hh_income_1mile,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[13]],Inf)),
         PC_non_white_hh_1mile=cut(PC_non_white_hh_1mile,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[14]],Inf)),
         MEDIAN_hh_income_0.5mile=cut(MEDIAN_hh_income_0.5mile,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[15]],Inf)),
         PC_non_white_hh_0.5mile=cut(PC_non_white_hh_0.5mile,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[16]],Inf)),
         MEDIAN_hh_income_0.25mile=cut(MEDIAN_hh_income_0.25mile,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[17]],Inf)),
         PC_non_white_hh_0.25mile=cut(PC_non_white_hh_0.25mile,breaks=c(-Inf,train_num_disc_list[["listcutp"]][[18]],Inf)))%>%
  select(!"id")%>%
  mutate(across(all_of(num_vars),factor)) #drop any factor levels with no observations, required for bnlearn
summary(test_complete_disc)

#### Learn BN structure -----------------------------------------------------------
# plot network function - from https://www.r-bloggers.com/2018/09/bayesian-network-example-with-the-bnlearn-package/
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

#Learn and plot Bayesian network structure using all possible variables
bn<-tree.bayes(train_complete_disc, "target")
plot.network(bn,"darkturquoise")

#Evaluate strength of arcs, select only nodes with significant arcs connected to target
strengths.x2<-arc.strength(bn,data=train_complete_disc,criterion = "x2") #Pearson's chi-squared criterion
sig_arcs.x2<-strengths.x2%>%
  filter(from=="target")%>%
  filter(strength<0.05)

sig_vars.x2<-c(sig_arcs.x2$to,"target") #list significant variable names
sig_vars.x2
write.csv(sig_vars.x2,paste0("sigvars_",target,".csv"))
length(sig_vars.x2) #number of significant predictors for target

train_complete_disc_select<-train_complete_disc%>% #compile new training data set with only significant variables
  dplyr::select(all_of(c(sig_vars.x2)))

test_complete_disc_select<-test_complete_disc%>% #compile new test data set with only significant variables
  dplyr::select(all_of(c(sig_vars.x2)))

#Re-learn Bayesian network structure with significant nodes only
bn.sig<-tree.bayes(train_complete_disc_select,"target")
plot.network(bn.sig,"darkturquoise")

write.csv(bn.sig$arcs,paste0("bn_arcs_",target,".csv"))

#### Evaluate BN performance ------------------------------------------------------

#Evaluate within train data-set performance
fitted<-bn.fit(bn.sig, train_complete_disc_select, method = "bayes") #compute complete conditional probability table from the network
pred_probs_train<-predict(fitted, train_complete_disc_select, prob=TRUE) #predict probabilities of target node given network
probs_train<-data.frame(t(attributes(pred_probs_train)$prob))
realresult_train<-train_complete_disc_select%>%dplyr::select(all_of("target"))
pred_class_train<-prediction(probs_train$X1,realresult_train)
perf_class_train<-performance(pred_class_train,"tpr","fpr")
plot(perf_class_train) #plot ROC curve

auc_train<-as.numeric(performance(pred_class_train, "auc")@y.values) #calculate area under the ROC curve
auc_train #print AU-ROC value for training data set

aucpr_train<-as.numeric(performance(pred_class_train,"aucpr")@y.values) #calculate area under the PR curve
aucpr_train #print AU-PR value for training data set

#Store ROC curve data for training set
roc_train<-data.frame(x=perf_class_train@x.values,
                     y=perf_class_train@y.values)
colnames(roc_train)<-c("x","y")

#Store PR curve data for training set
perf_class_pr_train<-performance(pred_class_train,"ppv","tpr")
pr_train<-data.frame(x=perf_class_pr_train@x.values,
                     y=perf_class_pr_train@y.values)
colnames(pr_train)<-c("x","y")

#K-fold cross-validation
k=10 #use 10 folds
kfold<-bn.cv(data=train_complete_disc_select, 
             bn="tree.bayes",
             method="k-fold",
             k=k,
             runs=1,
             loss="pred-exact", #select loss function logl=log-likelihood loss
             loss.args=list(target="target"),
             algorithm.args = list(training="target"))
names(kfold)<-c(1:k) #assign each element of k-fold list output the corresponding fold number

#Generate ROC curves for each fold
bn.data<-train_complete_disc_select #rename data set for ROC functions
calc.roc.kfold<-function(list){
  test.index<-list[["test"]]
  test.set<-na.omit(bn.data[test.index,])
  train.set<-na.omit(bn.data[-test.index,])
  fitted<-list[["fitted"]]
  observed<-list[["observed"]]
  pred_probs<-predict(fitted,test.set,prob=TRUE)
  probs<-data.frame(t(attributes(pred_probs)$prob))
  pred_obs_sidebyside<-data.frame(probs$X1,observed)%>%filter(is.nan(probs.X1)==FALSE) #remove any NaN results from predicted probabilities
  pred_class<-prediction(pred_obs_sidebyside$probs.X1,pred_obs_sidebyside$observed)
  perf_class<-performance(pred_class,"tpr","fpr")
  auc<-as.numeric(performance(pred_class, "auc")@y.values) #calculate area under the ROC curve
  return(auc)
} #function to calculate average AUC from k-fold cross validation

kfold.auc<-sapply(kfold,calc.roc.kfold)
kfold.auc #AU-ROC value from each fold
mean.auc<-mean(kfold.auc)
mean.auc #mean AU-ROC value

#Evaluate performance on test data set
pred_probs_test<-predict(fitted, test_complete_disc_select, prob=TRUE) #predict probabilities of target node given fitted model 
probs_test<-data.frame(t(attributes(pred_probs_test)$prob))
realresult_test<-test_complete_disc_select%>%dplyr::select(all_of("target"))
pred_class_test<-prediction(probs_test$X1,realresult_test)
perf_class_test<-performance(pred_class_test,"tpr","fpr")
plot(perf_class_test) #plot ROC curve

auc_test<-as.numeric(performance(pred_class_test, "auc")@y.values) #calculate area under the ROC curve
auc_test #print AU-ROC value for full data set

aucpr_test<-as.numeric(performance(pred_class_test,"aucpr")@y.values) #calculate area under the PR curve
aucpr_test #print AU-PR value for training data set


#Store ROC curve data for test set
roc_test<-data.frame(x=perf_class_test@x.values,
                     y=perf_class_test@y.values)
colnames(roc_test)<-c("x","y")

#Store PR curve data for test set
perf_class_pr_test<-performance(pred_class_test,"ppv","tpr")
pr_test<-data.frame(x=perf_class_pr_test@x.values,
                     y=perf_class_pr_test@y.values)
colnames(pr_test)<-c("x","y")

#Compare all ROC performance curves
plot.roc.kfold<-function(list){
  test.index<-list[["test"]]
  test.set<-bn.data[test.index,]
  train.set<-bn.data[-test.index,]
  fitted<-list[["fitted"]]
  observed<-list[["observed"]]
  pred_probs<-predict(fitted,test.set,prob=TRUE)
  probs<-data.frame(t(attributes(pred_probs)$prob))
  pred_obs_sidebyside<-data.frame(probs$X1,observed)%>%filter(is.nan(probs.X1)==FALSE) #remove any NaN results from predicted probabilities
  pred_class<-prediction(pred_obs_sidebyside$probs.X1,pred_obs_sidebyside$observed)
  perf_class<-performance(pred_class,"tpr","fpr")
  x<-perf_class@x.values
  y<-perf_class@y.values
  roc.values<-data.frame(x,y)
  colnames(roc.values)<-c("fpr","tpr")
  return(roc.values)
} #function to return fpr and tpr for each fold

roc.values.kfold.list<-lapply(kfold,plot.roc.kfold) #compile fpr and tpr results for each fold
roc.values.kfold.df <- map_df(roc.values.kfold.list, ~as.data.frame(.x), .id="fold") #concatenate fpr and tpr results for plotting

roc.values.average<-roc.values.kfold.df%>% #calculate average ROC value across all folds
  mutate(bin_fpr=cut(fpr,breaks=100))%>%
  group_by(bin_fpr)%>%
  summarise(mean_fpr=mean(fpr),
            mean_tpr=mean(tpr),
            sd_tpr=sd(tpr),
            n=n(),
            upper=mean_tpr+1.96*(sd_tpr/sqrt(n())), #calculate 95% confidence interval around the mean
            lower=mean_tpr-1.96*(sd_tpr/sqrt(n())))


#Plot ROC curve for each fold and average of all folds
auc.label1<-paste("'Train data set AU-ROC =",round(auc_train,3),"'")
auc.label2<-paste("'K-fold mean AU-ROC =",round(mean.auc,3),"'")
auc.label3<-paste("'Test data set AU-ROC =",round(auc_test,3),"'")

roc.values.kfold.df%>%
  ggplot(aes(x=fpr,y=tpr))+
  geom_line(aes(group=fold,
                color="K-fold curves"),
            alpha=0.4)+
  geom_smooth(data=roc.values.average,
            aes(x=mean_fpr,y=mean_tpr,color="K-fold mean"),se=FALSE,
            size=1.2)+
  geom_abline(slope=1,
              size=1,
              linetype="dashed",
              color="black")+
  theme_bw()+
  theme(axis.title=element_text("bold"),
        legend.title=element_blank(),
        legend.position = c(0.8,0.2),
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        legend.background = element_rect(fill="white",color="black"))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x="False Positive Rate",y="True Positive Rate",title=paste(target))+
  annotate(geom="text",x=0.8,y=0.5,label=auc.label1,parse=TRUE,
           size=4)+
  annotate(geom="text",x=0.8,y=0.45,label=auc.label2,parse=TRUE,
           size=4)+
  annotate(geom="text",x=0.8,y=0.4,label=auc.label3,parse=TRUE,
           size=4)+
  geom_line(data=roc_train, aes(x=x,y=y,color="Train data set"),
            linetype="dotdash",
            size=1.2)+
  geom_line(data=roc_test, aes(x=x,y=y,color="Test data set"),
            linetype="dashed",
            size=1.2)+
  scale_color_manual(breaks=c("Train data set","Test data set","K-fold curves","K-fold mean"),
                     values=cbPalette)+
  scale_fill_manual(values="grey")

ggsave(paste0("ROC_",target,".png"),plot=last_plot(),height=6,width=7,units="in",dpi=600) #save ROC output to working directory

#Area under Precision-Recall curve (better for imbalanced data sets)
calc.pr.kfold<-function(list){
  test.index<-list[["test"]]
  test.set<-na.omit(bn.data[test.index,])
  train.set<-na.omit(bn.data[-test.index,])
  fitted<-list[["fitted"]]
  observed<-list[["observed"]]
  pred_probs<-predict(fitted,test.set,prob=TRUE)
  probs<-data.frame(t(attributes(pred_probs)$prob))
  pred_obs_sidebyside<-data.frame(probs$X1,observed)%>%filter(is.nan(probs.X1)==FALSE) #remove any NaN results from predicted probabilities
  pred_class<-prediction(pred_obs_sidebyside$probs.X1,pred_obs_sidebyside$observed)
  perf_class<-performance(pred_class,"tpr","fpr")
  aucpr<-as.numeric(performance(pred_class, "aucpr")@y.values) #calculate area under the ROC curve
  return(aucpr)
} #function to calculate average AUC from k-fold cross validation

kfold.aucpr<-sapply(kfold,calc.pr.kfold)
kfold.aucpr #Au-PR value from each fold
mean.aucpr<-mean(kfold.aucpr)
mean.aucpr #mean AU-PR value

plot.pr.kfold<-function(list){
  test.index<-list[["test"]]
  test.set<-bn.data[test.index,]
  train.set<-bn.data[-test.index,]
  fitted<-list[["fitted"]]
  observed<-list[["observed"]]
  pred_probs<-predict(fitted,test.set,prob=TRUE)
  probs<-data.frame(t(attributes(pred_probs)$prob))
  pred_obs_sidebyside<-data.frame(probs$X1,observed)%>%filter(is.nan(probs.X1)==FALSE) #remove any NaN results from predicted probabilities
  pred_class<-prediction(pred_obs_sidebyside$probs.X1,pred_obs_sidebyside$observed)
  perf_class<-performance(pred_class,"ppv","tpr")
  x<-perf_class@x.values
  y<-perf_class@y.values
  pr.values<-data.frame(x,y)
  colnames(pr.values)<-c("tpr","ppv")
  return(pr.values)
} #function to return fpr and tpr for each fold

pr.values.kfold.list<-lapply(kfold,plot.pr.kfold) #compile fpr and tpr results for each fold
pr.values.kfold.df <- map_df(pr.values.kfold.list, ~as.data.frame(.x), .id="fold") #concatenate fpr and tpr results for plotting

pr.values.average<-pr.values.kfold.df%>% #calculate average ROC value across all folds
  mutate(bin_tpr=cut(tpr,breaks=100))%>%
  group_by(bin_tpr)%>%
  summarise(mean_tpr=mean(tpr),
            mean_ppv=mean(ppv),
            sd_ppv=sd(ppv),
            upper=mean_ppv+1.96*(sd_ppv/sqrt(n())), #calculate 99% confidence interval around the mean
            lower=mean_ppv-1.96*(sd_ppv/sqrt(n())))

#Plot PR curve for each fold and average of all folds
aucpr.label1<-paste("'Train data set AU-PR =",round(aucpr_train,3),"'")
aucpr.label2<-paste("'K-fold mean AU-PR =",round(mean.aucpr,3),"'")
aucpr.label3<-paste("'Test data set AU-PR =",round(aucpr_test,3),"'")

prior<-train_complete_disc_select%>% #calculate prior probability of the target
  summarise(prior_above=sum(.data[["target"]]==1)/n(),
            prior_below=sum(.data[["target"]]==0)/n())
prior

pr.values.kfold.df%>%
  ggplot(aes(x=tpr,y=ppv))+
  geom_line(aes(group=fold,
                color="K-fold curves"),
            alpha=0.4)+
  geom_smooth(data=pr.values.average,
              aes(x=mean_tpr,y=mean_ppv,color="K-fold mean"),
              size=1.2,se=FALSE)+
  theme_bw()+
  theme(axis.title=element_text("bold"),
        legend.title=element_blank(),
        legend.position = c(0.8,0.7),
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        legend.background = element_rect(fill="white",color="black"))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits = c(0,1))+
  labs(x="True Positive Rate (Recall)",y="Precision",title=paste(target))+
  annotate(geom="text",x=0.8,y=0.95,label=aucpr.label1,parse=TRUE,
           size=4)+
  annotate(geom="text",x=0.8,y=0.90,label=aucpr.label2,parse=TRUE,
           size=4)+
  annotate(geom="text",x=0.8,y=0.85,label=aucpr.label3,parse=TRUE,
           size=4)+
  annotate(geom="text",x=0.05,y=prior$prior_above+0.02,label="Prior")+
  geom_line(data=pr_train, aes(x=x,y=y,color="Train data set"),
            linetype="dotdash",
            size=1.2)+
  geom_line(data=pr_test, aes(x=x,y=y,color="Test data set"),
            linetype="dotted",
            size=1.2)+
  scale_color_manual(breaks=c("Train data set","Test data set","K-fold curves","K-fold mean"),
                     values=cbPalette)+
  geom_hline(yintercept=prior$prior_above,linetype="dashed",color="black",size=1)+
  scale_fill_manual(values="grey")

ggsave(paste0("PR_",target,".png"),plot=last_plot(),height=6,width=7,units="in",dpi=600) #save PR output to working directory

#### Query probability distribution -----------------------------------------------

#Calculate each conditional probability table (CPT) iteratively and store in an empty matrix
df=train_complete_disc_select
cpt=matrix(nrow=6,ncol=ncol(df)) #define empty conditional probability table
colnames(cpt)<-names(df)
i=1
cpt_list <- vector(mode='list', length=3)
repeat {
  for (column in seq(1:ncol(df))){
    for (level in seq(1:nlevels(factor(df[,column])))) {
      column.name=names(df)[column]
      factor=factor(df[,column])
      string=paste("(",column.name,"== '",levels(factor)[level],"')",sep = "") #dynamically build evidence string for each level of each factor
      string2=paste0("(",as.name("target")," == 1)") #set target
      cmd=paste("cpquery(fitted, ", string2, ", ", string, ")", sep = "")#query conditional probability for P of target given evidence string
      cpt[level,column]=eval(parse(text = cmd)) #save each value in the corresponding cell of the matrix
    }
  }
  cpt_list[[i]]=cpt
  i=i+1
  if(i>100) {break} #repeat the loop 100 times
}
cpt.mean<-apply(array(unlist(cpt_list), c(dim(cpt_list[[1]]), length(cpt_list))), 1:2, mean, na.rm = TRUE) #calculate average posterior probability from 100 iterations
colnames(cpt.mean)<-names(df)
cpt.mean #Print average conditional probabilities

#create an index for the CPT query
index=matrix(nrow=6,ncol=ncol(df)) #define empty conditional probability table
colnames(index)<-names(df)
for (column in seq(1:ncol(df))){
  for (level in seq(1:nlevels(factor(df[,column])))) {
    factor=factor(df[,column])
    index[level,column]=levels(factor)[level] #save each value in the corresponding cell of the matrix
  }
}
index

#### Plot tornado chart of significant variables  ---------------------------------
#prepare CPT data for plotting
cpt.gather<-cpt.mean%>%
  as.data.frame(.)%>%
  gather(key="variable",value="probability")

index.gather<-index%>%
  as.data.frame(.)%>%
  gather(key="variable",value="level")

cpt.plotdata<-cpt.gather%>%
  cbind(index.gather$level)%>%
  filter(is.na(probability)==FALSE)
colnames(cpt.plotdata)<-c("variable","probability","level")

write.csv(cpt.plotdata,paste0("cpt.plotdata_",target,".csv")) 

tornado.labeldata<-cpt.plotdata%>%
  group_by(variable)%>%
  summarise(min=min(probability),
            max=max(probability),
            across())%>%
  filter(probability %in% min | probability %in% max)%>%
  filter(variable!="target")

write.csv(tornado.labeldata,paste0("tornado.labeldata_",target,".csv"))

#tornado plot
tornado.chart<-cpt.plotdata%>%
  filter(variable!="target")%>%
  group_by(variable)%>%
  summarise(min=min(probability),
            max=max(probability),
            range=max-min)%>%
  ggplot(aes(y=reorder(variable,range)))+
  geom_linerange(aes(xmin=min,xmax=max),
                 color="black",
                 size=.9)+
  geom_vline(xintercept = prior$prior_above,
             linetype="dashed")+
  scale_x_continuous(limits=c(0,1),expand=c(0,0))+
  geom_point(aes(x=min,y=variable),
             shape=21,
             color="black",
             fill="blue",
             size=3)+
  geom_point(aes(x=max,y=variable),
             shape=21,
             color="black",
             fill="red",
             size=3)+
  geom_rect(aes(xmin=0,xmax=prior$prior_above,ymin=-Inf,ymax=Inf),
            alpha=0.01,fill="blue")+
  geom_rect(aes(xmin=prior$prior_above,xmax=1,ymin=-Inf,ymax=Inf),
            alpha=0.01,fill="red")+
  labs(x="Posterior probability",y="Predictor variable",title=paste(target))+
  theme_bw()+
  geom_text_repel(data=tornado.labeldata,aes(x=probability,y=variable,label=level),
                  size=2.5)
tornado.chart

### Evaluate performance on test set compared to alternative heuristics --------------------

improvement<-function(list){
  test.index<-list[["test"]]
  model.test.set<-na.omit(train_complete_disc_select[test.index,])
  model.train.set<-na.omit(train_complete_disc_select[-test.index,])
  heuristic.test.set<-na.omit(train_data_complete[test.index,])
  fitted<-list[["fitted"]]
  observed<-list[["observed"]]
  pred_probs<-predict(fitted,model.test.set,prob=TRUE)
  probs<-data.frame(t(attributes(pred_probs)$prob))
  pred_obs_sidebyside<-data.frame(probs$X1,observed)%>%filter(is.nan(probs.X1)==FALSE) #remove any NaN results from predicted probabilities
  pred_class<-prediction(pred_obs_sidebyside$probs.X1,pred_obs_sidebyside$observed)
  perf_class<-performance(pred_class,"tpr","fpr")
  tot_pos<-pred_class@n.pos[[1]]
  tot_neg<-pred_class@n.neg[[1]]
  cutoffs<-pred_class@cutoffs[[1]]
  tp<-pred_class@tp[[1]] #model true positives
  tn<-pred_class@tn[[1]] #model true negatives
  fp<-pred_class@fp[[1]] #model false positives
  fn<-pred_class@fn[[1]] #model false negatives
  npos<-pred_class@n.pos[[1]] #number of positives
  nneg<-pred_class@n.neg[[1]] #number of negatives
  accuracy<-(tp+tn)/(tp+tn+fp+fn)
  precision<-tp/(tp+fp)
  sensitivity<-tp/npos
  specificity<-tn/nneg
  accuracy.df<-as.data.frame(cbind(cutoffs,tp,tn,fp,fn,npos,nneg,accuracy,precision,sensitivity,specificity))
  accuracy.df<-accuracy.df%>%
    filter(is.infinite(cutoffs)==FALSE)%>%
    mutate(pred.pos=tp+fp, #model predicted positives, number of samples needed to achieve the shown sensitivity
           F1.score=2*((precision*sensitivity)/(precision+sensitivity)),
           F2.score=5*((precision*sensitivity)/((4*precision)+sensitivity)),
           fdr=1-precision)
  ####
  groundwater<-heuristic.test.set%>% #calculate prior probability of target
    filter(type_binary=="GW")%>%
    summarise(true.pos=sum(.data[["target"]]==1),
              pred.pos=n(),
              prec=true.pos/pred.pos,
              sens=true.pos/tot_pos,
              fdr=1-prec, #false discovery rate (FDR)
              F1=2*((prec*sens)/(prec+sens)),
              F2=5*((prec*sens)/((4*prec)+sens)))
  ####
  age<-heuristic.test.set%>% #calculate prior probability of target
    filter(BUILT_cat=="pre1988")%>%
    summarise(sens=sum(.data[["target"]]==1)/tot_pos,
              true.pos=sum(.data[["target"]]==1),
              pred.pos=n(),
              prec=true.pos/pred.pos,
              fdr=1-prec, #false discovery rate (FDR)
              F1=2*((prec*sens)/(prec+sens)),
              F2=5*((prec*sens)/((4*prec)+sens)))
  ####
  private<-heuristic.test.set%>% #calculate prior probability of target
    filter(private_well==1)%>%
    summarise(sens=sum(.data[["target"]]==1)/tot_pos,
              true.pos=sum(.data[["target"]]==1),
              pred.pos=n(),
              prec=true.pos/pred.pos,
              fdr=1-prec, #false discovery rate (FDR)
              F1=2*((prec*sens)/(prec+sens)),
              F2=5*((prec*sens)/((4*prec)+sens)))
  ####
  headstart<-heuristic.test.set%>% #calculate prior probability of target
    filter(head_start==1)%>%
    summarise(sens=sum(.data[["target"]]==1)/tot_pos,
              true.pos=sum(.data[["target"]]==1),
              pred.pos=n(),
              prec=true.pos/pred.pos,
              fdr=1-prec, #false discovery rate (FDR)
              F1=2*((prec*sens)/(prec+sens)),
              F2=5*((prec*sens)/((4*prec)+sens)))
  heuristics.df<-as.data.frame(rbind(groundwater,age,headstart,private))
  heuristics.labs=c("Groundwater","Pre-1988 buildings","Head Start","Private well water")
  heuristics.df<-cbind(heuristics.labs,heuristics.df)
  #Compare model performance to alternatives
  #GW
  n.eq.gw<-accuracy.df[which.min(abs(accuracy.df$sensitivity-groundwater$sens)),]$pred.pos
  sens.eq.gw<-accuracy.df[which.min(abs(accuracy.df$pred.pos-groundwater$pred.pos)),]$sensitivity
  prec.eq.gw<-accuracy.df[which.min(abs(accuracy.df$pred.pos-groundwater$pred.pos)),]$precision
  fdr.eq.gw<-accuracy.df[which.min(abs(accuracy.df$pred.pos-groundwater$pred.pos)),]$fdr
  #Age
  n.eq.age<-accuracy.df[which.min(abs(accuracy.df$sensitivity-age$sens)),]$pred.pos
  sens.eq.age<-accuracy.df[which.min(abs(accuracy.df$pred.pos-age$pred.pos)),]$sensitivity
  prec.eq.age<-accuracy.df[which.min(abs(accuracy.df$pred.pos-age$pred.pos)),]$precision
  fdr.eq.age<-accuracy.df[which.min(abs(accuracy.df$pred.pos-age$pred.pos)),]$fdr
  #Headstart
  n.eq.headstart<-accuracy.df[which.min(abs(accuracy.df$sensitivity-headstart$sens)),]$pred.pos
  sens.eq.headstart<-accuracy.df[which.min(abs(accuracy.df$pred.pos-headstart$pred.pos)),]$sensitivity
  prec.eq.headstart<-accuracy.df[which.min(abs(accuracy.df$pred.pos-headstart$pred.pos)),]$precision
  fdr.eq.headstart<-accuracy.df[which.min(abs(accuracy.df$pred.pos-headstart$pred.pos)),]$fdr
  #Private
  n.eq.private<-accuracy.df[which.min(abs(accuracy.df$sensitivity-private$sens)),]$pred.pos
  sens.eq.private<-accuracy.df[which.min(abs(accuracy.df$pred.pos-private$pred.pos)),]$sensitivity
  prec.eq.private<-accuracy.df[which.min(abs(accuracy.df$pred.pos-private$pred.pos)),]$precision
  fdr.eq.private<-accuracy.df[which.min(abs(accuracy.df$pred.pos-private$pred.pos)),]$fdr
  #Summarise
  n.eq.df<-data.frame(heuristics.labs=c("Groundwater","Pre-1988 buildings","Head Start","Private well water"),
                      model_pred.pos.eq=c(n.eq.gw,n.eq.age,n.eq.headstart,n.eq.private),
                      model_sens.eq=c(sens.eq.gw,sens.eq.age,sens.eq.headstart,sens.eq.private),
                      model_prec.eq=c(prec.eq.gw,prec.eq.age,prec.eq.headstart,prec.eq.private),
                      model_fdr.eq=c(fdr.eq.gw,fdr.eq.age,fdr.eq.headstart,fdr.eq.private))
  improvement.df<-heuristics.df%>%
    merge(y=n.eq.df,by="heuristics.labs",all.x=TRUE)%>%
    mutate(perc_reduce_n=(pred.pos-model_pred.pos.eq)/pred.pos,
           perc_improve_sens=(model_sens.eq-sens)/sens,
           model_F1=max(accuracy.df$F1.score,na.rm=TRUE),
           model_F2=max(accuracy.df$F2.score,na.rm=TRUE),
           perc_improve_F1=(model_F1-F1)/F1,
           perc_improve_F2=(model_F2-F2)/F2)
} #Function to calculate performance of each alternative heuristic compared to BN model for each K-fold

kfold.improvement.list<-lapply(kfold,improvement)
kfold.improvement.df<-map_df(kfold.improvement.list, ~as.data.frame(.x), .id="fold")

kfold.improvement.summary<-kfold.improvement.df%>%
  group_by(heuristics.labs)%>%
  filter(true.pos!=0)%>%
  summarise(mean_sens=mean(sens),
            upper_sens=quantile(sens,0.975),
            lower_sens=quantile(sens,0.025),
            mean_model_sens.eq=mean(model_sens.eq),
            mean_pred.pos=mean(pred.pos),
            upper_pred.pos=quantile(pred.pos,0.975),
            lower_pred.pos=quantile(pred.pos,0.025),
            mean_model_pred.pos.eq=mean(model_pred.pos.eq),
            mean_perc_improve_sens=(mean_model_sens.eq-mean_sens)/mean_sens,
            mean_perc_reduce_n=(mean_pred.pos-mean_model_pred.pos.eq)/mean_pred.pos,
            mean_F1=mean(F1,na.rm=TRUE),
            mean_F2=mean(F2,na.rm=TRUE),
            mean_model_F1=mean(model_F1),
            mean_model_F2=mean(model_F2),
            mean_perc_improve_F1=(mean_model_F1-mean_F1)/mean_F1,
            mean_perc_improve_F2=(mean_model_F2-mean_F2)/mean_F2)
kfold.improvement.summary

write.csv(kfold.improvement.summary,paste0("improvement_data_",target,".csv"))

plot.improvement<-function(list){
  test.index<-list[["test"]]
  model.test.set<-na.omit(train_complete_disc_select[test.index,])
  model.train.set<-na.omit(train_complete_disc_select[-test.index,])
  heuristic.test.set<-na.omit(train_data_complete[test.index,])
  fitted<-list[["fitted"]]
  observed<-list[["observed"]]
  pred_probs<-predict(fitted,model.test.set,prob=TRUE)
  probs<-data.frame(t(attributes(pred_probs)$prob))
  pred_obs_sidebyside<-data.frame(probs$X1,observed)%>%filter(is.nan(probs.X1)==FALSE) #remove any NaN results from predicted probabilities
  pred_class<-prediction(pred_obs_sidebyside$probs.X1,pred_obs_sidebyside$observed)
  tot_pos<-pred_class@n.pos[[1]]
  tot_neg<-pred_class@n.neg[[1]]
  cutoffs<-pred_class@cutoffs[[1]]
  tp<-pred_class@tp[[1]] #model true positives
  tn<-pred_class@tn[[1]] #model true negatives
  fp<-pred_class@fp[[1]] #model false positives
  fn<-pred_class@fn[[1]] #model false negatives
  npos<-pred_class@n.pos[[1]] #number of positives
  nneg<-pred_class@n.neg[[1]] #number of negatives
  accuracy<-(tp+tn)/(tp+tn+fp+fn)
  precision<-tp/(tp+fp)
  sensitivity<-tp/npos
  specificity<-tn/nneg
  accuracy.df<-as.data.frame(cbind(cutoffs,tp,tn,fp,fn,npos,nneg,accuracy,precision,sensitivity,specificity))
  accuracy.df<-accuracy.df%>%
    filter(is.infinite(cutoffs)==FALSE)%>%
    mutate(pred.pos=tp+fp, #model predicted positives, number of samples needed to achieve the shown sensitivity
           F1.score=2*((precision*sensitivity)/(precision+sensitivity)),
           F2.score=5*((precision*sensitivity)/((4*precision)+sensitivity)),
           fdr=1-precision)
}

kfold.plot.improvement.list<-lapply(kfold,plot.improvement)
kfold.plot.improvement.df<-map_df(kfold.plot.improvement.list, ~as.data.frame(.x), .id="fold")

pr.values.kfold.list<-lapply(kfold,plot.pr.kfold) #compile fpr and tpr results for each fold
pr.values.kfold.df <- map_df(pr.values.kfold.list, ~as.data.frame(.x), .id="fold") #concatenate fpr and tpr results for plotting

improvement.values.average<-kfold.plot.improvement.df%>% #calculate average value across all folds
  mutate(bin_sens=cut(sensitivity,breaks=100))%>%
  group_by(bin_sens)%>%
  summarise(mean_sens=mean(sensitivity),
            mean_pred.pos=mean(pred.pos),
            sd_pred.pos=sd(pred.pos),
            n=n(),
            upper=mean_pred.pos+1.96*(sd_pred.pos/sqrt(n())), #calculate 95% confidence interval around the mean
            lower=mean_pred.pos-1.96*(sd_pred.pos/sqrt(n())))

#Plot improvement chart
ggplot()+
  geom_line(data=kfold.plot.improvement.df,
           aes(x=sensitivity,y=pred.pos,
               size="K-fold curves",
               group=fold),
           alpha=0.6,
           color="lightgrey")+
  geom_smooth(data=improvement.values.average,
              aes(x=mean_sens,y=mean_pred.pos,
                  size="K-fold mean"),
              color="darkgrey",
              se=FALSE)+
  geom_point(data=kfold.improvement.summary,
             aes(x=mean_sens,y=mean_pred.pos,fill=heuristics.labs),
             color="black",
             shape=21,
             alpha=0.4,
             size=4)+
  #geom_rect(data=kfold.improvement.summary,
  #              aes(xmin=lower_sens,xmax=upper_sens,
  #                  ymin=lower_pred.pos,ymax=upper_pred.pos,
  #                  color=heuristics.labs),
  #          fill=NA,
  #          linetype="dotted")+
  geom_segment(data=kfold.improvement.summary,
               aes(x=mean_sens,xend=mean_model_sens.eq,
                   y=mean_pred.pos,yend=mean_pred.pos,
                   color=heuristics.labs))+
  geom_segment(data=kfold.improvement.summary,
               aes(x=mean_sens,xend=mean_sens,
                   y=mean_pred.pos,yend=mean_model_pred.pos.eq,
                   color=heuristics.labs))+
  geom_point(data=kfold.improvement.summary,
             aes(x=mean_sens,y=mean_model_pred.pos.eq,
                 fill=heuristics.labs),
             color="black",
             shape=21)+
  geom_point(data=kfold.improvement.summary,
             aes(x=mean_model_sens.eq,y=mean_pred.pos,
                 fill=heuristics.labs),
             color="black",
             shape=21)+
  geom_text(data=kfold.improvement.summary,
            aes(x=(mean_model_sens.eq+mean_sens)/1.9,y=mean_pred.pos+3.5,label=paste0(round(mean_perc_improve_sens*100),"%"),color=heuristics.labs),
            size=2,nudge_y=0.01)+
  geom_text(data=kfold.improvement.summary,
            aes(y=(mean_model_pred.pos.eq+mean_pred.pos)/2.05,x=mean_sens-0.013,label=paste0(round(mean_perc_reduce_n*100),"%"),color=heuristics.labs),
            size=2)+
  geom_text_repel(data=kfold.improvement.summary,
                  aes(x=mean_sens,y=mean_pred.pos,
                      label=heuristics.labs),
                  box.padding=1,
                  size=2.8,nudge_y=15)+
  theme_bw()+
  theme(legend.position=c(0.2,0.75),
        axis.title=element_text(face="bold"),
        legend.title=element_text(face="bold"),
        legend.background = element_rect(fill="white",color="black"))+
  labs(x="True positive rate (sensitivity)",y="N predicted positive",size="BN model performance",
       fill="Heuristic performance",title=paste(target))+
  guides(color="none")+
  scale_x_continuous(expand=c(0,0.01),limits = c(0,1))+
  scale_y_continuous(expand=c(0,30))+
  scale_color_manual(values=cbPalette2)+
  scale_fill_manual(values=cbPalette2)+
  scale_size_manual(values=c(0.5,1.5),labels=c("K-fold curves","K-fold mean"))

ggsave(paste0("improvement_",target,".png"),plot=last_plot(),height=6,width=7,units="in",dpi=600) #save improvement plot to working directory

#Generate Performance summary
performance_summary<-data.frame(target=target,
                                prior=prior$prior_above,
                                auroc_train=auc_train,
                                auroc_test=auc_test,
                                auroc_kfold=mean.auc,
                                aupr_train=aucpr_train,
                                aupr_test=aucpr_test,
                                aupr_kfold=mean.aucpr)

performance_summary #print performance summary

write.csv(performance_summary,paste0("performance_summary_",target,".csv"))
