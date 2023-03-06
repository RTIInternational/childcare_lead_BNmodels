#BAYESIAN NETWORK MODELS TO PREDICT WATER LEAD RISK IN NORTH CAROLINA CHILD CARE FACILITIES

#Variable selection strategy: Chi-squared tests

#Author: Riley E. Mulhern, PhD <rmulhern@rti.org>

#Date: February 16, 2023

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
library(ggmice) #plot patterns of missing data
library(zoo) #smooth improvement fit

cbPalette <- c("#E69F00","#56B4E9","#999999","#009E73", "#D55E00", "#CC79A7","#0072B2","#F0E442") #color blind color palette
cbPalette2 <- c("#E69F00","#56B4E9","#009E73","#D55E00","#0072B2","#F0E442","#CC79A7","#999999")

#### Load data -----------------------------------------------------------------

#Get working directory
getwd() #This is where the outputs from this script will be saved. To set a different working directory use setwd().

#Import data set
data<-read.csv("https://github.com/RTIInternational/childcare_lead_BNmodels/raw/main/data/Mulhern%20et%20al_Improved%20decision%20making%20for%20water%20lead%20testing_DATA.csv")

#Define numeric model variables
num_vars<-c("PER_FREE",
               "PER_NON_WHITE",
               "TOTAL_ENROLL",
               "nsamples",
               "perc_filtered",
               "perc_non_white_cbg",
               "perc_hs_higher_cbg",
               "med_hh_income_cbg")

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
                "any_lcr_exceedance",
                "type_binary",
                "ph_binary",
                "Phos_binary",
                "chloramines",
                "coagulation",
                "connections_cat",
                "ruca_cat")

#### Set seed and target node --------------------------------------------------
set.seed(32)
target<-"maxabove1" #Options = maxabove1, maxabove5, maxabove10, maxabove15, perc90above1, perc90above5, perc90above10, perc90above15
data<-data%>%
  select(all_of(c(target,num_vars,cat_vars)))%>% #select only target node and predictor variables
  mutate(across(all_of(c(target,cat_vars)),factor))%>% #define target and categorical variables as factors
  rename(target=paste(target))

#### Split into test and train data sets ---------------------------------------
data$id<-1:nrow(data)

train_data<-data%>%
  sample_frac(0.80) #retain 80% of data set for training, 20% for testing

test_data<-anti_join(data,train_data,by="id")

#### Discretize data for machine learning -----------------------------------------
    #supervised discretization of continuous variables

#Find best cutpoints among complete cases for each numeric variable independently using only the training set
cutpoints_list<-list() #create an empty list
for (num_var in num_vars) {
  disc<-train_data%>%
    select(all_of(c(num_var,"target")))%>%
    mutate(blank=0)%>% #add in a blank variable to ensure the random forest function in ForestDisc continues to read the data as a data frame not a vector when discretizing a single varibale at a time
    filter(complete.cases(.))%>% #discretization algorithm only considers complete cases of each variable at a time for selecting cutpoints
    #mutate(num_var=as.numeric(num_var))%>%
    ForestDisc(data=.,id_target=2,max_splits=2,opt_meth = "SLSQP")
  cutpoints=disc[["listcutp"]]
  cutpoints_list[num_var]=cutpoints
}

#Apply cutpoints to numeric variables in training set
train_data_disc<-train_data#%>%select(!id)
for (num_var in num_vars) {
  train_data_disc[,num_var]=cut(train_data_disc[,num_var],breaks=c(-Inf,cutpoints_list[[num_var]],Inf))
}

#re-categorize LCR variable based on whether the facility was served by a private well,
#in which case LCR data is not applicable and a value should not be imputed
train_data_disc<-train_data_disc%>%
  mutate(any_lcr_exceedance=as.character(any_lcr_exceedance),
         #n_lcr_exceedances=as.character(n_lcr_exceedances),
         any_lcr_exceedance=factor(ifelse(private_well==1,"lcr_na",any_lcr_exceedance)))
         #n_lcr_exceedances=factor(ifelse(private_well==1,"lcr_na",n_lcr_exceedances)))

#Apply cutpoints identified for the training set to the test set (required step for BN models)
test_data_disc<-test_data#%>%select(!id)
for (num_var in num_vars) {
  test_data_disc[,num_var]=cut(test_data_disc[,num_var],breaks=c(-Inf,cutpoints_list[[num_var]],Inf))
}

#re-categorize LCR variables for test data set as above
test_data_disc<-test_data_disc%>%
  mutate(any_lcr_exceedance=as.character(any_lcr_exceedance),
         #n_lcr_exceedances=as.character(n_lcr_exceedances),
         any_lcr_exceedance=factor(ifelse(private_well==1,"lcr_na",any_lcr_exceedance)))
         #n_lcr_exceedances=factor(ifelse(private_well==1,"lcr_na",n_lcr_exceedances)))


#### Learn BN structure --------------------------------------------------------
# plot network function - from https://www.r-bloggers.com/2018/09/bayesian-network-example-with-the-bnlearn-package/
plot.network <- function(structure, color, ht = "600px"){
  if(missing(color)){color="darkturquoise"}
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
train_data_disc<-train_data_disc%>%select(!id)
bn<-tree.bayes(train_data_disc, "target")
plot.network(bn,"darkturquoise")

#Evaluate strength of arcs using selected variable selection method, select only nodes with significant arcs connected to target
strengths.x2<-arc.strength(bn,data=train_data_disc,criterion = "x2") #Pearson's chi-squared criterion
sig_arcs.x2<-strengths.x2%>%
  filter(from=="target")%>%
  filter(strength<0.05) #select only arcs that are statistically significant, p<0.05

png(paste0("strength_plot_x2_",target,".png"),width=14,height=9,units="in",res=600)
strength.plot(bn,strengths.x2,shape="rectangle",fontsize = 16,layout="dot",main=target) #plot arc strengths
dev.off()

sig_vars.x2<-c(sig_arcs.x2$to,"target") #list significant variable names
write.csv(sig_vars.x2,paste0("sigvars.x2_",target,".csv"))
length(sig_vars.x2) #number of significant predictors for target

train_data_select.x2<-train_data_disc%>% #compile new training data set with only significant variables
  dplyr::select(all_of(c(sig_vars.x2)))

test_data_select.x2<-test_data_disc%>% #compile new testing data set with only significant variables
  dplyr::select(all_of(c(sig_vars.x2)))

#Re-learn Bayesian network structure with significant nodes only
bn.sig<-tree.bayes(train_data_select.x2,"target")
plot.network(bn.sig,"darkturquoise")
write.csv(bn.sig$arcs,paste0("bn_arcs_x2_",target,".csv"))

#### Evaluate BN performance ---------------------------------------------------
#From this point forward, the final training and testing data are renamed as:
train<-train_data_select.x2
test<-test_data_select.x2
#Define functions to calculate AUC and AUC-PR

#bn.predict -- This function takes the user-specified training and test data sets
#to calculate the predicted probabilities of the target from the specified network structure.
#An integrated Bayesian LW imputation function fills in missing values of the testing data set before
#the prediction based on the structure of the network. The "impute" function requires
#that the target variable be present in the test set. To avoid data leakage during model
#performance evaluation and to simulate a practical use case where the target variable is not known,
#the test set is stripped of the real target result and  a random starting guess for the target
#outcome is filled in. This guess is only used for imputation and then is removed again prior to prediction.
bn.predict<-function(network, train_data, test_data){
  fitted<-bn.fit(network, train_data, method = "bayes") #compute complete conditional probability table from the network
  ##### Impute missing
  test_data_rm_target<-test_data%>%
    select(!target)%>% #hide real result from imputation process
    mutate(target=factor(sample(c(0,1),nrow(test_data),replace=TRUE))) #guess a random starting result for the target node
  test_data_imputed<-impute(fitted,test_data_rm_target)%>%
    select(!target) #remove initial random guess from imputed data set
  #####
  pred_probs<-predict(fitted, test_data_imputed, prob=TRUE) #predict probabilities of target node given network
  probs<-data.frame(t(attributes(pred_probs)$prob))
  realresult<-test_data%>%dplyr::select(all_of("target"))
  pred_class<-prediction(probs$X1,realresult)
  return(pred_class)
}

#bn.perf -- This function uses the output of bn.predict to plot basic ROC and PR curve and prints the AUC values for each.
#It also stores new variables in the global environment with the ROC and PR data
#called roc_name and pr_name (which are the complete data for more advanced plotting)
#and auc_name and aucpr_name (which are the AUC values of the ROC and PR curves)
#where 'name' corresponds to the 'output_name' field specified in the function call.
#Use 'output_name' to define which data set the performance calculations are being run for (i.e., the training or test set).
bn.perf<-function (network,train_data,test_data,output_name){
  fitted<-bn.fit(network, train_data, method = "bayes") #compute complete conditional probability table from the network
  pred_class<-bn.predict(network,train_data,test_data)
  assign(paste0("fitted_",output_name),fitted,envir=.GlobalEnv)
  perf_class_roc<-performance(pred_class,"tpr","fpr")
  perf_class_pr<-performance(pred_class,"ppv","tpr")
  plot(perf_class_pr) #plot PR curve
  plot(perf_class_roc) #plot ROC curve
  auc<-as.numeric(performance(pred_class, "auc")@y.values) #calculate area under the ROC curve
  aucpr<-as.numeric(performance(pred_class,"aucpr")@y.values) #calculate area under the PR curve
  roc.dat<-data.frame(x=perf_class_roc@x.values,
                      y=perf_class_roc@y.values)
  colnames(roc.dat)<-c("x","y")
  pr.dat<-data.frame(x=perf_class_pr@x.values,
                     y=perf_class_pr@y.values)
  colnames(pr.dat)<-c("x","y")
  assign(paste0("roc_",output_name),roc.dat,envir=.GlobalEnv)
  assign(paste0("pr_",output_name),pr.dat,envir=.GlobalEnv)
  assign(paste0("auc_",output_name),auc,envir=.GlobalEnv)
  assign(paste0("aucpr_",output_name),aucpr,envir=.GlobalEnv)
  print(data.frame(metric=c("auc-roc","auc-pr"),
                   result=c(auc,aucpr)))
}

#Test performance of network internally on the training set
bn.perf(network=bn.sig,train_data=train,test_data=train,output_name = "train")

#Test performance of network externally on the test set
bn.perf(network=bn.sig,train_data=train,test_data=test,output_name = "test")

#K-fold cross-validation
k=10 #use 10 folds
kfold<-bn.cv(data=train,
             bn="tree.bayes",
             method="k-fold",
             k=k,
             runs=1,
             loss="pred", #select loss function, Classification Error (pred): the prediction error for a single node in a discrete network.
             algorithm.args = list(training="target"))
names(kfold)<-c(1:k) #assign each element of k-fold list output the corresponding fold number

#Generate ROC curves for each fold
calc.roc.kfold<-function(list){
  test.index<-list[["test"]]
  test_data<-train[test.index,]
  train_data<-train[-test.index,]
  network=bn.sig
  fitted<-bn.fit(network, train_data, method = "bayes") #compute complete conditional probability table from the network
  pred_class<-bn.predict(network,train_data,test_data)
  perf_class<-performance(pred_class,"tpr","fpr")
  auc<-as.numeric(performance(pred_class, "auc")@y.values) #calculate area under the ROC curve
  return(auc)
} #function to calculate average AUC from k-fold cross validation

kfold.auc<-sapply(kfold,calc.roc.kfold)
kfold.auc #AU-ROC value from each fold
mean.auc<-mean(kfold.auc)
mean.auc #mean AU-ROC value
max.auc<-max(kfold.auc)
max.auc #max
min.auc<-min(kfold.auc)
min.auc

#Compare all ROC performance curves

#function to return fpr and tpr for each fold
plot.roc.kfold<-function(list){
  test.index<-list[["test"]]
  test_data<-train[test.index,]
  train_data<-train[-test.index,]
  network=bn.sig
  fitted<-bn.fit(network, train_data, method = "bayes") #compute complete conditional probability table from the network
  pred_class<-bn.predict(network,train_data,test_data)
  perf_class<-performance(pred_class,"tpr","fpr")
  x<-perf_class@x.values
  y<-perf_class@y.values
  roc.values<-data.frame(x,y)
  colnames(roc.values)<-c("fpr","tpr")
  return(roc.values)
}

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
            linewidth=1.2)+
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

ggsave(paste0("ROC_x2_",target,".png"),plot=last_plot(),height=6,width=7,units="in",dpi=600) #save ROC output to working directory

#Area under Precision-Recall curve (better for imbalanced data sets)
calc.pr.kfold<-function(list){
  test.index<-list[["test"]]
  test_data<-train[test.index,]
  train_data<-train[-test.index,]
  network=bn.sig
  fitted<-bn.fit(network, train_data, method = "bayes") #compute complete conditional probability table from the network
  pred_class<-bn.predict(network,train_data,test_data)
  perf_class<-performance(pred_class,"tpr","fpr")
  aucpr<-as.numeric(performance(pred_class, "aucpr")@y.values) #calculate area under the ROC curve
  return(aucpr)
} #function to calculate average AUC from k-fold cross validation

kfold.aucpr<-sapply(kfold,calc.pr.kfold)
kfold.aucpr #Au-PR value from each fold
mean.aucpr<-mean(kfold.aucpr)
mean.aucpr #mean AU-PR value
max.aucpr<-max(kfold.aucpr)
max.aucpr #max
min.aucpr<-min(kfold.aucpr)
min.aucpr #min

plot.pr.kfold<-function(list){
  test.index<-list[["test"]]
  test_data<-train[test.index,]
  train_data<-train[-test.index,]
  network=bn.sig
  fitted<-bn.fit(network, train_data, method = "bayes") #compute complete conditional probability table from the network
  pred_class<-bn.predict(network,train_data,test_data)
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

prior<-train%>% #calculate prior probability of the target
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
              linewidth=1.2,se=FALSE)+
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

ggsave(paste0("PR_x2_",target,".png"),plot=last_plot(),height=6,width=7,units="in",dpi=600) #save PR output to working directory

#### Query probability distribution -----------------------------------------------

#Calculate each conditional probability table (CPT) iteratively and store in an empty matrix
cpt=matrix(nrow=6,ncol=ncol(train)) #define empty conditional probability table
colnames(cpt)<-names(train)
i=1
cpt_list <- vector(mode='list', length=3)
repeat {
  for (column in seq(1:ncol(train))){
    for (level in seq(1:nlevels(factor(train[,column])))) {
      column.name=names(train)[column]
      factor=factor(train[,column])
      string=paste("(",column.name,"== '",levels(factor)[level],"')",sep = "") #dynamically build evidence string for each level of each factor
      string2=paste0("(",as.name("target")," == 1)") #set target
      cmd=paste("cpquery(fitted_train, ", string2, ", ", string, ")", sep = "")#query conditional probability for P of target given evidence string
      cpt[level,column]=eval(parse(text = cmd)) #save each value in the corresponding cell of the matrix
    }
  }
  cpt_list[[i]]=cpt
  i=i+1
  if(i>100) {break} #repeat the loop 100 times
}
cpt.mean<-apply(array(unlist(cpt_list), c(dim(cpt_list[[1]]), length(cpt_list))), 1:2, mean, na.rm = TRUE) #calculate average posterior probability from 100 iterations
colnames(cpt.mean)<-names(train)
cpt.mean #Print average conditional probabilities

#create an index for the CPT query
index=matrix(nrow=6,ncol=ncol(train)) #define empty conditional probability table
colnames(index)<-names(train)
for (column in seq(1:ncol(train))){
  for (level in seq(1:nlevels(factor(train[,column])))) {
    factor=factor(train[,column])
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

write.csv(cpt.plotdata,paste0("cpt.plotdata_x2_",target,".csv"))

tornado.labeldata<-cpt.plotdata%>%
  group_by(variable)%>%
  summarise(min=min(probability),
            max=max(probability),
            across())%>%
  filter(probability %in% min | probability %in% max)%>%
  filter(variable!="target")

write.csv(tornado.labeldata,paste0("tornado.labeldata_x2_",target,".csv"))

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
ggsave(paste0("tornado_x2_",target,".png"),plot=tornado.chart,height=6,width=7,units="in",dpi=600) #save tornado plot to working directory

### Evaluate performance on test set compared to alternative heuristics --------------------

#Sensitivity vs Predicted Positive curves for each fold
imprv.kfold.curves<-function(list){
  test.index<-list[["test"]]
  test_data<-train[test.index,]
  train_data<-train[-test.index,]
  network=bn.sig
  fitted<-bn.fit(network, train_data, method = "bayes") #compute complete conditional probability table from the network
  pred_class<-bn.predict(network,train_data,test_data)
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
imprv_kfold_curves_list<-lapply(kfold,imprv.kfold.curves)
imprv_kfold_curves_df<-map_df(imprv_kfold_curves_list, ~as.data.frame(.x), .id="fold")

#Average, smoothed Sensitivity vs Predicted Positive curve across all 10 folds
imprv_kfold_curves_avg<-imprv_kfold_curves_df%>% #calculate average value across all folds
  mutate(bin_sens=cut(sensitivity,breaks=seq(0,1,0.01)))%>%
  group_by(bin_sens)%>%
  summarise(mean_sens=mean(sensitivity),
            mean_pred.pos=mean(pred.pos),
            sd_pred.pos=sd(pred.pos),
            n=n(),
            upper=mean_pred.pos+1.96*(sd_pred.pos/sqrt(n())), #calculate 95% confidence interval around the mean
            lower=mean_pred.pos-1.96*(sd_pred.pos/sqrt(n())))%>%
  mutate(loess_fit_mean_pred.pos=loess(mean_pred.pos~mean_sens,data=.)[["fitted"]])

#Mean highest achievable F score by the model across all 10 folds
mean_max_model_Fscores<-imprv_kfold_curves_df%>%
  group_by(fold)%>%
  summarise(max_F1=max(F1.score,na.rm=TRUE),
            max_F2=max(F2.score,na.rm=TRUE))%>%
  ungroup()%>%
  summarise(F1=mean(max_F1),
            F2=mean(max_F2))

#Average Sensitivity and Predicted Positive for each heuristic for each fold
imprv.heuristic<-function(list){
  test.index<-list[["test"]]
  test_data<-train[test.index,]
  train_data<-train[-test.index,]
  heuristic_test_data<-train_data_disc[test.index,]
  network=bn.sig
  fitted<-bn.fit(network, train_data, method = "bayes") #compute complete conditional probability table from the network
  pred_class<-bn.predict(network,train_data,test_data)
  perf_class<-performance(pred_class,"tpr","fpr")
  tot_pos<-pred_class@n.pos[[1]]
  ####
  groundwater<-heuristic_test_data%>% #calculate prior probability of target
    filter(type_binary=="GW")%>%
    summarise(true.pos=sum(.data[["target"]]==1),
              pred.pos=n(),
              prec=true.pos/pred.pos,
              sens=true.pos/tot_pos,
              fdr=1-prec, #false discovery rate (FDR)
              F1=2*((prec*sens)/(prec+sens)),
              F2=5*((prec*sens)/((4*prec)+sens)))
  ####
  age<-heuristic_test_data%>% #calculate prior probability of target
    filter(BUILT_cat=="pre1988")%>%
    summarise(sens=sum(.data[["target"]]==1)/tot_pos,
              true.pos=sum(.data[["target"]]==1),
              pred.pos=n(),
              prec=true.pos/pred.pos,
              fdr=1-prec, #false discovery rate (FDR)
              F1=2*((prec*sens)/(prec+sens)),
              F2=5*((prec*sens)/((4*prec)+sens)))
  ####
  private<-heuristic_test_data%>% #calculate prior probability of target
    filter(private_well==1)%>%
    summarise(sens=sum(.data[["target"]]==1)/tot_pos,
              true.pos=sum(.data[["target"]]==1),
              pred.pos=n(),
              prec=true.pos/pred.pos,
              fdr=1-prec, #false discovery rate (FDR)
              F1=2*((prec*sens)/(prec+sens)),
              F2=5*((prec*sens)/((4*prec)+sens)))
  ####
  headstart<-heuristic_test_data%>% #calculate prior probability of target
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
} #Function to calculate performance of each alternative heuristic compared to BN model for each K-fold

imprv_heuristic_list<-lapply(kfold,imprv.heuristic)
imprv_heuristic_df<-map_df(imprv_heuristic_list, ~as.data.frame(.x), .id="fold")

#Summarize improvement calculations
#Difference from avg. Sensitivity and Predicted Positives for each heuristic compared to equivalent avg. Sensitivity and Predicted Positives from model
kfold.improvement.summary<-imprv_heuristic_df%>%
  group_by(heuristics.labs)%>%
  filter(true.pos!=0)%>%
  summarise(mean_sens=mean(sens),
            upper_sens=quantile(sens,0.975),
            lower_sens=quantile(sens,0.025),
            mean_pred.pos=mean(pred.pos),
            upper_pred.pos=quantile(pred.pos,0.975),
            lower_pred.pos=quantile(pred.pos,0.025),
            mean_F1=mean(F1,na.rm=TRUE),
            mean_F2=mean(F2,na.rm=TRUE),
            model_F1=mean_max_model_Fscores$F1,
            model_F2=mean_max_model_Fscores$F2,
            mean_perc_improve_F1=(model_F1-mean_F1)/mean_F1,
            mean_perc_improve_F2=(model_F2-mean_F2)/mean_F2,
            model_n.eq=imprv_kfold_curves_avg[which.min(abs(imprv_kfold_curves_avg$mean_sens-mean_sens)),]$loess_fit_mean_pred.pos,
            model_sens.eq=imprv_kfold_curves_avg[which.min(abs(imprv_kfold_curves_avg$loess_fit_mean_pred.pos-mean_pred.pos)),]$mean_sens,
            mean_perc_improve_sens=(model_sens.eq-mean_sens)/mean_sens,
            mean_perc_reduce_n=(mean_pred.pos-model_n.eq)/mean_pred.pos)

kfold.improvement.summary

write.csv(kfold.improvement.summary,paste0("improvement_data_x2_",target,".csv"))

#Plot improvement chart
ggplot()+
  geom_line(data=imprv_kfold_curves_df,
            aes(x=sensitivity,y=pred.pos,
                size="K-fold curves",
                group=fold),
            alpha=0.6,
            color="lightgrey")+
  geom_line(data=imprv_kfold_curves_avg,
            aes(x=mean_sens,y=loess_fit_mean_pred.pos,
                size="K-fold mean"),
            color="darkgrey")+
  geom_point(data=kfold.improvement.summary,
             aes(x=mean_sens,y=mean_pred.pos,fill=heuristics.labs),
             color="black",
             shape=21,
             alpha=0.4,
             size=4)+
  geom_segment(data=kfold.improvement.summary,
               aes(x=mean_sens,xend=model_sens.eq,
                   y=mean_pred.pos,yend=mean_pred.pos,
                   color=heuristics.labs))+
  geom_segment(data=kfold.improvement.summary,
               aes(x=mean_sens,xend=mean_sens,
                   y=mean_pred.pos,yend=model_n.eq,
                   color=heuristics.labs))+
  geom_point(data=kfold.improvement.summary,
             aes(x=mean_sens,y=model_n.eq,
                 fill=heuristics.labs),
             color="black",
             shape=21)+
  geom_point(data=kfold.improvement.summary,
             aes(x=model_sens.eq,y=mean_pred.pos,
                 fill=heuristics.labs),
             color="black",
             shape=21)+
  geom_text(data=kfold.improvement.summary,
            aes(x=(model_sens.eq+mean_sens)/1.9,y=mean_pred.pos+5,label=paste0(round(mean_perc_improve_sens*100),"%"),color=heuristics.labs),
            size=2,nudge_y=0.01)+
  geom_text(data=kfold.improvement.summary,
            aes(y=(model_n.eq+mean_pred.pos)/2.05,x=mean_sens-0.013,label=paste0(round(mean_perc_reduce_n*100),"%"),color=heuristics.labs),
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

ggsave(paste0("improvement_x2_",target,".png"),plot=last_plot(),height=6,width=7,units="in",dpi=600) #save improvement plot to working directory

#Generate Performance summary
performance_summary<-data.frame(target=target,
                                prior=prior$prior_above,
                                auroc_train=auc_train,
                                auroc_test=auc_test,
                                auroc_kfold=mean.auc,
                                auroc_kfold_max=max.auc,
                                auroc_kfold_min=min.auc,
                                aupr_train=aucpr_train,
                                aupr_test=aucpr_test,
                                aupr_kfold=mean.aucpr,
                                aupr_kfold_max=max.aucpr,
                                aupr_kfold_min=min.aucpr)

performance_summary #print performance summary

write.csv(performance_summary,paste0("performance_summary_x2_",target,".csv"))
