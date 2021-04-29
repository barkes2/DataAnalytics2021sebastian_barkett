#set directory
#reading in all three datasets
redwine<-read.csv("winequality-red.csv")
whitewine<-read.csv("winequality-white.csv")
CervicalData<-read.csv("CervicalData.csv")
Absent<-read.csv("Absenteeism_at_work.csv")
#wine datasets needed to be reformatted (csv->txt->change headers->csv) before
#R studio can interpret them correctly
head(redwine)
head(whitewine)
head(CervicalData)
#Absent csv needed to be reformatted to reflect semicolon usage (csv->txt->csv)
head(Absent)
#now that all datasets are read in lets attach them
attach(redwine)
attach(whitewine)
#masked headers for white wine?Lets look at it
attach(CervicalData)
attach(Absent)
#sort out masked headers for whitewine data-> this is because headers in both 
#datasets are the same you dingus. changing headers now
#Now that all the data is attached, lets get summaries of all three datasets
#redwine summary
summary(redwine)
#whitewine summary
summary(whitewine)
#CervicalData
summary(CervicalData)
#Absent summary
summary(Absent)
#First we need to establish our questions for each dataset and perform some EDA
View(redwine)
View(whitewine)
#EDA related to wine dataset(s) will involve examining the acidity relationship 
#of reds vs whites under the lens of pH, as well as any relationships that
#may exist between pH and volatile acidity, which wine experts may characterize
#through scent of the wine. Here I expect to see Whites are more acidic in pH
#and contain higher volatile acidity when compared to reds
head(whitewine)
View(CervicalData)
#examine cases for all 71 individuals who are positive (1) or negative (0) for 
#cervicalcancer and look for correlations between the values for sexual risk, 
#and personal hygiene to see if these are indicators of cervical cancer
View(Absent)
#Evaluate the average work load and absenteeism in hours for individuals with 
#postgraduate(3) and master or doctoral (4) educations (as one group) when 
#compared to (1) and (2) education individuals
#===============================================================================
#next lets evaluate for 0 or NA values and remove this data/create new subsets
#wine subsets only require the rows with non-zero values for w.volatile.acidity,
#r.volatile.acidity, & pH
#lets start with red
redwine$r.volatile.acidity[redwine$r.volatile.acidity==0]<-NA
redwineNZva<-redwine[complete.cases(redwine),]
View(redwineNZva)
#dont need this dataset as there were no cases of zero values in redwine for 
#r.volatile.acidity
#lets check pH
redwine$r.pH[redwine$r.pH==0]<-NA
redwineNZpH<-redwine[complete.cases(redwine),]
View(redwineNZpH)
#no cases of zero values for redwine pH here, dont need this
#lets check white wine
whitewine$w.volatile.acidity[whitewine$w.volatile.acidity==0]<-NA
whitewineNZva<-whitewine[complete.cases(whitewine),]
View(whitewineNZva)
#just occuring to me now that i can just sort the rows to find zero values for
#these parameters, WE ARE TOO DEEP IN IT NOW! lets just copy this to see if
#there are zero values for whitewine pH
whitewine$w.pH[whitewine$w.pH==0]<-NA
whitewineNZpH<-whitewine[complete.cases(whitewine),]
View(whitewineNZpH)
#okay, egg on my face. no zero values in any of the datasets, did notice theres
#a bagillion more samples for the white wine data than the red wine, but thats 
#whatever

#im in this mode now so im going to go look through the other two dataset csv 
#files for our variables that we need to not waste time. If i find zero values
#i will remove them here
#CervicalData (n=72) no zero values for behavior_sexualRisk or behavior_personal
#Hygine
#Absent (n=741) no zero values for average workload, but zero values for 
#Absenteeism time in hours <- removing now
Absent$Absenteeism.time.in.hours[Absent$Absenteeism.time.in.hours==0]<-NA
AbsentNZatih<-Absent[complete.cases(Absent),]
View(AbsentNZatih)
#Absenteeism time in hours went from n=740 to n=737 (3 rows removed)
#==============================================================================
#here are the datasets we will be using
AbsentNZatih
redwine
whitewine
CervicalData
#Next step is EDA
#plotting the ECDF
library(ggplot2)
library(gridExtra)
library(grid)
g<-ggplot(redwine, aes(x=r.pH,y=r.volatile.acidity))
g+stat_ecdf()
g2<-ggplot(whitewine, aes(x=w.pH,y=w.volatile.acidity))
g2+stat_ecdf()
g3<-ggplot(AbsentNZatih, aes(x=Work.load.Average.day,y=Absenteeism.time.in.hours))
g3+stat_ecdf()
g4<-ggplot(CervicalData, aes(x=behavior_sexualRisk,y=behavior_personalHygine))
g4+stat_ecdf()
grid.arrange(g+stat_ecdf(),g2+stat_ecdf(),g3+stat_ecdf(),g4+stat_ecdf(),top="ECDF for RedWine, WhiteWine, Absent, and CervicalData")
#Boxplots
par(mfrow=c(2,2))
boxplot(redwine$r.pH,redwine$r.volatile.acidity,ylab="(Left)-redwine pH; (Right)-redwine volatile acidity")
boxplot(whitewine$w.pH,whitewine$w.volatile.acidity,ylab="(Left)-whitewine pH; (Right)-whitewine volatile acidity")
boxplot(AbsentNZatih$Absenteeism.time.in.hours,AbsentNZatih$Work.load.Average.day, ylab="(Left) Absent Time Hours; (Right)-Average Work Load")
boxplot(CervicalData$behavior_sexualRisk, CervicalData$behavior_personalHygine, ylab="(Left)-Sexual Risk; (Right)-Personal Hygine")
#Absent dataset would benefit from individual boxplots for each education group

#Histograms
par(mfrow=c(2,2))
hist(redwine$r.pH,breaks=14,ylab="Count within Bin", xlab="redwine pH")
hist(redwine$r.volatile.acidity,breaks=14,ylab="Count within Bin", xlab="redwine volatile acidity")
hist(whitewine$w.pH,breaks=14,ylab="Count within Bin", xlab="whitewine pH")
hist(whitewine$w.volatile.acidity,breaks=14,ylab="Count within Bin", xlab="whitewine volatile acidity")
#Hist for other two datasets

par(mfrow=c(2,1))
hist(AbsentNZatih$Absenteeism.time.in.hours, breaks=15, ylab="Count within Bin", xlab="Absent Time (hours)")
hist(AbsentNZatih$Work.load.Average.day, breaks=14, ylab="Count within Bin", xlab="Average Work Load")

par(mfrow=c(2,1))
hist(CervicalData$behavior_sexualRisk, breaks=14, ylab="Count within Bin", xlab="Sexual Risk")
hist(CervicalData$behavior_personalHygine, breaks=14, ylab="Count within Bin", xlab="Personal Hygine")

#==============================================================================
#Using Principle Component Analysis
#somewhat as practice for my project 
principal_components<-princomp(redwine, cor=TRUE, score=TRUE)
summary(principal_components)
#bit of a mistake there, lets slim down the datasets
redwine1<-redwine[,2:9]
head(redwine1)
principal_components<-princomp(redwine1, cor=TRUE, score=TRUE)
summary(principal_components)
plot(principal_components, main="PCA redwine")
plot(principal_components, type="l", main="PCA redwine")
biplot(principal_components)
#now whitewine
whitewine1<-whitewine[,2:9]
head(whitewine1)
principal_components_w<-princomp(whitewine1, cor=TRUE, score=TRUE)
summary(principal_components_w, main="Biplot Red Wine")
plot(principal_components_w, main="PCA whitewine")
plot(principal_components_w,type="l", main="PCA whitewine")
biplot(principal_components_w, main="Biplot White Wine")
#PCA of Absent Data
#creating subset of this data
Absent1<-subset(AbsentNZatih, select=-c(ID,Reason.for.absence,Month.of.absence,Day.of.the.week,Seasons,Transportation.expense,Hit.target,Disciplinary.failure,Social.drinker))
head(Absent1)
principal_components_A<-princomp(Absent1, cor=TRUE, score=TRUE)
summary(principal_components_A)
plot(principal_components_A, main="PCA Absent")
plot(principal_components_A, type="l", main="PCA Absent")
biplot(principal_components_A)                
#PCA of CervicalData
#Happy with dataset as is for PCA
principal_components_CD<-princomp(CervicalData, cor=TRUE, score=TRUE)
summary(principal_components_CD)
plot(principal_components_CD, main="PCA Cervical Data")
plot(principal_components_CD, type="l", main="PCA Cervical Data")
biplot(principal_components_CD)
#might benefit from filtering both datasets for those with and those without cervical
#cancer and running PCA to spotlight differences-----DO THIS
principal_components_CPos<-princomp(CancerPosData1, cor=FALSE, score=TRUE)
summary(principal_components_CPos)
biplot(principal_components_CPos)
#==============================================================================
#Model Development - Model 1
#Using Clustering KMEANS

#REDWINE
View(redwine1)
#determine number of clusters that is optimal
#Elbow
install.packages("factoextra")
install.packages("NbClust")
library(factoextra)
library(NbClust)
fviz_nbclust(redwine1,kmeans,method="wss")+geom_vline(xintercept=4,linetype=2)+labs(subtitle="Elbow Method")
#Silhouette
fviz_nbclust(redwine1,kmeans,method="silhouette")+labs(subtitle="Redwine Silhouette Method")
#Gap Statistic Method
fviz_nbclust(redwine1,kmeans,nstart=25,method="gap_stat",nboot=500)+labs(subtitle = "Redwine Gap Statistic Method")
#We will go with 3 clusters (middle ground between Elbow and Silhouette)
redwine_results<-kmeans(redwine1,3)
redwine_results
#depending on intialization of the clusters, will be variation if you run it
table(redwine1$r.pH, redwine_results$cluster)
#checking clustering on pH of redwine data (poor predictor)
table(redwine1$r.volatile.acidity, redwine_results$cluster)
table(redwine1$r.citric.acid, redwine_results$cluster)
#same for vol acid and citric acid variables
plot(redwine1[c("r.pH","r.volatile.acidity")],col=redwine_results$cluster)
#variables plotted on similr PC1 acid of PCA and clustering failed to distinguish
plot(redwine1[c("r.free.sulfur.dioxide","r.total.sulfur.dioxide")],col=redwine_results$cluster)
#THIS ONE WORKS!!! Nice, three distinct clusters, these two variables plot close
#on PC2 axis
plot(redwine1[c("r.total.sulfur.dioxide","r.density")],col=redwine_results$cluster)
#redwine$r.density
#redwine$r.chlorides
#redwine$r.citric.acid
#redwine$r.residual.sugar
#redwine$r.volatile.acidity
#redwine clustering only works well at predicting free and total sulfur dioxide

#WHITEWINE
View(whitewine1)
#determine number of clusters that is optimal
#Elbow
#library(factoextra)
#library(NbClust)
fviz_nbclust(whitewine1,kmeans,method="wss")+geom_vline(xintercept=4,linetype=2)+labs(subtitle="Whitewine Elbow Method")
#Silhouette
fviz_nbclust(whitewine1,kmeans,method="silhouette")+labs(subtitle="Whitewine Silhouette Method")
#Gap Statistic Method
fviz_nbclust(whitewine1,kmeans,nstart=25,method="gap_stat",nboot=500)+labs(subtitle = "Whitewine Gap Statistic Method")
#We will go with 3 clusters (middle ground between Elbow and Silhouette)
whitewine_results<-kmeans(whitewine1,3)
whitewine_results
plot(whitewine1[c("w.free.sulfur.dioxide","w.total.sulfur.dioxide")],col=whitewine_results$cluster)
#THIS ONE WORKS!!! Nice, three distinct clusters, these two variables plot close
#on PC2 axis
plot(whitewine1[c("w.total.sulfur.dioxide","w.density")],col=whitewine_results$cluster)
#works somewhat well, maybe need 4 clusters to use these two variables

#Trying another model with 4 cluster groups
whitewine_results2<-kmeans(whitewine1,4)
whitewine_results2
plot(whitewine1[c("w.free.sulfur.dioxide","w.total.sulfur.dioxide")],col=whitewine_results2$cluster)
#THIS ONE WORKS!!! Nice, three distinct clusters, these two variables plot close
#on PC2 axis
plot(whitewine1[c("w.density","w.total.sulfur.dioxide")],col=whitewine_results2$cluster)
#works somewhat well!
#w.density
#w.chlorides
#w.citric.acid
#w.residual.sugar
#w.volatile.acidity

#ABSENT
fviz_nbclust(Absent1,kmeans,method="wss")+geom_vline(xintercept=4,linetype=2)+labs(subtitle="Absent Elbow Method")
#Silhouette
fviz_nbclust(Absent1,kmeans,method="silhouette")+labs(subtitle="Absent Silhouette Method")
#Gap Statistic Method
absent_results<-kmeans(Absent1,3)
absent_results
plot(Absent1[c("Absenteeism.time.in.hours","Work.load.Average.day")],col=absent_results$cluster)
#Works well!
#names(Absent1)
plot(Absent1[c("Work.load.Average.day","Social.smoker")],col=absent_results$cluster)
#work must be rough
plot(Absent1[c("Work.load.Average.day","Education")],col=absent_results$cluster)
#Also works
plot(Absent1[c("Distance.from.Residence.to.Work","Work.load.Average.day")],col=absent_results$cluster)
plot(Absent1[c("Body.mass.index","Absenteeism.time.in.hours")],col=absent_results$cluster)
#[1] "Distance.from.Residence.to.Work" "Service.time"                   
#[3] "Age"                             "Work.load.Average.day"          
#[5] "Education"                       "Son"                            
#[7] "Social.smoker"                   "Pet"                            
#[9] "Weight"                          "Height"                         
#[11] "Body.mass.index"                 "Absenteeism.time.in.hours" 

#Cervical Cancer
#Lets make two subsets of data (one cancer positive and one cancer negative)

CancerPosData1<-CervicalData[CervicalData$ca_cervix != 0, ]
View(CancerPosData1)
CancerNegData1<-CervicalData[CervicalData$ca_cervix != 1, ]
View(CancerNegData1)
fviz_nbclust(CancerPosData1,kmeans,method="wss")+geom_vline(xintercept=4,linetype=2)+labs(subtitle="Cervical Cancer Pos Elbow Method")
fviz_nbclust(CancerNegData1,kmeans,method="wss")+geom_vline(xintercept=4,linetype=2)+labs(subtitle="Cervical Cancer Neg Elbow Method")
#Silhouette
fviz_nbclust(CancerNegData1,kmeans,method="silhouette")+labs(subtitle="Cervical Cancer Neg Silhouette Method")
fviz_nbclust(CancerPosData1,kmeans,method="silhouette")+labs(subtitle="Cervical Cancer Pos Silhouette Method")
PosCancer_results<-kmeans(CancerPosData1,3)
PosCancer_results
NegCancer_results<-kmeans(CancerNegData1,3)
NegCancer_results

principal_components_CPos<-princomp(CancerPosData1, cor=FALSE, score=TRUE)
summary(principal_components_CPos)
plot(principal_components_CPos, main="PCA Cervical Cancer Pos Data")
plot(principal_components_CPos, type="l", main="PCA Cervical Cancer Pos Data")
biplot(principal_components_CPos)
plot(CancerPosData1[c("empowerment_desires","socialSupport_instrumental")],col=PosCancer_results$cluster)


principal_components_CNeg<-princomp(CancerNegData1, cor=FALSE, score=TRUE)
summary(principal_components_CNeg)
plot(principal_components_CNeg, main="PCA Cervical Cancer Neg Data")
plot(principal_components_CNeg, type="l", main="PCA Cervical Cancer Neg Data")
biplot(principal_components_CNeg)
plot(CancerNegData1[c("empowerment_desires","socialSupport_instrumental")],col=PosCancer_results$cluster)
#names(CervicalData)
#[1] "behavior_sexualRisk"        "behavior_eating"           
#[3] "behavior_personalHygine"    "intention_aggregation"     
#[5] "intention_commitment"       "attitude_consistency"      
#[7] "attitude_spontaneity"       "norm_significantPerson"    
#[9] "norm_fulfillment"           "perception_vulnerability"  
#[11] "perception_severity"        "motivation_strength"       
#[13] "motivation_willingness"     "socialSupport_emotionality"
#[15] "socialSupport_appreciation" "socialSupport_instrumental"
#[17] "empowerment_knowledge"      "empowerment_abilities"     
#[19] "empowerment_desires"        "ca_cervix"

#maybe not the best analysis with so little sample size
#==============================================================================
#Model Development - Model 2
#KNN Clustering

#Redwine
redwine1$r.total.sulfur.dioxide<-redwine$r.total.sulfur.dioxide
View(redwine1)                        
redwine1$r.total.sulfur.dioxide1<-as.numeric(redwine$r.total.sulfur.dioxide)
redwine1$r.total.sulfur.dioxide1<-cut(redwine1$r.total.sulfur.dioxide,br=c(-1,40,80,175),labels=c("Class1",'Class2','Class3'))
summary(redwine1$r.total.sulfur.dioxide1)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
redwine1<-as.data.frame(lapply(redwine1,normalize))
summary(redwine1$r.volatile.acidity)
#checking normalization
ind<-sample(2,nrow(redwine1),replace=TRUE,prob=c(0.7,0.3))
#need only non-zero-use complete cases?

KNNtrain<-redwine1[ind==1,]
KNNtest<-redwine1[ind==2,]
sqrt(1599)
#39
library(class)
#KNNtrain$r.free.sulfur.dioxide
KNNpred<-knn(train=KNNtrain,test=KNNtest,cl=KNNtrain$r.total.sulfur.dioxide,k=39) 
#KNNtrain is training set cases;KNNtest is test set cases, and cl indicates the 
#factor of true classifications of the KNNtrain set
KNNpred #prediction
table(KNNpred) 

whitewine1$w.total.sulfur.dioxide1<-as.numeric(whitewine$w.total.sulfur.dioxide)
whitewine1$w.total.sulfur.dioxide1<-cut(whitewine1$w.total.sulfur.dioxide,br=c(-1,120,180,400),labels=c("Class1",'Class2','Class3'))
summary(whitewine1$w.total.sulfur.dioxide1)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
whitewine1<-as.data.frame(lapply(whitewine1,normalize)) #error
summary(whitewine1$w.volatile.acidity)
#===============================================================================
#Model 3 - SVM

#Redwine
library(ggplot2)
library(e1071)
svm_model1<-svm(redwine1$r.total.sulfur.dioxide1~.,data=redwine)
summary(svm_model1)
pred1<-predict(svm_model1,redwine)
table1<-table(Predicted=pred1,Actual=redwine1$r.total.sulfur.dioxide1)
table1
#         Actual
#Predicted Class1 Class2 Class3
#Class1    841     14      0
#Class2     17    477      5
#Class3      0      2    241
Model1_accuracyRate = sum(diag(table1))/sum(table1)
Model1_accuracyRate
#[1] 0.9762054
Model1_MissClassificationRate = 1 - Model1_accuracyRate
Model1_MissClassificationRate
#[1] 0.02379461

#Whitewine
svm_model2<-svm(whitewine1$w.total.sulfur.dioxide1~.,data=whitewine)
summary(svm_model2)
pred2<-predict(svm_model2,whitewine)
table2<-table(Predicted=pred2,Actual=whitewine1$w.total.sulfur.dioxide1)
table2
#         Actual
#Predicted Class1 Class2 Class3
#Class1   1806     19      0
#Class2     22   2214     26
#Class3      0      6    804
Model2_accuracyRate = sum(diag(table2))/sum(table2)
Model2_accuracyRate
#[1] 0.9850929
Model2_MissClassificationRate = 1 - Model2_accuracyRate
Model2_MissClassificationRate
#[1] 0.01490709

#Absent
Absent1$Class<-as.numeric(Absent1$Absenteeism.time.in.hours)
Absent1$Class<-cut(Absent1$Work.load.Average.day,br=c(-1,260,300,400),labels=c("Class1",'Class2','Class3'))
summary(Absent1$Class)
View(Absent1)
svm_model3<-svm(Absent1$Class~.,data=Absent1)
summary(svm_model3)
pred3<-predict(svm_model3,Absent)
table3<-table(Predicted=pred3,Actual=Absent1$Class)
table3
#         Actual
#Predicted Class1 Class2 Class3
#Class1    296     10      0
#Class2      3    225      4
#Class3      0      1    157
Model3_accuracyRate = sum(diag(table3))/sum(table3)
Model3_accuracyRate
#[1] 0.9741379
Model3_MissClassificationRate = 1 - Model3_accuracyRate
Model3_MissClassificationRate
#[1] 0.02586207

#==============================================================================
#Model 3 - CervicalCancer
#Pearson Analysis ->regression
CancerPos.cor=cor(CancerPosData1,method=c("spearman"))
View(CancerPos.cor)
#empowerment desires & social support instrumental 0.71
#motivational strength & perc. ability -0.60
boxplot(CancerPosData1$empowerment_desires,CancerPosData1$socialSupport_instrumental, main="Cancer Positive Empowerment Desires (left) and Social Support Instrumental (right)")
attach(CancerPosData1)
r<-lm(empowerment_desires~socialSupport_instrumental,data=CancerPosData1)
summary(r)
plot(socialSupport_instrumental,empowerment_desires)
cor(socialSupport_instrumental,empowerment_desires)
abline(r)

CancerNeg.cor=cor(CancerNegData1,method=c("pearson"))#n>30
View(CancerNeg.cor)
#perception severity & norm fulfillment 0.85
#perception severity & perception vulnerability 0.81
#social support appreciation & social support emotionality 0.86
#social support instrumental & social support appreciation 0.81
#empow abilities & social support instrumental 0.82
#empow abilities & empowerment knowledge 0.80
attach(CancerNegData1)
r<-lm(socialSupport_instrumental~socialSupport_appreciation,data=CancerNegData1)
summary(r)
plot(socialSupport_appreciation,socialSupport_instrumental)
cor(socialSupport_appreciation,socialSupport_instrumental)
abline(r)

#==============================================================================
#Regression - Other
#Wine
rR<-lm(r.free.sulfur.dioxide~r.total.sulfur.dioxide,data=redwine1)
summary(rR)
plot(r.total.sulfur.dioxide,r.free.sulfur.dioxide)
cor(r.total.sulfur.dioxide,r.free.sulfur.dioxide)
#[1] 0.6676665
abline(rR)

rW<-lm(w.free.sulfur.dioxide~w.total.sulfur.dioxide,data=whitewine1)
summary(rW)
plot(w.total.sulfur.dioxide,w.free.sulfur.dioxide)
cor(w.total.sulfur.dioxide,w.free.sulfur.dioxide)
#[1] 0.615501
abline(rW)

#Absent
#Pearson first to see most signif correlation
Absent.cor=cor(Absent,method=c("pearson"))
View(Absent.cor)
rA<-lm(Body.mass.index~Weight,data=Absent1)
summary(rA)
plot(Weight,Body.mass.index)
cor(Weight,Body.mass.index)
#[1] 0.9041169
abline(rA)
