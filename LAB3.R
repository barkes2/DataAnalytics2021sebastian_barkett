#Generating boxplots for 5 nyt (6,12,15,23,31) 2 variables ("Age", "Impressions")
#Deliverables:2 figures (e.g. Age and Impression figures) that have 5 boxplots
nyt6<-read.csv("nyt6.csv")
nyt12<-read.csv("nyt12.csv")
nyt15<-read.csv("nyt15.csv")
nyt23<-read.csv("nyt23.csv")
nyt31<-read.csv("nyt31.csv")
head(nyt6)
#Rows 10-29 are converting the Age values that are 0 to NA and then removing them
nyt6$Age[nyt6$Age==0]<-NA 
tf<-is.na(nyt6$Age)
nyt6Agenz<-nyt6$Age[!tf]
nyt6Agenz
nyt12$Age[nyt12$Age==0]<-NA 
tf1<-is.na(nyt12$Age)
nyt12Agenz<-nyt12$Age[!tf1]
nyt12Agenz
nyt15$Age[nyt15$Age==0]<-NA 
tf2<-is.na(nyt15$Age)
nyt15Agenz<-nyt15$Age[!tf2]
nyt15Agenz
nyt23$Age[nyt23$Age==0]<-NA 
tf3<-is.na(nyt23$Age)
nyt23Agenz<-nyt23$Age[!tf3]
nyt23Agenz
nyt31$Age[nyt31$Age==0]<-NA 
tf4<-is.na(nyt31$Age)
nyt31Agenz<-nyt31$Age[!tf4]
nyt31Agenz

#"Gender" and "Signed_In" appear to be discrete data
#Need to choose quant data ("Age" and "Impressions" will do)
par(mfrow=c(2,3))
boxplot(nyt6Agenz, ylab="Age",main="Boxplot Dist. of Age in NYT6")
boxplot(nyt12Agenz, ylab="Age",main="Boxplot Dist. of Age in NYT12")
boxplot(nyt15Agenz, ylab="Age",main="Boxplot Dist. of Age in NYT15")
boxplot(nyt23Agenz, ylab="Age",main="Boxplot Dist. of Age in NYT23")
boxplot(nyt31Agenz, ylab="Age",main="Boxplot Dist. of Age in NYT31")
#Now lets do Impressions
par(mfrow=c(2,3))
boxplot(nyt6$Impressions, ylab="Impressions",main="Boxplot of Impressions in NYT6")
boxplot(nyt12$Impressions, ylab="Impressions",main="Boxplot of Impressions in NYT12")
boxplot(nyt15$Impressions, ylab="Impressions",main="Boxplot of Impressions in NYT15")
boxplot(nyt23$Impressions, ylab="Impressions",main="Boxplot of Impressions in NYT23")
boxplot(nyt31$Impressions, ylab="Impressions",main="Boxplot of Impressions in NYT31")
#Now we will create histograms for all 5 datasets and the variables "Age" and "Impressions"
par(mfrow=c(2,3))
hist(nyt6Agenz,breaks=14,ylab="Count of Users within Bin Age Group",xlab="Age",main="Histogram of Age in NYT6")
hist(nyt12Agenz,breaks=14,ylab="Count of Users within Bin Age Group",xlab="Age",main="Histogram of Age in NYT12")
hist(nyt15Agenz,breaks=13,ylab="Count of Users within Bin Age Group",xlab="Age",main="Histogram of Age in NYT15")
hist(nyt23Agenz,breaks=14,ylab="Count of Users within Bin Age Group",xlab="Age",main="Histogram of Age in NYT23")
hist(nyt31Agenz,breaks=14,ylab="Count of Users within Bin Age Group",xlab="Age",main="Histogram of Age in NYT31")
par(mfrow=c(2,3))
hist(nyt6$Impressions,breaks=15,ylab="Count of Users within Bin Impression Group",xlab="Impressions",main="Histogram Impressions NYT6")
hist(nyt12$Impressions,breaks=14,ylab="Count of Users within Bin Impression Group",xlab="Impressions",main="Histogram Impressions NYT12")
hist(nyt15$Impressions,breaks=14,ylab="Count of Users within Bin Impression Group",xlab="Impressions",main="Histogram Impressions NYT15")
hist(nyt23$Impressions,breaks=14,ylab="Count of Users within Bin Impression Group",xlab="Impressions",main="Histogram Impressions NYT23")
hist(nyt31$Impressions,breaks=14,ylab="Count of Users within Bin Impression Group",xlab="Impressions",main="Histogram Impressions NYT31")
#Now we will plot the ECDF or Empirical distribution function for "Age" and "Impressions"

