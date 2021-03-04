#Lab 2 part 2
#using dataset_multipleRegression.csv, use the unemployment rate (UNEM) and 
#number of spring high school graduates (HGRAD) to predict the fall enrollment 
#(ROLL) for this year by knowing that UNEM=7% and HGRAD=90,000
#
#This dataset contains 29 years worth of data related to fall enrollment (ROLL)
#unemployment rate (UNEM), spring high school graduates (HGRAD), and per capita
#income (INC).  We will start by making a prediction of ROLL based on regression.
mR<-read.csv("dataset_multipleRegression.csv")
attach(mR)
#fix(mR)
mR
#making a scatterplot
plot(mR$UNEM,mR$ROLL)
#correlation
cor(mR$UNEM,mR$ROLL)
#Model: trying a simple linear regression
r<-lm(ROLL~UNEM,data=mR) #Y first tilda then name of X
#add regression line (must run regression before line)
abline(r)
summary(r)
#names to access regression object
names(r)
#fitted values for xs, and plot fitted v UNEM
fitted(r)
plot(mR$UNEM,r$fitted)
#1.) how to make a prediction with a certain x?
3957.0+1133.8*7
#[1] 11893.6
#predicted value for ROLL (fitted) at unemployment rate of (UNEM) of 7%
#yields a fall enrollment of 11893.6 based on the fitted regression model

#2.)what if we use coefficient?
coef(r) #gives intercept and slope coef
coef(r)[1] #gives intercept
coef(r)[2] #gives slope coef
r$coef[1] #another way to run same above command

r$coef[1]+r$coef[2]*7
#this above command gives us the intercept value of 11893.81 based on a 
#UNEM of 7% slightly more sig figs that prior method

#3.) Using the predict function
predict(r,list(UNEM=7))
predict(r,data.frame(UNEM=c(7)))
#both above predict functions give estimates of 11893.81

#Predictive vs confidence interval
predict(r,data.frame(UNEM=c(7)),interval="confidence", level=0.95)
#fit      lwr      upr
#1 11893.81 10508.08 13279.53
predict(r,data.frame(UNEM=c(7)),interval="prediction", level=0.95)

#when do you use predictive vs confidence interval


#repeat and add per capita income (INC) to the model. Predict ROLL if INC=$25,000
#=================================================================================================
#retreive the abalone.csv dataset, perform knn classification to get predictors for age (rings). Interpretation not required.
#abalone dataset from UCI repository
#reading the dataset from UCI repository URL
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"),header=FALSE,sep=",")
#Column names
colnames(abalone)<-c("sex","length",'diameter','height','whole_weight','shucked_weight','viscera_weight','shell_weight','rings')
#summary on abalone
summary(abalone)
#structure of the abalone data
str(abalone)
#summary of the abalone rings column
summary(abalone$rings)
#"rings" variable has a range between 1-29; we want to predict this variable
#break the rings variable into 3 levels "young"(<8) "adult"(8-11) and "old"(11<)
abalone$rings<-as.numeric(abalone$rings)
abalone$rings<-cut(abalone$rings,br=c(-1,8,11,35),labels=c("young",'adult','old'))
abalone$rings<-as.factor(abalone$rings)
summary(abalone$rings)
#z<-abalone
aba<-abalone
aba$sex<-NULL #remove sex variable because KNN needs numeric
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
} #normalize the data using min max normalization
aba[1:7]<-as.data.frame(lapply(aba[1:7],normalize))
summary(aba$shucked_weight)
#After normalization each variable has a min of 0 and max of 1, we will now split the data into train and test sets
ind<-sample(2,nrow(aba),replace=TRUE,prob=c(0.7,0.3))
KNNtrain<-aba[ind==1,]
KNNtest<-aba[ind==2,]
sqrt(2918) #make k equal to the square root of 2918, the number of observations in the training set, sqrt(2918) is roughly 54.01852->rounds to 55
#take the odd value for k value
library(class)
help("knn") #documentation
KNNpred<-knn(train=KNNtrain[1:7],test=KNNtest[1:7],cl=KNNtrain$rings,k=55) #KNNtrain is training set cases;KNNtest is test set cases, and cl indicates the factor of true classifications of the KNNtrain set
KNNpred #prediction
table(KNNpred) #organize the prediction into a table (below)
#young adult  old 
# 413   726   117 
#====================================================================================================================================================================
#exercise 3
set.seed(123)
sim.xy<-function(n,mean,sd)cbind(rnorm(n,mean[1],sd[1]),rnorm(n,mean[2],sd[2]))
xy<-rbind(sim.xy(100,c(0,0),c(.2,.2)),sim.xy(100,c(2.5,0),c(.4,.2)),sim.xy(100,c(1.25,.5),c(.3,.2))) 
#generate three clouds of points, well separated in the 2D plane
xy
xy[1,]<-c(0,2) #converting the 1st observation to an outlying value
km3<-kmeans(xy,3) #ask for three clusters
km3
plot(xy,col=km3$centers,pch=3, main="3 Cluster Plot")
cex=2.0
points(km3$centers,pch=3)
km4<-kmeans(xy,4) #asking for 4 clusters
cex=1.0
plot(xy,col=km4$cluster, main="4 Cluster Plot")
cex=2.0
points(km4$centers,pch=3)

#using the iris dataset, create a new dataframe and remove the fifth column.  
data("iris") #loading our dataset
dfiris<-subset(iris,select=-c(Species)) 
dfiris
dfiris2<-scale(dfiris) #scaling the data
head(dfiris2, n=3)
#
#loading the required packages
library(factoextra)
library(NbClust)
#
#Elbow method for optimal clustering
fviz_nbclust(dfiris2,kmeans,method="wss")+geom_vline(xintercept=4,linetype=2)+labs(subtitle="Iris Elbow Method")
#geom_vline function adds a line for better visualization, and labs(subtitle="") function adds a subtitle
#appears that 3 or 4 clusters is ideal for the iris dataset from this method
#
#Silhouette method
fviz_nbclust(dfiris2,kmeans,method="silhouette")+labs(subtitle="Iris Silhouette Method")
#this method suggests only two clusters for the iris dataset
#
#Gap statistic method
set.seed(123)
fviz_nbclust(dfiris2,kmeans,nstart=25,method="gap_stat",nboot=500)+labs(subtitle = "Iris Gap Statistic Method")
#this method suggests only 2 clusters
#
#NbClust() method
nbclust_out<-NbClust(data=dfiris2,distance="euclidean",min.nc=2,max.nc=4,method="kmeans")
#min.nc is minimum number of clusters, max.nc is maximum number of clusters, 
#method is one of "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", or "kmeans"
#This method shows an elbow at 3.0 for the D index value plot and only one point at 3 clusters on the Second differences D index Value plot
#
nbclust_plot<-data.frame(clusters=nbclust_out$Best.nc[1, ]) #create a dataframe of the optimal number of clusters
nbclust_plot<-subset(nbclust_plot,clusters>=2&clusters<=4) #select only indeces which select between 2 and 4 clusters
ggplot(nbclust_plot)+aes(x=clusters)+geom_histogram(bins=30L,fill="#0c4c8a")+
  labs(x="Number of Clusters",y="Frequency amoung all indeces",title="Iris Optimal Number of Clusters")+theme_dark() #create the plot
#It seems that the best number of clusters is ideally 2 (possibly 3)
#
#building a cluster silhouette plot to check above findings
library(cluster)
set.seed(123)
km_res<-kmeans(dfiris2,centers=4,nstart=20) #centers is number of clusters
sil<-silhouette(km_res$cluster,dist(dfiris2))
fviz_silhouette(sil)
#
#making a fviz_cluster()
library(factoextra) #already loaded above but add incase i use code later
fviz_cluster(km_res,dfiris2,ellipse.type="norm", main="Iris Cluster Plot")
#
#Applying kmeans with my defined k and 1000 iterations
fit<-kmeans(dfiris2,centers=3,iter.max=1000)
cluster.data<-data.frame(dfiris2,fit$cluster)
cluster.data
#
#plotting clustering efforts with 3 cluster groups
fviz_cluster(fit,dfiris2,ellipse.type="norm",main="Iris Cluster Plot")
#
#replotting clustering efforts with 4 cluster groups
fit2<-kmeans(dfiris2,centers=4,iter.max=1000)
fviz_cluster(fit2,dfiris2,ellipse.type="norm",main="Iris Cluster Plot")
#
#use table(iris[5],<your clustering>) to assess your results
table(iris[5],cluster.data[5])
#     1:3
#1:3   0