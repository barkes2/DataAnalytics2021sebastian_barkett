#Lab 2 part 2
#using dataset_multipleRegression.csv, use the unemployment rate (UNEM) and number of spring high school graduates (HGRAD) to predict the fall enrollment (ROLL) for this year by knowing that UNEM=7% and HGRAD=90,000
getwd()

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
#remove sex variable because KNN needs numeric
#z<-abalone
aba<-abalone
aba$sex<-NULL
#normalize the data using min max normalization
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
aba[1:7]<-as.data.frame(lapply(aba[1:7],normalize))
summary(aba$shucked_weight)

#=================================================================================================
#exercise 3
set.seed(123)
sim.xy<-function(n,mean,sd)cbind(rnorm(n,mean[1],sd[1]),rnorm(n,mean[2],sd[2]))
xy<-rbind(sim.xy(100,c(0,0),c(.2,.2)),sim.xy(100,c(2.5,0),c(.4,.2)),sim.xy(100,c(1.25,.5),c(.3,.2))) #generate three clouds of points, well separated in the 2D plane
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


#computing kmeans with k=4
set.seed(123)
KMEANSRESULT<-kmeans(dfiris2,4,n=25) #kmeans clustering result with 4 clusters after 25 iterations pick the best
print(KMEANSRESULT)#printing the results
KMEANSRESULT2<-kmeans(dfiris2,3,n=25) #clearly too many clustering groups if ran with 4
print(KMEANSRESULT2)#printing the results
aggregate(dfiris, by-list(cluster-KMEANSRESULT2$cluster),mean) #computing the means of all three cluster groups

iris.dist<-dist(iris[,-5])
iris.dist #weird matrix style representation
iris.mds<-cmdscale(iris.dist)
iris.mds #150 rows, 2 columns ([,1];[,2])
c.chars<-c("*","o","+")[as.integer(iris$Species)]
#iris$Species is the 5th column
c.chars
#KMEANSRESULT is the variable you used in your kmeans lab assignment for the return variable
KMEANSRESULT<-kmeans()
a.cols<-rainbow(3)[KMEANSRESULT$cluster]

#using this dataframe, apply kmeans with 1000 iterations

#use table(iris[5],<your clustering>) to assess your results