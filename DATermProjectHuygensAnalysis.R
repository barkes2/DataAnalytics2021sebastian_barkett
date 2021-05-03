huygenspt<-read.csv("XXXHUYGENSPT.csv")
huygenszm<-read.csv("XXXHUYGENSWindMole.csv")
names(huygenspt)
#[1] "TIME..MILLISECONDS."    "HUYGENS.ALTITUDE..KM."  "ALTITUDE..M..x"        
#[4] "TEMPERATURE..K."        "TEMP.UNCERTAINTY..K."   "TOTAL.PRESSURE..Pa."   
#[7] "AMBIENT.PRESSURE..Pa."  "ALTITUDE..M..y"         "DESCENT.VELOCITY..M.S."
names(huygenszm)
#[1] "X..........UTC_ABS_TIME."     "HUYGENS.ALTITUDE..KM."       
#[3] "ALTITUDE..M..x"               "ZONAL.WIND.SPEED..M.S."      
#[5] "ZONAL.WIND.SPEED.ERROR..M.S." "X.CH4."      
HuygensData<-merge(huygenspt,huygenszm,by=c('HUYGENS.ALTITUDE..KM.'),all.x=T)
View(HuygensData)
write.csv(HuygensData,"C:/Users/sdbar/Box Sync/HUYGENS/csv FOR DA\\FINALHuygensData.csv",row.names=FALSE)
#Full dataset is compiled, now to remove any NA values
HuygensDataNZ<-HuygensData[complete.cases(HuygensData),]
View(HuygensDataNZ)
#now remove unneccesary columns
names(HuygensDataNZ)
#[1] "HUYGENS.ALTITUDE..KM."        "TIME..MILLISECONDS."         
#[3] "ALTITUDE..M..x.x"             "TEMPERATURE..K."             
#[5] "TEMP.UNCERTAINTY..K."         "TOTAL.PRESSURE..Pa."         
#[7] "AMBIENT.PRESSURE..Pa."        "ALTITUDE..M..y"              
#[9] "DESCENT.VELOCITY..M.S."       "X..........UTC_ABS_TIME."    
#[11] "ALTITUDE..M..x.y"             "ZONAL.WIND.SPEED..M.S."      
#[13] "ZONAL.WIND.SPEED.ERROR..M.S." "X.CH4."
#Will remove 2,10
HUYGENS<-HuygensDataNZ[-c(2,10)]
View(HUYGENS)
plot(X.CH4.,HUYGENS$ALTITUDE..M..x.x, main="CH4 vs Altitude (m)")
hist(ZONAL.WIND.SPEED..M.S.,main="Histogram of Titan's Wind Speed")
plot(ZONAL.WIND.SPEED..M.S.,HUYGENS$ALTITUDE..M..x.x, main="Wind Speed vs Altitude (m)")
hist(TOTAL.PRESSURE..Pa.,main="Histogram of Titan's Total Pressure")
plot(TOTAL.PRESSURE..Pa.,HUYGENS$ALTITUDE..M..x.x, main="Total P (Pa) vs Altitude (m)")
hist(TEMPERATURE..K.,main="Histogram of Titan's Temperature (K)")
plot(TEMPERATURE..K.,HUYGENS$ALTITUDE..M..x.x, main="Temperature (K) vs Altitude (m)")
#==============================================================================
#Principle Component Analysis
principal_components<-princomp(HUYGENS,cor=TRUE,score=TRUE)
summary(principal_components)
#looks like 97% of the variation in this dataset is explained by the first two 
#principal components
plot(principal_components, main="Huygens PCA Histogram")
plot(principal_components,type="l", main="Huygens PCA Line")
biplot(principal_components, main="Huygens PCA Biplot")
#==============================================================================
#Now a Pearson's parametric correlation analysis
attach(HUYGENS)
HUYGENS.cor=cor(HUYGENS,method=c("pearson"))
View(HUYGENS.cor)
write.table(HUYGENS.cor,"C:/Users/sdbar/Box Sync/HUYGENS/csv FOR DA\\XXXHuygensPearsonMatrix.txt",sep="\t")
#19 relationships that have positive Pearson correlation coefficients above 0.90
#2 of these relationships are between CH4 concentration and total/ambient pressure
#===============================================================================
#Optimal Cluster Methods
library(factoextra)
library(NbClust)
fviz_nbclust(HUYGENS,kmeans,method="wss")+geom_vline(xintercept=4,linetype=2)+labs(subtitle="HUYGENS Elbow Method")
#4 clusters

#Silhouette
fviz_nbclust(HUYGENS,kmeans,method="silhouette")+labs(subtitle="HUYGENS Silhouette Method")
#2 clusters

#Gap Statistic Method
fviz_nbclust(HUYGENS,kmeans,nstart=25,method="gap_stat",nboot=500)+labs(subtitle = "HUYGENS Gap Statistic Method")
#5 clusters
#==============================================================================
#Model 1 - KMeans
HUYGENSKMeansresults<-kmeans(HUYGENS,4)
HUYGENSKMeansresults
#CH4 Mole Fraction vs Total Pressure; R coef = 0.98
plot(HUYGENS[c("X.CH4.","TOTAL.PRESSURE..Pa.")],col=HUYGENSKMeansresults$cluster
     ,main="HUYGENS KMeans Cluster CH4 vs Total Pressure")
#CH4 Mole Fraction vs Ambient Pressure; R coef = 0.98
plot(HUYGENS[c("X.CH4.","AMBIENT.PRESSURE..Pa.")],col=HUYGENSKMeansresults$cluster
     ,main="HUYGENS KMeans Cluster CH4 vs Ambient Pressure")
#T vs Altitude (m); R coef = 0.60
plot(HUYGENS[c("TEMPERATURE..K.","ALTITUDE..M..x.x")],col=HUYGENSKMeansresults$cluster
     ,main="HUYGENS KMeans Cluster T(K) vs Altitude (m)")
#Zonal Wind Speed vs Altitude(m); R coef = 0.95
plot(HUYGENS[c("ZONAL.WIND.SPEED..M.S.","ALTITUDE..M..x.x")],col=HUYGENSKMeansresults$cluster
     ,main="HUYGENS KMeans Cluster Wind Speed (m/s) vs Altitude (m)")
#Wind Speed (m) vs Descent Vel (m/s); R coef = 0.85
plot(HUYGENS[c("ZONAL.WIND.SPEED..M.S.","DESCENT.VELOCITY..M.S.")],col=HUYGENSKMeansresults$cluster
     ,main="HUYGENS KMeans Cluster Wind Speed (m/s) vs Descent Velocity (m/s)")
#Altitude (m) vs CH4 Mole Fraction; R coef = -0.64
plot(HUYGENS[c("X.CH4.","ALTITUDE..M..x.x")],col=HUYGENSKMeansresults$cluster
     ,main="HUYGENS KMeans Cluster CH4 Mole Fraction vs Altitude (m)")

#Now lets try 3 clusters on a couple of the plots. May improve model
HUYGENSKMeansresults01<-kmeans(HUYGENS,3)
HUYGENSKMeansresults01
#Altitude (m) vs CH4 Mole Fraction; R coef = -0.64
plot(HUYGENS[c("X.CH4.","ALTITUDE..M..x.x")],col=HUYGENSKMeansresults01$cluster
     ,main="HUYGENS KMeans Cluster CH4 Mole Fraction vs Altitude (m)")
#Zonal Wind Speed vs Altitude(m); R coef = 0.95
plot(HUYGENS[c("ZONAL.WIND.SPEED..M.S.","ALTITUDE..M..x.x")],col=HUYGENSKMeansresults01$cluster
     ,main="HUYGENS KMeans Cluster Wind Speed (m/s) vs Altitude (m)")
#CH4 Mole Fraction vs Total Pressure; R coef = 0.98
plot(HUYGENS[c("X.CH4.","TOTAL.PRESSURE..Pa.")],col=HUYGENSKMeansresults01$cluster
     ,main="HUYGENS KMeans Cluster CH4 vs Total Pressure")

#One last KMeans run with Gap Stat suggestion - 5 clusters
HUYGENSKMeansresults02<-kmeans(HUYGENS,5)
HUYGENSKMeansresults02
#CH4 Mole Fraction vs Total Pressure; R coef = 0.98
plot(HUYGENS[c("X.CH4.","TOTAL.PRESSURE..Pa.")],col=HUYGENSKMeansresults02$cluster
     ,main="HUYGENS KMeans Cluster CH4 vs Total Pressure")
#CH4 Mole Fraction vs Ambient Pressure; R coef = 0.98
plot(HUYGENS[c("X.CH4.","AMBIENT.PRESSURE..Pa.")],col=HUYGENSKMeansresults02$cluster
     ,main="HUYGENS KMeans Cluster CH4 vs Ambient Pressure")
#T vs Altitude (m); R coef = 0.60
plot(HUYGENS[c("TEMPERATURE..K.","ALTITUDE..M..x.x")],col=HUYGENSKMeansresults02$cluster
     ,main="HUYGENS KMeans Cluster T(K) vs Altitude (m)")
#Zonal Wind Speed vs Altitude(m); R coef = 0.95
plot(HUYGENS[c("ZONAL.WIND.SPEED..M.S.","ALTITUDE..M..x.x")],col=HUYGENSKMeansresults02$cluster
     ,main="HUYGENS KMeans Cluster Wind Speed (m/s) vs Altitude (m)")
#Wind Speed (m) vs Descent Vel (m/s); R coef = 0.85
plot(HUYGENS[c("ZONAL.WIND.SPEED..M.S.","DESCENT.VELOCITY..M.S.")],col=HUYGENSKMeansresults02$cluster
     ,main="HUYGENS KMeans Cluster Wind Speed (m/s) vs Descent Velocity (m/s)")
#Altitude (m) vs CH4 Mole Fraction; R coef = -0.64
plot(HUYGENS[c("X.CH4.","ALTITUDE..M..x.x")],col=HUYGENSKMeansresults02$cluster
     ,main="HUYGENS KMeans Cluster CH4 Mole Fraction vs Altitude (m)")
#=============================================================================
#Partition Clustering

#Visualizing a distance matrix 
library("cluster")
library("factoextra")
library("magrittr")
res.dist <- get_dist(HUYGENS, stand = TRUE, method = "pearson")

fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Partition Clustering with 4 clusters<-suggested by Elbow Method
fviz_cluster(HUYGENSKMeansresults, data = HUYGENS,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(), main="Partition Cluster Plot HUYGENS 4 Clusters")
#Partition Clustering with 3 clusters<-compromise of Elbow and Silhouette Method
fviz_cluster(HUYGENSKMeansresults01, data = HUYGENS,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(), main="Partition Cluster Plot HUYGENS 3 Clusters")
#Partition Clustering with 5 clusters <- suggested by Gap Stat
fviz_cluster(HUYGENSKMeansresults02, data = HUYGENS,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(), main="Partition Cluster Plot HUYGENS 5 Clusters")
#= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#Model 1 cont. - PAM clustering
# Compute PAM
library("cluster")
pam.res4 <- pam(HUYGENS, 4)
# Visualize
fviz_cluster(pam.res4,main="HUYGENS PAM 4 Cluster")

# Compute PAM
pam.res5 <- pam(HUYGENS, 5)
# Visualize
fviz_cluster(pam.res5,main="HUYGENS PAM 5 Cluster")

# Compute PAM
pam.res3 <- pam(HUYGENS, 3)
# Visualize
fviz_cluster(pam.res3,main="HUYGENS PAM 3 Cluster")
#=============================================================================
#Kernel Density Estimation
attach(HUYGENS)
#Histogram of CH4
hist(HUYGENS$X.CH4.)
#Kernel Density
d<-density(X.CH4.)
#plot(density(X.CH4.))
plot(d,main="Density Plot Huygens CH4 Mole Fraction")#same but shorthand
polygon(d,col="red",border="blue")
#Dedicated R Package
install.packages("kdensity")
library(kdensity)
kde<-kdensity(HUYGENS$X.CH4.,start="gumbel",kernel="gaussian")
plot(kde,main="Kernel Density HUYGENS CH4 Mole Fraction")
#parametric density
plot(kde,main="Huygens CH4 Mole Fraction")
lines(kde,plot_start=TRUE,col="red")
rug(HUYGENS$X.CH4.)

#Histogram of Ambient Pressure
hist(AMBIENT.PRESSURE..Pa.)
#Kernel Density
d<-density(AMBIENT.PRESSURE..Pa.)
#plot(density(AMBIENT.PRESSURE..Pa.))
plot(d,main="Density Plot Huygens AMBIENT PRESSURE(Pa)")#same but shorthand
polygon(d,col="red",border="blue")
#Dedicated R Package
library(kdensity)
kde<-kdensity(HUYGENS$AMBIENT.PRESSURE..Pa.,start="gumbel",kernel="gaussian")
#parametric density
plot(kde,main="Huygens AMBIENT PRESSURE (Pa)")
lines(kde,plot_start=TRUE,col="red")
rug(HUYGENS$AMBIENT.PRESSURE..Pa.)

#Histogram of Zonal Wind Speed
hist(ZONAL.WIND.SPEED..M.S.)
#Kernel Density
d<-density(ZONAL.WIND.SPEED..M.S.)
#plot(density(AMBIENT.PRESSURE..Pa.))
plot(d,main="Density Plot Huygens ZONAL.WIND.SPEED (m/s)")#same but shorthand
polygon(d,col="red",border="blue")
#Dedicated R Package
library(kdensity)
kde<-kdensity(ZONAL.WIND.SPEED..M.S.,start="gumbel",kernel="gaussian")
#parametric density
plot(kde,main="Huygens ZONAL.WIND.SPEED (m/s)")
lines(kde,plot_start=TRUE,col="red")
rug(ZONAL.WIND.SPEED..M.S.)

#Histogram of Temperature
hist(TEMPERATURE..K.)
#Kernel Density
d<-density(TEMPERATURE..K.)
#plot(density(AMBIENT.PRESSURE..Pa.))
plot(d,main="Density Plot Huygens TEMPERATURE (K)")#same but shorthand
polygon(d,col="red",border="blue")
#Dedicated R Package
library(kdensity)
kde<-kdensity(TEMPERATURE..K.,start="gumbel",kernel="gaussian")
#parametric density
plot(kde,main="Huygens TEMPERATURE (K)")
lines(kde,plot_start=TRUE,col="red")
rug(TEMPERATURE..K.)

#Histogram of Descent Velocity
hist(DESCENT.VELOCITY..M.S.)
#Kernel Density
d<-density(DESCENT.VELOCITY..M.S.)
#plot(density(AMBIENT.PRESSURE..Pa.))
plot(d,main="Density Plot Huygens DESCENT.VELOCITY (m/s)")#same but shorthand
polygon(d,col="red",border="blue")
#Dedicated R Package
library(kdensity)
kde<-kdensity(DESCENT.VELOCITY..M.S.,start="gumbel",kernel="gaussian")
#parametric density
plot(kde,main="Huygens DESCENT.VELOCITY (m/s)")
lines(kde,plot_start=TRUE,col="red")
rug(DESCENT.VELOCITY..M.S.)
#=============================================================================
#Model 2 - Hierarchical Clustering
hc.complete<-hclust(dist(HUYGENS),method="complete")
hc.average<-hclust(dist(HUYGENS),method="average")
hc.single<-hclust(dist(HUYGENS),method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage")
plot(hc.average,main="Average Linkage")
plot(hc.single,main="Single Linkage")

#cutting our trees
cutree(hc.complete,5)
cutree(hc.average,5)
cutree(hc.single,5)

xsc<-scale(HUYGENS)
hc.complete.scale<-hclust(dist(xsc),method="complete")
hc.average.scale<-hclust(dist(xsc),method="average")
hc.single.scale<-hclust(dist(xsc),method="single")
plot(hc.complete.scale,main="Complete Linkage")
plot(hc.average.scale,main="Average Linkage")
plot(hc.single.scale,main="Single Linkage")

#cutting our trees; scaling to cluster of 4
cutree(hc.complete,4)
cutree(hc.average,4)
cutree(hc.single,4)

#euclidean tree
d<-dist(HUYGENS,method="euclidean")
hfit<-hclust(d,method="ward.D2")
plot(hfit)
groups<-cutree(hfit,k=5)
rect.hclust(hfit,k=5,border="green")

fviz_dend(hfit, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

res.hc3 <- eclust(HUYGENS,"hclust", k = 3, graph = FALSE)
res.hc4 <- eclust(HUYGENS,"hclust", k = 4, graph = FALSE)
res.hc5 <- eclust(HUYGENS,"hclust", k = 5, graph = FALSE)

#=============================================================================
#Silhouette Plot

km_res<-kmeans(HUYGENS,centers=4,nstart=20) #centers is number of clusters
sil<-silhouette(km_res$cluster,dist(HUYGENS))
fviz_silhouette(sil)
#  cluster size ave.sil.width
#1       1  750          0.66
#2       2  181          0.69
#3       3 1053          0.64
#4       4  867          0.87

km_res<-kmeans(HUYGENS,centers=5,nstart=20) #centers is number of clusters
sil<-silhouette(km_res$cluster,dist(HUYGENS))
fviz_silhouette(sil)
#  cluster size ave.sil.width
#1       1  181          0.68
#2       2  511          0.61
#3       3  867          0.86
#4       4  634          0.63
#5       5  658          0.59

km_res<-kmeans(HUYGENS,centers=3,nstart=20) #centers is number of clusters
sil<-silhouette(km_res$cluster,dist(HUYGENS))
fviz_silhouette(sil)
#  cluster size ave.sil.width
#1       1 1803          0.73
#2       2  867          0.89
#3       3  181          0.74

names(HUYGENS)
#[1] "HUYGENS.ALTITUDE..KM."        "ALTITUDE..M..x.x"            
#[3] "TEMPERATURE..K."              "TEMP.UNCERTAINTY..K."        
#[5] "TOTAL.PRESSURE..Pa."          "AMBIENT.PRESSURE..Pa."       
#[7] "ALTITUDE..M..y"               "DESCENT.VELOCITY..M.S."      
#[9] "ALTITUDE..M..x.y"             "ZONAL.WIND.SPEED..M.S."      
#[11] "ZONAL.WIND.SPEED.ERROR..M.S." "X.CH4."

#==============================================================================
#Hybrid 3D Clustering in Factor Map
library(FactoMineR)
# Compute PCA with ncp = 3
res.pca <- PCA(HUYGENS, ncp = 3, graph = FALSE)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE)
# Principal components + tree
plot(res.hcpc, choice = "3D.map")
#==============================================================================
#Model 3 - Fuzzy Clustering
#items can be members in multiple clusters

#4 cluster
HUYGENSscale <- scale(HUYGENS)     # Standardize the data
res.fanny <- fanny(HUYGENSscale, 4)  # Compute fuzzy clustering with k = 2
library(factoextra)
fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right", main="Fuzzy 4 Cluster HUYGENS")
#5 cluster
HUYGENSscale <- scale(HUYGENS)     # Standardize the data
res.fanny <- fanny(HUYGENSscale, 5)  # Compute fuzzy clustering with k = 2
library(factoextra)
fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right", main="Fuzzy 5 Cluster HUYGENS")

#3 cluster
HUYGENSscale <- scale(HUYGENS)     # Standardize the data
res.fanny <- fanny(HUYGENSscale, 3)  # Compute fuzzy clustering with k = 2
library(factoextra)
fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right", main="Fuzzy 3 Cluster HUYGENS")
#3 is not good
#==============================================================================
#Model 4 - DBSCAN - Density Based Clustering

#5 cluster
km.res <- kmeans(HUYGENS, 5, nstart = 25)
fviz_cluster(km.res, HUYGENS, frame = FALSE, geom = "point", main="DBSCAN HUYGENS 5 Cluster")
#4 cluster
km.res <- kmeans(HUYGENS, 4, nstart = 25)
fviz_cluster(km.res, HUYGENS, frame = FALSE, geom = "point", main="DBSCAN HUYGENS 4 Cluster")
#3 cluster
km.res <- kmeans(HUYGENS, 3, nstart = 25)
fviz_cluster(km.res, HUYGENS, frame = FALSE, geom = "point", main="DBSCAN HUYGENS 3 Cluster")
#==============================================================================
#Comparison of Clusters - RAND Index
install.packages("fossil")
library(fossil)
#Types of clustering done:
#1)KMeans, 2)Hierarchical Clustering, 3)Partition Clustering, 
#4)Fuzzy Clustering, 5)Density Based Clustering

#KMeans vs Fuzzy=============================================
#KMeans 4 cluster vs Fuzzy 4 cluster
rand.index(HUYGENSKMeansresults$cluster,res.fanny$clustering)
#[1] 0.9933984
#KMeans 5 cluster vs Fuzzy 5 cluster
rand.index(HUYGENSKMeansresults02$cluster,res.fanny$clustering)
#[1] 0.9824712
#KMeans 3 cluster vs Fuzzy 3 cluster
rand.index(HUYGENSKMeansresults01$cluster,res.fanny$clustering)
#[1] 0.7889745

#KMeans vs DBSCAN============================================
#KMeans 4 cluster vs DBSCAN 4 cluster
rand.index(HUYGENSKMeansresults$cluster,km.res$cluster)
#[1] 1
#KMeans 5 cluster vs DBSCAN 5 cluster
rand.index(HUYGENSKMeansresults02$cluster,km.res$cluster)
#[1] 1
#KMeans 3 cluster vs DBSCAN 3 cluster
rand.index(HUYGENSKMeansresults01$cluster,km.res$cluster)
#[1] 1

#Fuzzy vs DBSCAN=============================================
#Fuzzy 4 cluster vs DBSCAN 4 cluster
rand.index(res.fanny$clustering,km.res$cluster)
#[1] 0.9933984
#Fuzzy 5 cluster vs DBSCAN 5 cluster
rand.index(res.fanny$clustering,km.res$cluster)
#[1] 0.9824712
#Fuzzy 3 cluster vs DBSCAN 3 cluster
rand.index(res.fanny$clustering,km.res$cluster)
#[1] 0.7889745

#KMeans vs Hierarchical Clustering===========================
#KMeans 4 cluster vs hierarchical 4 clustering
rand.index(HUYGENSKMeansresults$cluster,res.hc4$cluster)
#[1] 0.9518317
#KMeans 5 cluster vs hierarchical 5 clustering
rand.index(HUYGENSKMeansresults02$cluster,res.hc5$cluster)
#[1] 0.9391819
#KMeans 3 cluster vs hierarchical 3 clustering
rand.index(HUYGENSKMeansresults01$cluster,res.hc3$cluster)
#[1] 1

#Hierarchical clustering vs Fuzzy============================
#hierarchical 4 clustering vs Fuzzy 4 cluster
rand.index(res.hc4$cluster,res.fanny$clustering)
#[1] 0.9576875
#hierarchical 5 clustering vs Fuzzy 5 cluster
rand.index(res.hc5$cluster,res.fanny$clustering)
#[1] 0.9216531
#hierarchical 3 clustering vs Fuzzy 3 cluster
rand.index(res.hc3$cluster,res.fanny$clustering)
#[1] 0.7889745

#Hierarchical clustering vs DBSCAN===========================
#Hierarchical 4 clustering vs DBSCAN 4 cluster
rand.index(res.hc4$cluster,km.res$cluster)
#[1] 0.9518317
#Hierarchical 5 clustering vs DBSCAN 5 cluster
rand.index(res.hc5$cluster,km.res$cluster)
#[1] 0.9391819
#Hierarchical 3 clustering vs DBSCAN 3 cluster
rand.index(res.hc3$cluster,km.res$cluster)
#[1] 1
#==============================================================================


