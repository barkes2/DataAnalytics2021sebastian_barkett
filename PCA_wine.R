#Work on code snippets 
winedata<-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/w
                     ine/wine.data",
sep=",")
head(winedata)
#manually assigning names for each column, need to read documentation
nrow(winedata)
colnames(winedata)<-c("Cvs", "Alcohol","Malic_Acid", "Ash", "Alkalinity_of_Ash",
                      "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_
                      Phenols","Proanthocyanins", "Color_Intensity", "Hue", "OD2
                      80/OD315_of_Diluted_Wine","Proline")
head(winedata)
help("heatmap") #reading documentation on heatmap function
heatmap(cor(winedata),Rowv=NA,Colv=NA) #generating a heatmap showing relation
#ships
cultivar_classes<-factor(winedata$Cvs) #declaring the cultivar_classes using the 
#factor() function each cultivar Cv1, Cv2, Cv3
cultivar_classes
winedata_PCA<-prcomp(scale(winedata[,-1]))
summary(winedata_PCA)
#we can see PC1 gives the 36.2% cumulative contribution, which indicates
#that PC1 represents 36.2% variance