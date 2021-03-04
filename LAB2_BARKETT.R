#useful commands here: 
#a) head(<object>) command returns the first part of an object
#b) tail (<object>) command returns the last part of an object
#c) summary(<object>)

#Measures of Central Tendency for the EPI dataset:
# 1) Generate Central Tendency values for EPI variable
getwd()
EPI_data<-read.csv("EPI_data.csv")#reads in the csv file from the pathway you oultine. check from "getwd()"
attach(EPI_data) #sets the default object
View(EPI_data) #opens window with EPI data
fix(EPI_data) #opens editing window
EPI #displays values from the EPI column in the EPI_data; lots of NA values to be masked
tf <- is.na(EPI) #records true value as NA
E<-EPI[!tf] #filters out these NA values making a new array
E #just so you can view the new array and show that it is missing NA values
summary(E) #displays stats about the EPI data in array without NA values
fivenum(E,na.rm=TRUE) #similar to above: returns Tukey's five number summary for the data (min,lower hinge, median, upper-hinge, max)
#values should be:
#Min.   1st Qu.   Median    Mean    3rd Qu.   Max.
#32.10  48.60     59.20     58.37   67.60     93.50
#how do we find the mode?
# a)create a function
mode<-function(v) {
  uniqv<-(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
# b) create the vector with numbers
v<-EPI[!tf]
# c) calculate the mode using the user function
result<-mode(v)
print(result)
#the mode of EPI is:
#> print(result)
#[1] 44.6
#Central Tendency Values of the EPI column: 
#Mean is 58.37; Median is 59.20; and the Mode is 44.6
#=========================================================================================================================================
# 2) Generate Central Tendency values for DALY variable
getwd()
EPI_data<-read.csv("EPI_data.csv")#reads in the csv file from the pathway you oultine. check from "getwd()"
attach(EPI_data) #sets the default object
View(EPI_data) #opens window with EPI data
fix(EPI_data) #opens editing window
DALY #displays values from the DALY column in the EPI_data; lots of NA values to be masked
tf <- is.na(DALY) #records true value as NA
D<-DALY[!tf] #filters out these NA values making a new array
D #just so you can view the new array and show that it is missing NA values
summary(D) #displays stats about the EPI data in array without NA values
fivenum(D,na.rm=TRUE) #similar to above: returns Tukey's five number summary for the data (min,lower hinge, median, upper-hinge, max)
#values should be:
#Min.   1st Qu. Median  Mean    3rd Qu. Max. 
#0.00   37.19   60.35   53.94   71.97   91.50 
#how do we find the mode?
# a)create a function
mode<-function(z) {
  uniqv<-(z)
  uniqv[which.max(tabulate(match(z,uniqv)))]
}
# b) create the vector with numbers
z<-DALY[!tf]
# c) calculate the mode using the user function
result<-mode(z)
print(result)
#> print(result)
#[1] 86.86
#Central Tendency Values of the DALY column:
#Mean is 53.94; Median is 60.35; and the Mode is 86.86
#=========================================================================================================================================
#Generate Histograms for EPI and DALY variables in the EPI 2010 dataset
# 1) Generate the Histogram for EPI variable
EPI_data<-read.csv("2010EPI_data.csv") #reads in .csv file from the pathway you outline. check your working directory with command "getwd()"
#Note: replace default data frame name - cannot start with numbers! Munging
#Note: replace <path> with either a directory path or use setwd("<path>")
attach(EPI_data) #set the 'default' object
EPI #prints out values EPI_data$EPI
tf <- is.na(EPI) #records True value is NA
E <- EPI[!tf] #filters out NA values, new array
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.)) # or try bw="SJ"
lines(density(EPI,na.rm=TRUE,bw="SJ"))
rug(EPI)

# 2) Genreate the Histogram for the DALY variable
EPI_data<-read.csv("2010EPI_data.csv") #reads in .csv file from the pathway you outline. check your working directory with command "getwd()"
#Note: replace default data frame name - cannot start with numbers! Munging
#Note: replace <path> with either a directory path or use setwd("<path>")
attach(EPI_data) #set the 'default' object
DALY #prints out values EPI_data$EPI
tf <- is.na(DALY) #records True value is NA
D <- DALY[!tf] #filters out NA values, new array
hist(DALY)
hist(DALY, seq(30., 95., 1.0), prob=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.)) # or try bw="SJ"
lines(density(DALY,na.rm=TRUE,bw="SJ"))
rug(DALY)

#below two are in notes and are shown as running; unsure if part of lab but here they are
boxplot(ENVHEALTH,ECOSYSTEM)
#Generates the box plot for the variables ENVHEALTH and ECOSYSTEM
qqplot(ENVHEALTH,ECOSYSTEM)
#generates the Q-Q plot for the variables ENVHEALTH and ECOSYSTEM
#========================================================================================================================================
#Regression Exercises:
#Using the EPI (under /EPI on web) dataset find the single most important factor in increasing the EPI in a given region

#Linear and least-squares

#some independent comparisons

#EPI vs ENVHEALTH
getwd()
EPI_data<-read.csv("EPI_data.csv")#reads in the csv file from the pathway you oultine. check from "getwd()"
attach(EPI_data) #sets the default object
View(EPI_data) #opens window with EPI data
fix(EPI_data) #opens editing window
EPI #displays values from the EPI column in the EPI_data; lots of NA values to be masked
tf <- is.na(EPI) #records true value as NA
E<-EPI[!tf] #filters out these NA values making a new array
E #just so you can view the new array and show that it is missing NA values
#install.packages("car")
require(car)
model1<-lm(ENVHEALTH~EPI) #Regression; Y = m * X2 + b (equation for a line)
summary(model1)
#Call:
#  lm(formula = ENVHEALTH ~ EPI)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-38.275 -10.924   1.105  10.147  48.864 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -30.9278     6.0801  -5.087    1e-06 ***
#  EPI           1.5656     0.1019  15.364   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 16.09 on 161 degrees of freedom
#(68 observations deleted due to missingness)
#Multiple R-squared:  0.5945,	Adjusted R-squared:  0.592 
#F-statistic:   236 on 1 and 161 DF,  p-value: < 2.2e-16

#Step 1: look at model and fit
plot(EPI,ENVHEALTH,xlab="EPI",ylab="ENVHEALTH",main="EPI & ENVHEALTH Trendline")
abline(model1)
segments(EPI,fitted(model1),EPI,ENVHEALTH)
#Step 2: Examine the residuals
plot(E,resid(model1)) #used E here as EPI was reading error on 'x' and 'y' lengths differing. Best guess is NA values if E works?
#Step 3: look at residuals, leverage, and influence
par(mfrow=c(2,2))#window with 2x2 plots
plot(model1)
#Step 4: check influence from Cook's distance
influenceIndexPlot(model1,id.n=3)
#Step 5: look at the hat values
plot(hatvalues(model1))

#EPI vs DALY
model2<-lm(DALY~EPI) #Regression; Y = m * X2 + b (equation for a line)
summary(model2)
#Call:
#  lm(formula = DALY ~ EPI)
#
#Residuals:
#  Min     1Q Median     3Q    Max 
#-41.53 -13.60  -0.63  11.01  61.87 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -33.5700     6.8339  -4.912  2.2e-06 ***
#  EPI           1.4938     0.1145  13.043  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 18.09 on 161 degrees of freedom
#(68 observations deleted due to missingness)
#Multiple R-squared:  0.5138,	Adjusted R-squared:  0.5107 
#F-statistic: 170.1 on 1 and 161 DF,  p-value: < 2.2e-16
plot(EPI,DALY,xlab="EPI",ylab="DALY",main="EPI & DALY Trendline")
abline(model2)
segments(EPI,fitted(model2),EPI,DALY)
par(mfrow=c(2,2))
plot(model2)
influenceIndexPlot(model2,id.n=3)
plot(hatvalues(model2))

#EPI vs AIR_H
model3<-lm(AIR_H~EPI) #Regression; Y = m * X2 + b (equation for a line)
summary(model3)
#Call:
#  lm(formula = AIR_H ~ EPI)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-48.787 -11.896   1.497  12.813  63.033 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -31.7084     7.0901  -4.472 1.46e-05 ***
#  EPI           1.6390     0.1188  13.793  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 18.77 on 161 degrees of freedom
#(68 observations deleted due to missingness)
#Multiple R-squared:  0.5416,	Adjusted R-squared:  0.5388 
#F-statistic: 190.3 on 1 and 161 DF,  p-value: < 2.2e-16
plot(EPI,AIR_H,xlab="EPI",ylab="AIR_H",main="EPI & AIR_H Trendline")
abline(model3)
segments(EPI,fitted(model3),EPI,AIR_H)
par(mfrow=c(2,2))
plot(model3)
influenceIndexPlot(model3,id.n=3)
plot(hatvalues(model3))

#EPI vs WATER_H
model4<-lm(WATER_H~EPI) #Regression; Y = m * X2 + b (equation for a line)
summary(model4)
#Call:
#  lm(formula = WATER_H ~ EPI)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-50.678 -14.894   0.908  14.807  56.614 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -24.8595     7.9687   -3.12  0.00215 ** 
#  EPI           1.6355     0.1336   12.25  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 21.09 on 161 degrees of freedom
#(68 observations deleted due to missingness)
#Multiple R-squared:  0.4823,	Adjusted R-squared:  0.4791 
#F-statistic:   150 on 1 and 161 DF,  p-value: < 2.2e-16
plot(EPI,WATER_H,xlab="EPI",ylab="WATER_H",main="EPI & WATER_H Trendline")
abline(model4)
segments(EPI,fitted(model4),EPI,WATER_H)
par(mfrow=c(2,2))
plot(model4)
influenceIndexPlot(model4,id.n=3)
plot(hatvalues(model4))

#OTHER ITEMS
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH<-lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
#Call:
#  lm(formula = ENVHEALTH ~ DALY + AIR_H + WATER_H)
#
#Coefficients:
#(Intercept)   DALY         AIR_H        WATER_H  
#-1.458e-05    5.000e-01    2.500e-01    2.500e-01  
summary(lmENVH)
#Call:
#  lm(formula = ENVHEALTH ~ DALY + AIR_H + WATER_H)
#
#Residuals:
#  Min         1Q     Median         3Q        Max 
#-0.0073210 -0.0027069 -0.0000915  0.0022285  0.0053404 
#
#Coefficients:
#  Estimate Std. Error   t value Pr(>|t|)    
#(Intercept) -1.458e-05  6.520e-04    -0.022    0.982    
#DALY         5.000e-01  1.988e-05 25147.716   <2e-16 ***
#  AIR_H        2.500e-01  1.276e-05 19593.273   <2e-16 ***
#  WATER_H      2.500e-01  1.816e-05 13764.921   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.003015 on 159 degrees of freedom
#Multiple R-squared:      1,	Adjusted R-squared:      1 
#F-statistic: 3.77e+09 on 3 and 159 DF,  p-value: < 2.2e-16
cENVH<-coef(lmENVH)
cENVH
#(Intercept)          DALY         AIR_H       WATER_H 
#-1.458233e-05  5.000248e-01  2.499979e-01  2.499861e-01 

#Predict
DALYNEW<-c(seq(5,95,5))
DALYNEW
#[1]  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95
AIR_HNEW<-c(seq(5,95,5))
AIR_HNEW
#[1]  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95
WATER_HNEW<-c(seq(5,95,5))
WATER_HNEW
#[1]  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
NEW
#DALYNEW AIR_HNEW WATER_HNEW
#1        5        5          5
#2       10       10         10
#3       15       15         15
#4       20       20         20
#5       25       25         25
#6       30       30         30
#7       35       35         35
#8       40       40         40
#9       45       45         45
#10      50       50         50
#11      55       55         55
#12      60       60         60
#13      65       65         65
#14      70       70         70
#15      75       75         75
#16      80       80         80
#17      85       85         85
#18      90       90         90
#19      95       95         95
pENV<-predict(lmENVH,NEW,interval="prediction")
pENV #readout of 'fit' 'lwr' and 'upr'
cENV<-predict(lmENVH,NEW,interval="confidence")
cENV #readout of 'fit' 'lwr' and 'upr'
#repeat these for the variables AIR_E and CLIMATE

model5<-lm(EPI~AIR_E+CLIMATE) #Regression; Y = m * X2 + b (equation for a line)
summary(model5)
#Call:
#  lm(formula = EPI ~ AIR_E + CLIMATE)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-27.070 -10.028   1.681   9.104  30.883 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 61.47701    4.55071  13.509   <2e-16 ***
#  AIR_E       -0.14659    0.07844  -1.869   0.0635 .  
#CLIMATE      0.07489    0.06219   1.204   0.2303    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 12.33 on 160 degrees of freedom
#(68 observations deleted due to missingness)
#Multiple R-squared:  0.02456,	Adjusted R-squared:  0.01237 
#F-statistic: 2.015 on 2 and 160 DF,  p-value: 0.1367
plot(EPI,AIR_E,xlab="EPI",ylab="AIR_E",main="EPI & AIR_E Trendline")
abline(model5)
segments(EPI,fitted(model5),EPI,AIR_E)
plot(EPI,CLIMATE,xlab="EPI",ylab="CLIMATE",main="EPI & CLIMATE Trendline")
abline(model5)
segments(EPI,fitted(model5),EPI,CLIMATE)
par(mfrow=c(2,2))
plot(model5)
influenceIndexPlot(model5,id.n=3)
plot(hatvalues(model5))

#Predict
AIR_ENEW<-c(seq(5,95,5))
AIR_ENEW
#[1]  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95
CLIMATENEW<-c(seq(5,95,5))
CLIMATENEW
#[1]  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95
NEWer<-data.frame(AIR_ENEW,CLIMATENEW)
NEWer
cEPI<-coef(model5)
cEPI
lmEPI<-lm(EPI~AIR_E+CLIMATE) 
#pEPI<-predict(lmEPI,NEWer,interval="prediction")
#pEPI
#coEPI<-predict(lmEPI,NEWer,interval="confidence")
#coEPI
#===============================================================================

#LAB2 PART2
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