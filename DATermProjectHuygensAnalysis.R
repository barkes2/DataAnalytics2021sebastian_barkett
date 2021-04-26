huygenstemp<-read.csv("HASI_L4_TEM_TEMPERATURE-TAB.csv")
huygenspres<-read.csv("HASI_L4_PPI_PRESSURE_VEL-TAB.csv")
head(huygenstemp)
head(huygenspres)
htp<-merge(huygenstemp,huygenspres,by=c('TIME..MILLISECONDS.'),all.x=T)
View(htp)
write.csv(htp,"C:/Users/sdbar/Box Sync/HUYGENS/csv FOR DA\\HuygensPresTempMerge.csv",row.names=FALSE)

huygenszonalwind<-read.csv("ZONALWIND-TAB.csv",stringsAsFactors = FALSE)
huygensmolefraction<-read.csv("GCMS_MOLE_FRACTION_STG2-TAB.csv", stringsAsFactors = FALSE)
huygenszonalwind$UTCABSTIME<-as.character(huygenszonalwind$X..........UTC_ABS_TIME.)
huygenszonalwind$UTCABSTIME<-gsub('2005-01-14T','',huygenszonalwind$UTCABSTIME)
levels(huygenszonalwind$UTCABSTIME)<-gsub('2005-01-14T','',levels(huygenszonalwind$UTCABSTIME))
View(huygenszonalwind)
huygensmolefraction$UTCABSTIME<-as.character(huygensmolefraction$X..........UTC_ABS_TIME.)
huygensmolefraction$UTCABSTIME<-gsub('2005-01-14T','',huygensmolefraction$UTCABSTIME)
levels(huygensmolefraction$UTCABSTIME)<-gsub('2005-01-14T','',levels(huygensmolefraction$UTCABSTIME))
View(huygensmolefraction)
write.csv(huygenszonalwind01,"C:/Users/sdbar/Box Sync/HUYGENS/csv FOR DA\\ZONALWIND-TAB02.csv",row.names=FALSE)
write.csv(huygensmolefraction01,"C:/Users/sdbar/Box Sync/HUYGENS/csv FOR DA\\GCMS_MOLE_FRACTION_STG2-TAB02.csv",row.names=FALSE)
huygenszonalwind01<-read.csv("ZONALWIND-TAB01.csv",stringsAsFactors = FALSE)
huygensmolefraction01<-read.csv("GCMS_MOLE_FRACTION_STG2-TAB01.csv", stringsAsFactors = FALSE)
head(huygenszonalwind01)
head(huygensmolefraction01)
#need to reduce the number of digits in UTCABSTIME column to merge the two datasets

huygensmolefraction01$UTCABSTIME<-gsub(':','',huygensmolefraction01$UTCABSTIME)
levels(huygensmolefraction01$UTCABSTIME)<-gsub(':','',levels(huygensmolefraction01$UTCABSTIME))
View(huygensmolefraction01)
#install dlyr
library(dplyr)



hwm<-merge(huygenszonalwind01,huygensmolefraction01,by=c('UTCABSTIME'),all.x=T)
write.csv(hwm,"C:/Users/sdbar/Box Sync/HUYGENS/csv FOR DA\\HuygensZonalWindMoleFracMerge01.csv",row.names=FALSE)
View(hwm)



#==============================================================================
View(htp)
#running a PCA on the Huygen's temp & pressure data set
#should I create a dataframe without the unneccesary columns? why not
install.packages("ggplot2")
library(ggplot2)
str(htp)
ncol(htp)
names(htp)
#[1] "TIME..MILLISECONDS."    "ALTITUDE..M..x"         "TEMPERATURE..K."       
#[4] "TEMP.UNCERTAINTY..K."   "REFERENCE.SENSOR.x"     "REFERENCE.SENSOR.y"    
#[7] "TOTAL.PRESSURE..Pa."    "AMBIENT.PRESSURE..Pa."  "ALTITUDE..M..y"        
#[10] "DESCENT.VELOCITY..M.S."
#add vector in new dataframe to select particular columns
htpnew<-htp[c(2:3,7:8,10)]
names(htpnew)
#[1] "ALTITUDE..M..x"         "TEMPERATURE..K."        "TOTAL.PRESSURE..Pa."   
#[4] "AMBIENT.PRESSURE..Pa."  "DESCENT.VELOCITY..M.S."
#now to remove rows with NA
htpnewnz<-htpnew[complete.cases(htpnew),]
View(htpnewnz)
#this excludes any surface collected data (long string of collections made on 
#surface of titan)
principal_components_htp<-princomp(htpnewnz,cor=TRUE,score=TRUE)
summary(principal_components_htp)
#looks like 97% of the variation in this dataset is explained by the first two 
#principal components
plot(principal_components_htp)
plot(principal_components_htp,type="l")
help(biplot)
biplot(principal_components_htp)
#total P and Ambient P share a strong correlation, Temp is closer related to 
#Altitude and descent velocity data
#==============================================================================
#Now a Pearson's parametric correlation analysis
head(htpnewnz)
names(htpnewnz)
attach(htpnewnz)
#two sided
cor.test(AMBIENT.PRESSURE..Pa.,TOTAL.PRESSURE..Pa.)
#        Pearson's product-moment correlation
#
#data:  AMBIENT.PRESSURE..Pa. and TOTAL.PRESSURE..Pa.
#t = 146041, df = 1814, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 1 1
#sample estimates:
#cor 
#  1 
#Pearson correlation coefficient finds these pressure values to be as strongly
#correlated as two variables can be; also positive correlation value
#lets make a matrix of all of these values, data is continuous so we will use
#Pearson's
htp.cor=cor(htpnewnz,method=c("pearson"))
View(htp.cor)
#install.packages("xlsx")
write.table(htp.cor,"C:/Users/sdbar/Box Sync/HUYGENS/csv FOR DA\\HuygensPressureTempPearsonMatrix.txt",sep="\t")
#after checking out the correlation matrix, it appears that both ambient and 
#total pressure are somewhat negatively correlated with altitude (m) indicating
#that as altitude decreases, pressure increases with Pearson coefficients of 
#roughly -0.77 for both
#As for Temperature and both pressure measurements it appears that these are 
#weakly negatively correlated with a Pearson coefficient of -0.274 for both
#as for our strongest relationship of temperature and altitude, we can see a 
#positive correlation value of roughly 0.803 meaning that as altitude decreases,
#temperature steadily drops along-side this
#===============================================================================
#Monte Carlo Simulation
#install.packages("MonteCarlo")
library(dplyr)
names(htpnewnz)
htp.sample<-sample_n(htpnewnz,300)
View(htp.sample)
#took a sample of 300 points from my dataframe containing non-zero/NA for all 
#columns
hist(htp.sample$ALTITUDE..M..x)
#distribution of altitude data has shifted torward lower altitude readings
N=2000
simulated_means_htp<-rep(htpnewnz, N)
par(mfrow=c(2,3))
hist(simulated_means_htp$ALTITUDE..M..x,xlab="Altitude (m)",main="Histogram of 
     Simulated Means for Altitude")
hist(simulated_means_htp$TEMPERATURE..K.,xlab="Temperature (K)",main="Histogram of 
     Simulated Means for Temperature")
hist(simulated_means_htp$TOTAL.PRESSURE..Pa.,xlab="Total Pressure (Pa)",main="Histogram of 
     Simulated Means for Total Pressure")
hist(simulated_means_htp$AMBIENT.PRESSURE..Pa.,xlab="Ambient Pressure (Pa)",main="Histogram of 
     Simulated Means for Ambient Pressure")
hist(simulated_means_htp$DESCENT.VELOCITY..M.S.,xlab="Descent Velocity (m/s)",main="Histogram of 
     Simulated Means for Descent Velocity")
#is standard dev needed?

simulated_means.Alt<-rep(htp.sample$ALTITUDE..M..x,2000)
head(simulated_means.Alt)
for (i in 1:N){
  sim_data.Alt<-htp.sample$ALTITUDE..M..x
  simulated_means.Alt[i]<-mean(sim_data.Alt)
  rm(sim_data.Alt)
}
hist(simulated_means.Alt)
sd(simulated_means.Alt)
#massive sd
