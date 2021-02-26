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
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<-predict(lmENVH,NEW,interval="prediction")
cENV<-predict(lmENVH,NEW,interval="confidence")

#repeat these for the variables AIR_E and CLIMATE

#Due on March 4th by 11:59 on LMS link available on Feb 25th