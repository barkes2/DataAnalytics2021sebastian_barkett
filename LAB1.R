#reminder that your pathway is in Documents/Downloads/<filename> so go about trx all files to folder
#read in csv file - 
GPW3_GRUMP <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv")
View(GPW3_GRUMP)
#or read in excel file directly or by 
#csv convert - 2010EPI_data.xls #(EPI2010_all countries or EPI2010_onlyEPIcountries tabs)
#also for other datasets enter these in R command window panel or cmd line
data() #information related to R datasets
help(data) #opens help window for data sets
EPI_data<-read.csv("2010EPI_data.csv") #reads in .csv file from the pathway you outline. check your working directory with command "getwd()"
#Note: replace default data frame name - cannot start with numbers! Munging
#Note: replace <path> with either a directory path or use setwd("<path>")
View(EPI_data) #opens window with EPI_data for visual inspecting/to begin munging the dataset
attach(EPI_data) #set the 'default' object
fix(EPI_data) #launches a simple data editor - very easy to use
EPI #prints out values EPI_data$EPI
tf <- is.na(EPI) #records True value is NA
E <- EPI[!tf] #filters out NA values, new array
summary(EPI) #stats
fivenum(EPI,na.rm=TRUE) #returns Tukey's five number summary (min,lower hinge, median, upper-hinge, max) for the data
#if na.rm=TRUE all NA and NaN values are dropped before the statistics are computed
stem(EPI) #stem and leaf plot
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.)) # or try bw="SJ"
rug(EPI)
#Use help(<comman>), e.g. >help(stem)
#See group1/lab1_summary.R
#Save your plots, name them
#Save commands you used to generate them
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) #cumulative density function
par(pty="s")
qqnorm(EPI); qqline(EPI) #Quantile-Quantile?
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn") #make a Q-Q plot against the generating dist by: x<-seq(30,95,1)
qqline(x)
#Excercise 1: fitting a distribution
#do the same exploration and fitting for another 2 variables in the EPI_data
#(i.e. primary variables (DALY, WATER_H, ...)
#Try fitting other distributions - i.e. as ecdf or qq-
boxplot(EPI,DALY) #Comparing distributions
qqplot(EPI,DALY)
#intercompare: EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_EWATER_E, BIODIVERSITY **(subject to possible filtering)
#Note the 2010 and 2016 datasets
#See archive "alldist.zip" or individual files in /distribution
#Excel files for dif distributions; expanded in the folder: e.g. lognorm.xls
help(distributions) #In R

#Excercise 2 Conditional Filtering (below)
EPILand<-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)
#Repeat Excersize 1 look at: No_surface_water, Desert and High_Population_Density

#My Excercise: how to filter on EPI_regions or GEO_subregion?
#E.g. EPI_South_Asia <- EPI[<what is this>]
#GPW3_GRUMP - repeat: the reading in, then exploration/summary, plots, histograms, distributions, filtering
#water-treatment.csv #water_treatment 

#Lab objectives: Get familiar with Distributions, Populations, Fitting, & Creating Data Frames in R