#Determining which factor influences sale price more for each tax class?
#GROSS.SQUARE.FEET or YEAR.BUILT?
queens<-read.csv("rollingsales_queens.csv")
attach(queens)
#View(queens)
#How many tax classes live in queens?
summary(TAX.CLASS.AT.TIME.OF.SALE)
#only 4 tax classes represented in queens
#Using data frame indexing to create 4 data frames for each tax class 
#at time of sale
head(queens)
queensTC1<-subset(queens, TAX.CLASS.AT.TIME.OF.SALE=='1')
#View(queensTC1)
summary(queensTC1)
queensTC2<-subset(queens, TAX.CLASS.AT.TIME.OF.SALE=='2')
#View(queensTC2)
summary(queensTC2)
queensTC3<-subset(queens, TAX.CLASS.AT.TIME.OF.SALE=='3')
#View(queensTC3)
summary(queensTC3)
queensTC4<-subset(queens, TAX.CLASS.AT.TIME.OF.SALE=='4')
#View(queensTC4)
summary(queensTC4)
#Only identifiable issue is the lack of n in queensTC3 dataframe
#anyways next step is going to be to filter out gross square feet of 0 for each
head(queensTC1)
#set the zeros to NA
queensTC1$GROSS.SQUARE.FEET[queensTC1$GROSS.SQUARE.FEET==0]<-NA
queensTC1$GROSS.SQUARE.FEET
#Delete the associated rows
queensTC1_GSFNZ<-queensTC1[complete.cases(queensTC1),]
#View(queensTC1_GSFNZ)
#now lets do this for the other 4 tax classes: P.S. Im going to try to use less
#lines/explanations for this part; P.S.S. TC3 has no relevant data to my questi
queensTC2$GROSS.SQUARE.FEET[queensTC2$GROSS.SQUARE.FEET==0]<-NA
queensTC2$GROSS.SQUARE.FEET
queensTC2_GSFNZ<-queensTC2[complete.cases(queensTC2),]
#View(queensTC2_GSFNZ)
queensTC4$GROSS.SQUARE.FEET[queensTC4$GROSS.SQUARE.FEET==0]<-NA
queensTC4$GROSS.SQUARE.FEET
queensTC4_GSFNZ<-queensTC4[complete.cases(queensTC4),]
#View(queensTC4_GSFNZ)
#So now there are only three Tax classes we can assess for queens: TC1,TC2,& TC4
#Similarly to before, Any zero values in these new dataframes for Year.Built or 
#Sale.Price need to go
queensTC1_GSFNZ$YEAR.BUILT[queensTC1_GSFNZ$YEAR.BUILT==0]<-NA
queensTC1_GSFNZ$YEAR.BUILT
#Delete the associated rows
queensTC1_GSFNZ_YBNZ<-queensTC1_GSFNZ[complete.cases(queensTC1_GSFNZ),]
View(queensTC1_GSFNZ_YBNZ)
queensTC2_GSFNZ$YEAR.BUILT[queensTC2_GSFNZ$YEAR.BUILT==0]<-NA
queensTC2_GSFNZ$YEAR.BUILT
queensTC2_GSFNZ_YBNZ<-queensTC2_GSFNZ[complete.cases(queensTC2_GSFNZ),]
View(queensTC2_GSFNZ_YBNZ)
queensTC4_GSFNZ$YEAR.BUILT[queensTC4_GSFNZ$YEAR.BUILT==0]<-NA
queensTC4_GSFNZ$YEAR.BUILT
queensTC4_GSFNZ_YBNZ<-queensTC4_GSFNZ[complete.cases(queensTC4_GSFNZ),]
View(queensTC4_GSFNZ_YBNZ)
#Now that all NonZero values for YEAR.BUILT are gone, lets remove zeros for
#SALE.PRICE
#First we have to remove the shitty "$" and "," symbol in the SALE.PRICE column
queensTC1_GSFNZ_YBNZ$SALE.PRICE2=as.numeric(gsub("[$,]","",queensTC1_GSFNZ_YBNZ$SALE.PRICE))
queensTC1_GSFNZ_YBNZ$SALE.PRICE2[queensTC1_GSFNZ_YBNZ$SALE.PRICE2==0]<-NA
queensTC1_GSFNZ_YBNZ$SALE.PRICE2
queensTC1_GSFNZ_YBNZ_SPNZ<-queensTC1_GSFNZ_YBNZ[complete.cases(queensTC1_GSFNZ_YBNZ),]
View(queensTC1_GSFNZ_YBNZ_SPNZ)
#debatably left in values of "1" and "10" as the sale price but I do not want 
#to be the judge of removing these as well. Just zero values
queensTC2_GSFNZ_YBNZ$SALE.PRICE2=as.numeric(gsub("[$,]","",queensTC2_GSFNZ_YBNZ$SALE.PRICE))
queensTC2_GSFNZ_YBNZ$SALE.PRICE2[queensTC2_GSFNZ_YBNZ$SALE.PRICE2==0]<-NA
queensTC2_GSFNZ_YBNZ$SALE.PRICE2
queensTC2_GSFNZ_YBNZ_SPNZ<-queensTC2_GSFNZ_YBNZ[complete.cases(queensTC2_GSFNZ_YBNZ),]
View(queensTC2_GSFNZ_YBNZ_SPNZ)
queensTC4_GSFNZ_YBNZ$SALE.PRICE2=as.numeric(gsub("[$,]","",queensTC4_GSFNZ_YBNZ$SALE.PRICE))
queensTC4_GSFNZ_YBNZ$SALE.PRICE2[queensTC4_GSFNZ_YBNZ$SALE.PRICE2==0]<-NA
queensTC4_GSFNZ_YBNZ$SALE.PRICE2
queensTC4_GSFNZ_YBNZ_SPNZ<-queensTC4_GSFNZ_YBNZ[complete.cases(queensTC4_GSFNZ_YBNZ),]
View(queensTC4_GSFNZ_YBNZ_SPNZ)
queensTC1_GSFNZ_YBNZ_SPNZ$GROSS.SQUARE.FEET2=as.numeric(gsub("[,]","",queensTC1_GSFNZ_YBNZ_SPNZ$GROSS.SQUARE.FEET))
queensTC2_GSFNZ_YBNZ_SPNZ$GROSS.SQUARE.FEET2=as.numeric(gsub("[,]","",queensTC2_GSFNZ_YBNZ_SPNZ$GROSS.SQUARE.FEET))
queensTC4_GSFNZ_YBNZ_SPNZ$GROSS.SQUARE.FEET2=as.numeric(gsub("[,]","",queensTC4_GSFNZ_YBNZ_SPNZ$GROSS.SQUARE.FEET))
#Simplifying our dataframe names
q1<-queensTC1_GSFNZ_YBNZ_SPNZ
q2<-queensTC2_GSFNZ_YBNZ_SPNZ
q4<-queensTC4_GSFNZ_YBNZ_SPNZ
#So now all three of our variables are non-zero (GROSS.SQUARE.FEET,SALE.PRICE2, 
#& YEAR.BUILT) and sorted into our three tax classes with n>0
#lets start with histograms
par(mfrow=c(2,2))
q1GSF<-q1$GROSS.SQUARE.FEET2
q1GSF
hist(q1GSF,breaks=100,ylab="Count of Households in Gross Square Feet Bracket",
     xlab="Gross Square Feet in Queens Tax Class 1",main="Histogram of Gross 
     Square Feet for Tax Class 1 in Queens")
summary(q1GSF)
q2GSF<-q2$GROSS.SQUARE.FEET2
q2GSF
hist(q2GSF,breaks=40,ylab="Count of Households in Gross Square Feet Bracket",
     xlab="Gross Square Feet in Queens Tax Class 2",main="Histogram of Gross 
     Square Feet for Tax Class 2 in Queens")
summary(q2GSF)
#class 2 properties are considered income generating properties
q4GSF<-q4$GROSS.SQUARE.FEET2
q4GSF
hist(q4GSF,breaks=100,ylab="Count of Households in Gross Square Feet Bracket",
     xlab="Gross Square Feet in Queens Tax Class 4",main="Histogram of Gross 
     Square Feet for Tax Class 4 in Queens")
summary(q1GSF)
#Lets make Histograms for Sale.Price
par(mfrow=c(2,2))
q1SP<-q1$SALE.PRICE2
q1SP
hist(q1SP,breaks=45,ylab="Count of Sale Price Bracket",
     xlab="Sale Price in Queens Tax Class 1",main="Histogram of Sale Price for 
     Tax Class 1 in Queens")
summary(q1SP)
q2SP<-q2$SALE.PRICE2
q2SP
hist(q2SP,breaks=90,ylab="Count of Sale Price Bracket",
     xlab="Sale Price in Queens Tax Class 2",main="Histogram of Sale Price for 
     Tax Class 2 in Queens")
summary(q2SP)
q4SP<-q4$SALE.PRICE2
q4SP
hist(q4SP,breaks=90,ylab="Count of Sale Price Bracket",
     xlab="Sale Price in Queens Tax Class 4",main="Histogram of Sale Price for 
     Tax Class 4 in Queens")
summary(q4SP)
#Finally lets run a histogram for year.built
par(mfrow=c(2,2))
q1YB<-q1$YEAR.BUILT
q1YB
hist(q1YB,breaks=45,ylab="Count in Year Bracket",
     xlab="Year Built Queens Tax Class 1",main="Histogram of Year Built for 
     Tax Class 1 in Queens")
summary(q1YB)
q2YB<-q2$YEAR.BUILT
q2YB
hist(q2YB,breaks=40,ylab="Count in Year Bracket",
     xlab="Year Built Queens Tax Class 2",main="Histogram of Year Built for 
     Tax Class 2 in Queens")
summary(q2YB)
q4YB<-q4$YEAR.BUILT
q4YB
hist(q4YB,breaks=30,ylab="Count in Year Bracket",
     xlab="Year Built Queens Tax Class 4",main="Histogram of Year Built for 
     Tax Class 2 in Queens")
summary(q4YB)
#Now that we have looked at all of the distributions for our data/variables
#lets plot the ECDF for Sale.Price X Gross.Square.Feet and Sale.Price X 
#Year.Built
library(gridExtra)
library(grid)
library(ggplot2)
g<-ggplot(q1, aes(x=SALE.PRICE2,y=GROSS.SQUARE.FEET2))
g+stat_ecdf()
g2<-ggplot(q2, aes(x=SALE.PRICE2,y=GROSS.SQUARE.FEET2))
g2+stat_ecdf()
g4<-ggplot(q4, aes(x=SALE.PRICE2,y=GROSS.SQUARE.FEET2))
g4+stat_ecdf()
d<-data.frame(x=c(0,9E+7))
ll<-Map(f=stat_function,colour=c('red','green','blue'),
        fun=list(g,g2,g4),geom='step')
ggplot(data=d,aes(x=x))+ll
