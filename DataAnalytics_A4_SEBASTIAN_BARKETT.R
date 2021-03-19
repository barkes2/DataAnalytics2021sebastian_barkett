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
View(queensTC1)
summary(queensTC1)
queensTC2<-subset(queens, TAX.CLASS.AT.TIME.OF.SALE=='2')
View(queensTC2)
summary(queensTC1)
queensTC3<-subset(queens, TAX.CLASS.AT.TIME.OF.SALE=='3')
View(queensTC3)
summary(queensTC1)
head(queens)
queensTC4<-subset(queens, TAX.CLASS.AT.TIME.OF.SALE=='4')
View(queensTC4)
#Only identifiable issue is the lack of n in queensTC3 dataframe
#anyways next step is going to be to filter out gross square feet of 0 for each
queensTC1$GROSS.SQUARE.FEET[queensTC1$GROSS.SQUARE.FEET==0]<-NA
tf0<-is.na(queensTC1$GROSS.SQUARE.FEET)
queensTC1_GSFNZ<-queensTC1$GROSS.SQUARE.FEET[!tf0]
View(queensTC1_GSFNZ)
