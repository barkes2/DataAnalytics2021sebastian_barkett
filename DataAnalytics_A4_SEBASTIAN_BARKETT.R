#Determining which factor influences sale price more for each tax class?
#GROSS.SQUARE.FEET or YEAR.BUILT?
queens<-read.csv("rollingsales_queens.csv")
attach(queens)
head(queens)
#View(queens)
#How many tax classes live in queens?
summary(TAX.CLASS.AT.TIME.OF.SALE)
#only 4 tax classes represented in queens?
#Lets start by converting rows with zero values for GROSS.SQUARE.FEET to NA
#and then removing them in an updated data frame
queens$GROSS.SQUARE.FEET[queens$GROSS.SQUARE.FEET==0]<-NA
tf<-is.na(queens$GROSS.SQUARE.FEET)
queensGSFNZ<-queens$GROSS.SQUARE.FEET[!tf]
queensGSFNZ
#New data frame created that only has non-zero values for GROSS.SQUARE.FEET
