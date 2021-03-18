#Determining which factor influences sale price more for each tax class?
#GROSS.SQUARE.FEET or YEAR.BUILT?
queens<-read.csv("rollingsales_queens.csv")
attach(queens)
head(queens)
#View(queens)
#How many tax classes live in queens?
summary(TAX.CLASS.AT.TIME.OF.SALE)
#only 4 classes represented in queens?
