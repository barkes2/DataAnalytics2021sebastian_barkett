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
#So now all three of our variables are non-zero (GROSS.SQUARE.FEET,SALE.PRICE2, 
#& YEAR.BUILT) and sorted into our three tax classes with n>0
