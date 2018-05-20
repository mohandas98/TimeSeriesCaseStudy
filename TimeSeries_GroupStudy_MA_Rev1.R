#install.packages("graphics")

#library(dplyr)
#library(graphics)

#Load the metadata
metadata<- read.csv("Global Superstore.csv", header = T, sep = ',',stringsAsFactors=FALSE)

#Convert Order.Date to Date
metadata$Order.Date<-as.Date(metadata$Order.Date,format="%d-%m-%Y")

#Extract the Month and Year information from the Order.Date

#Extract year and convert it to numeric
metadata$OrderDateYear<-as.numeric(format(metadata$Order.Date,"%Y"))

#Extract month and convert it to numeric
metadata$OrderDateMonth<-months.Date(metadata$Order.Date,abbreviate=TRUE)
metadata$OrderDateMonth<-match(metadata$OrderDateMonth,month.abb)



#Convert Market and Segment to factors
metadata$Market<-as.factor(metadata$Market)
metadata$Segment<-as.factor(metadata$Segment)

#Remove the other coloumns
colnames(metadata)
metadata<-metadata[,-(1:7)]
colnames(metadata)
metadata<-metadata[,-(2:5)]
colnames(metadata)
metadata<-metadata[,c(-3,-4,-5,-6,-7,-10,-12,-13)]
colnames(metadata)

#--------Determine top two Markets and Segment with best CV-------------

#Create data.frames for each combination of Market and Category

#-------------
# Market  
#--------------
# Africa   
# APAC
# Canada
# EMEA
# EU
# LATAM
# US 
#-------------
# Segment
#-------------
# Consumer 
# Corporate
# Home Office 

#Form a grid of the Market and Segment

grid <- expand.grid(.market=c("Africa","APAC","Canada","EMEA","EU","LATAM","US"),
      .segment=c("Consumer","Corporate","Home Office") )

#Find the number of permutations derived of Market and Segment
n<-nrow(grid)

#Create a data.frame to collect the results 
Mrkt<-character()
Sgmt<-character()
MeanProfit<-double()
SdProfit<-double()
cv<-double()
Result <- data.frame(Mrkt,Sgmt,MeanProfit,SdProfit,cv)
Result$Mrkt<-as.character(Result$Mrkt)
Result$Sgmt<-as.character(Result$Sgmt)

for(i in 1:n)
{
	#Filter rows for only the required Market and Segment
	temp<-filter(metadata,metadata$Market==grid$.market[i],metadata$Segment==grid$.segment[i])
	
	#Now group by year and month and summarize
    tempGrpd<-group_by(temp,OrderDateYear,OrderDateMonth)
    
    #Find the aggregated Sum for Profit
    tempSum <- summarize(tempGrpd, Profit = sum(Profit))

    #Find the coefficient of variation
    m <- mean(tempSum$Profit)
    sigma <- sd(tempSum$Profit)
    cv<-m/sigma

    #Store the result for comparison later
    Result[i,1]<-as.character(grid$.market[i])
    Result[i,2]=as.character(grid$.segment[i])
    Result[i,3] = m
    Result[i,4] = sigma
    Result[i,5] = cv



}

Result<-Result[order(Result$cv,decreasing = TRUE),]

Result

#      Mrkt        Sgmt MeanProfit  SdProfit        cv
# 5      EU    Consumer  3930.9939 2454.1398 1.6017807
# 2    APAC    Consumer  4642.0325 2934.3785 1.5819474
# 6   LATAM    Consumer  2513.1861 1662.4295 1.5117550
# 9    APAC   Corporate  2702.8591 1886.8305 1.4324864
# 12     EU   Corporate  2570.7079 1963.5252 1.3092309
# 13  LATAM   Corporate  1205.7379  978.0003 1.2328605
# 14     US   Corporate  1916.2320 1920.8479 0.9975969
# 7      US    Consumer  2794.1502 2828.7698 0.9877616
# 16   APAC Home Office  1738.4428 1818.3736 0.9560427
# 21     US Home Office  1256.2225 1377.0048 0.9122862
# 19     EU Home Office  1265.5845 1413.0343 0.8956502
# 20  LATAM Home Office   898.6486 1056.5392 0.8505587
# 1  Africa    Consumer   995.2521 1313.3196 0.7578140
# 3  Canada    Consumer   230.4214  321.5098 0.7166855
# 10 Canada   Corporate   148.1312  230.0144 0.6440083
# 8  Africa   Corporate   430.9784  765.4631 0.5630297
# 15 Africa Home Office   425.2618  761.2168 0.5586606
# 4    EMEA    Consumer   531.9286 1164.0039 0.4569818
# 17 Canada Home Office   124.1292  278.4790 0.4457399
# 11   EMEA   Corporate   260.3986 1163.2273 0.2238588
# 18   EMEA Home Office   122.2138  718.7085 0.1700464


#Top two Market segments are EU-Consumer and APAC-Consumer