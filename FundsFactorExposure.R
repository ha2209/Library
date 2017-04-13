library(readr)
library(Rblpapi)
library(leaps)
library(timeDate)
rm(list=ls())
setwd('D:\\Personal\\Investments')

ct1 <- 0
ct2 <- 0
ct3 <- 0

tickers <- c("VWO","IVV","ALJ","AMAG","INTC","VUG","VEU","VTV","VEMPX","VIIIX","VTPSX","VFINX","VGTSX","BAC","ALBKY","DSEEX","VXUS","VTI","WFSPX","WACSX","VBMPX","PTTRX")
type <- c("X","U","U","U","U","U","X","U","U","U","X","U","X","U","X","U","X","U","U","B","B","B")


US_data <- read_csv("US_F-F_Data.csv", col_types = "Dddddddd")
ExUS_data <- read_csv("Global_EX_US_F-F_Data.csv", col_types = "Dddddddd")
Bonds_data <- read_csv("US_Bonds_Data.csv", col_types = "Dddddd")


Out <- as.data.frame(matrix(0,length(tickers),2+dim(US_data)[2]-2+dim(ExUS_data)[2]-2+dim(Bonds_data)[2]-2))
colnames(Out) <- c("tickers", "(Intercept)", names(US_data)[2:(dim(US_data)[2]-1)],
                   names(ExUS_data)[2:(dim(ExUS_data)[2]-1)], names(Bonds_data)[2:(dim(Bonds_data)[2]-1)])
Out$tickers <- tickers
US_Index <- 7:16
ExUS_Index <- c(1:6,13:16)
Bonds_Index <- 1:12

blpConnect()
for (i in 1:length(tickers)){
  ts <- bdh(paste(tickers[i],"US Equity"), c("TOT_RETURN_INDEX_GROSS_DVDS"),
            start.date=as.Date("2009-12-31"), options=c("periodicitySelection"="DAILY"),
            include.non.trading.days = TRUE)
  NonNAindex <- which(!is.na(ts$TOT_RETURN_INDEX_GROSS_DVDS))
  firstNonNA <- min(NonNAindex)
  ts <- ts[firstNonNA:dim(ts)[1],]
  ts$TOT_RETURN_INDEX_GROSS_DVDS <- na.locf(ts$TOT_RETURN_INDEX_GROSS_DVDS)

  
  mergedts <- merge(US_data, ts,  by.x = "dates", by.y = "date")
  mergedts <- merge(mergedts, ExUS_data,  by.x = "dates", by.y = "dates")
  mergedts <- merge(mergedts, Bonds_data,  by.x = "dates", by.y = "dates")
  
  mergedts[,2:dim(mergedts)[2]] <- rbind(NA,apply(mergedts[,2:dim(mergedts)[2]],2,diff)/mergedts[1:dim(mergedts)[1]-1,2:dim(mergedts)[2]])
  mergedts = na.omit(mergedts)
  X <- as.data.frame(cbind(mergedts$US_Mkt,mergedts$US_SMB,mergedts$US_HML,mergedts$US_RMW,mergedts$US_CMA,mergedts$US_MOM,
             mergedts$ExUS_Mkt,mergedts$ExUS_SMB,mergedts$ExUS_HML,mergedts$ExUS_RMW,mergedts$ExUS_CMA,mergedts$ExUS_MOM,
             mergedts$IT_GOVT,mergedts$LT_GOVT,mergedts$LT_CORP,mergedts$US_MBS))
  colnames(X)<-c(colnames(US_data)[2:(dim(US_data)[2]-1)], colnames(ExUS_data)[2:(dim(ExUS_data)[2]-1)],
                 colnames(Bonds_data)[2:(dim(Bonds_data)[2]-1)])
  X <- as.matrix(log(1+X))
  
  y <- log(1 + mergedts$TOT_RETURN_INDEX_GROSS_DVDS - mergedts$RF)
  out_index <- switch(type[i], U = US_Index, X = ExUS_Index, B = Bonds_Index)
  
  results <- regsubsets(X,y,force.out=out_index)
  c <- coef(results, which(summary(results)$bic == min(summary(results)$bic)))
  Out[i,match(intersect(names(Out),names(c)), names(Out))] <- t(c)
}
write_csv(Out, path='D:/Personal/Investments/Funds_Factor_Exposure.csv')