#This file downloads US and Global ex-US equity factors data from Kenneth French Data Library
#and save data as CSV files

library(readr)
library(zoo)
library(Rblpapi)
library(xts)
library(mondate)

rm(list=ls())
endDate <- 201702

#Download zipped file for US F-F factors except Momentum
temp <- tempfile()
download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip",temp)
data_us <- read_csv(unz(temp, "F-F_Research_Data_5_Factors_2x3.CSV"), skip = 3, col_types = "cdddddd")
unlink(temp)
#rename first column as dates
colnames(data_us)[colnames(data_us)=="X1"] <- "dates"
#extract only monthly data ending on endDate
data_us <- head(data_us, match(TRUE, grepl(endDate, data_us$dates)))
#convert dates column type to be Date
data_us$dates <- as.Date(as.yearmon(data_us$dates, "%Y%m"), frac = 1)

#Download zipped file for US F-F momentum factor
temp <- tempfile()
download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_CSV.zip",temp)
data_mom <- read_csv(unz(temp, "F-F_Momentum_Factor.CSV"), skip = 13, col_types = "cd")
unlink(temp)

#rename first column as dates
colnames(data_mom)[colnames(data_mom)=="X1"] <- "dates"
#extract only monthly data ending on endDate
data_mom <- head(data_mom, match(TRUE, grepl(endDate, data_mom$dates)))
#convert dates column type to be Date
data_mom$dates <- as.Date(as.yearmon(data_mom$dates, "%Y%m"), frac = 1)

# merge all F-F factors
data_us <- merge(data_us, data_mom,  by.x = "dates", by.y = "dates")

# Convert periodic return to price index time series
data_us <- cbind(c(as.Date(mondate(data_us$dates[1]) - 1), data_us$dates),
                 as.data.frame(rbind(1, cumprod(1+data_us[,2:dim(data_us)[2]]/100))))
colnames(data_us)[1] <- "dates"
data_us <- data_us[,c(1:6, 8 ,7)]
colnames(data_us)[2:dim(data_us)[2]] <- c("US_Mkt","US_SMB","US_HML","US_RMW","US_CMA","US_MOM","US_RF")

write_csv(data_us, path='D:/Personal/Investments/US_F-F_Data.csv')

#Download zipped file for Global Ex-US F-F factors except Momentum
temp <- tempfile()
download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_ex_US_5_Factors_CSV.zip",temp)
data_exUS <- read_csv(unz(temp, "Global_ex_US_5_Factors.csv"), skip = 6, col_types = "cdddddd")
unlink(temp)
#rename first column as dates
colnames(data_exUS)[colnames(data_exUS)=="X1"] <- "dates"
#extract only monthly data ending on endDate
data_exUS <- head(data_exUS, match(TRUE, grepl(endDate, data_exUS$dates)))
#convert dates column type to be Date
data_exUS$dates <- as.Date(as.yearmon(data_exUS$dates, "%Y%m"), frac = 1)

#Download zipped file for Global Ex-US F-F momentum factor
temp <- tempfile()
download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_ex_US_Mom_Factor_CSV.zip",temp)
data_mom <- read_csv(unz(temp, "Global_ex_US_MOM_Factor.csv"), skip = 6, col_types = "cd")
unlink(temp)
#rename first column as dates
colnames(data_mom)[colnames(data_mom)=="X1"] <- "dates"
#extract only monthly data ending on endDate
data_mom <- head(data_mom, match(TRUE, grepl(endDate, data_mom$dates)))
#convert dates column type to be Date
data_mom$dates <- as.Date(as.yearmon(data_mom$dates, "%Y%m"), frac = 1)

# merge all F-F factors
data_exUS <- merge(data_exUS, data_mom,  by.x = "dates", by.y = "dates")

# Convert periodic return to price index time series
data_exUS <- cbind(c(as.Date(mondate(data_exUS$dates[1]) - 1), data_exUS$dates),
                 as.data.frame(rbind(1, cumprod(1+data_exUS[,2:dim(data_exUS)[2]]/100))))
colnames(data_exUS)[1] <- "dates"
data_exUS <- data_exUS[,c(1:6, 8 ,7)]
colnames(data_exUS)[2:dim(data_exUS)[2]] <- c("ExUS_Mkt","ExUS_SMB","ExUS_HML","ExUS_RMW","ExUS_CMA","ExUS_MOM","ExUS_RF")
write_csv(data_exUS, path='D:/Personal/Investments/Global_EX_US_F-F_Data.csv')


#Download US Fixed Income factors data from Bloomberg
tickers <- c("LT51TRUU","LUTLTRUU","LD07TRUU","LUMSTRUU","LD12TRUU")
names <- c("dates","IT_GOVT","LT_GOVT","LT_CORP","US_MBS","RF")
blpConnect()
ts <- bdh(paste(tickers,"Index"), c("PX_LAST"), start.date=as.Date("1993-01-31"),
          end.date=data_us$dates[length(data_us$dates)], options=c("periodicitySelection"="MONTHLY"))
data_bonds = as.data.frame(cbind(ts$`LT51TRUU Index`$date,ts$`LT51TRUU Index`$PX_LAST,ts$`LUTLTRUU Index`$PX_LAST,ts$`LD07TRUU Index`$PX_LAST,ts$`LUMSTRUU Index`$PX_LAST,ts$`LD12TRUU Index`$PX_LAST))
colnames(data_bonds) <- names
for (i in 1:4){
  p <- data_bonds[,i+1]
  c <- data_bonds[,dim(data_bonds)[2]]
  rp <- p[2:length(p)]/p[1:length(p)-1]-1
  rc <- c[2:length(c)]/c[1:length(c)-1]-1
  r_excess <- rp - rc
  p_excess <- rbind(1, as.matrix(cumprod(1+r_excess)))
  data_bonds[,i+1] <- p_excess
}
data_bonds$dates <- as.Date(data_bonds$dates)
write_csv(data_bonds, path='D:/Personal/Investments/US_Bonds_Data.csv')
