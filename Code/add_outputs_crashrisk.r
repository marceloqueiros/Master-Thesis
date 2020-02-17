rm(list=ls())

library("readtext")
library("quanteda")
library("lubridate")
library("tidyverse")
library(dplyr)
library(tibble)

theme_set(theme_bw())

#READ---------
path_rds = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\dataset\\info_industry_market\\"
path_metrics = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\dataset\\generated_files\\"

stocks_sample = readRDS(paste(path_rds,'stocks_sample.rds', sep = "")) #RDS
stocks_daily = readRDS(paste(path_rds,'stocks_daily_data.rds', sep = "")) #RDS
FF_Factors_daily = read.csv(paste(path_rds,'F-F_Research_Data_Factors_daily.CSV', sep = ""), stringsAsFactors = FALSE, header=TRUE, sep=",")
FF_Factors_daily[,1] = as.numeric(FF_Factors_daily[,1])
#FF_5Factors2x3_daily = as.numeric(read.csv(paste(path_rds,'F-F_Research_Data_5_Factors_2x3_daily.CSV', sep = ""), header=FALSE,stringsAsFactors = FALSE, sep=","))

sic_codes = readRDS(paste(path_rds,'sic_codes.rds', sep = "")) #RDS
sic_codes_to_industry = read.csv(paste(path_rds,'Sic_to_industry.CSV', sep = ""), header = TRUE, stringsAsFactors = FALSE, sep=";")
industry_daily = read.csv(paste(path_rds,'49_Industry_Portfolios_Daily.CSV', sep = ""), header = FALSE, stringsAsFactors = FALSE, sep=";")[1:24456,]
industry_daily$V1[5:length(industry_daily$V1)] = as.numeric(industry_daily$V1[5:length(industry_daily$V1)])

all_metrics = readRDS(paste(path_metrics,'all_metrics.rds', sep = "")) #RDS
all_metrics$cik = as.numeric(all_metrics$cik)
#READ---------
# all_stocks = merge(stocks_sample, stocks_daily, by="PERMNO")
# all_stocks = merge(stocks_sample, sic_codes, by="PERMNO")

# all_stocks$industry <- 0

# for (i in 1:length(all_stocks$industry)){
#   for (ll in 1:length(sic_codes_to_industry$min)){
#     if (all_stocks$SICCD[i] >= sic_codes_to_industry$min[ll] & all_stocks$SICCD[i] <= sic_codes_to_industry$max[ll]){
#       all_stocks$industry[i] = sic_codes_to_industry$number[ll]
#       print(ll)
#       break
#     }
#   }
# }

#lm(as.numeric(industry_daily$V4[5:10])~as.numeric(industry_daily$V5[5:10]) + as.numeric(industry_daily$V6[5:10]))

#read companies to all_metrics csv
  
without_permno = data.frame("cik"=-1, "company_year"=-1)
without_days_stock = data.frame("cik"=-1, "company_year"=-1, "start"=-1, "end"=-1)
without_sic = data.frame("cik"=-1, "company_year"=-1, "sic"=-1)
without_industry = data.frame("cik"=-1, "company_year"=-1, "value" = 0)

all_metrics$NCSKEW = -999.999
all_metrics$DUVOL = -999.999
all_metrics$CRASH_COUNT = -999.999
CRASH_COUNT_up = 0
CRASH_COUNT_down = 0

for (company_year in 1:length(all_metrics$company)){ #every company and year
  print(company_year)
    
    #atention for company name - without spaces
    PERMNO_company = stocks_sample$PERMNO[which(stocks_sample$cik == all_metrics$cik[company_year])]
    if ( length(PERMNO_company) == 0) {
      without_permno = rbind(without_permno, c(all_metrics$cik[company_year],company_year))
      
    }else{
  
    company_start = all_metrics$company[company_year]
    company_end = all_metrics$company[company_year+1]
    #tem que se ver a ultima
    
    date_start = as.numeric(all_metrics$date[company_year])
    if(company_year == length(all_metrics$company)){
      date_end = 20181231  #last company
    }else if (company_start == company_end){
        date_end = as.numeric(all_metrics$date[company_year+1])
      }else{
        date_end = 20181231 #limited by stocks_daily
    }
    
    days = subset(stocks_daily, stocks_daily$CALDT > date_start & stocks_daily$CALDT < date_end & stocks_daily$PERMNO == PERMNO_company)$CALDT
    dif <- as.Date(as.character(days[1]), format="%Y%m%d")- as.Date(as.character(date_start), format="%Y%m%d")
    
    if ( length(days) == 0 || as.numeric(dif) > 10) {
      without_days_stock = rbind(without_days_stock, c(all_metrics$cik[company_year],company_year, date_start, date_end))
    }else{
    
    data_lm = data.frame("day" = days, "r_j" = 0, "r_i" = 0, "r_m" = 0, sic = 0, industry = 0)
    
    #r_m is from date_start to date_end
    #r_i is needed to see the industry of the day and after the index
    #after we can do the regression -> lm
    data_lm$r_j = subset(stocks_daily, stocks_daily$CALDT > date_start & stocks_daily$CALDT < date_end & stocks_daily$PERMNO == PERMNO_company)$RET
    
    r_m_subset = subset(FF_Factors_daily, FF_Factors_daily$day > date_start & FF_Factors_daily$day < date_end)
    data_lm$r_m = (r_m_subset$Mkt.RF + r_m_subset$RF)/100 #  ("Mkt-Rf" + "RF")/100.
    
    #company_sics = data.frame(r_j_subset$CALDT, sic = 0, industry = 0 , r_i = 0)
    #colnames(company_sics) <- c("day", "sic", "industry", "r_i")
    # sic codes the company
    subset_company_sics = subset(sic_codes, sic_codes$PERMNO == PERMNO_company & sic_codes$SICCD != 9999)
    
    #after subset created we search for the sic for this day 
    flag_break = 0
    for (i in 1:length(subset_company_sics$SICCD)){#sic
      data_lm[ data_lm[, "day"] >= subset_company_sics$NAMEDT[i] & 
                data_lm[, "day"] <= subset_company_sics$NAMEENDDT[i] , 5 ] = subset_company_sics$SICCD[i] ##index 5 is sic
      #if (subset_company_sics$SICCD[i] == 9999){
      #  without_sic = rbind(without_sic, c(all_metrics$cik[company_year],company_year, 9999))
      #  flag_break = 1
      #  break
      #}
    }
    
    if(flag_break == 0){
      #calculate industry number and correspondent index
      #flag_break = 0
      for (i in 1:length(data_lm$industry)){
        data_lm$industry[i] = sic_codes_to_industry$number[which(data_lm$sic[i] >= sic_codes_to_industry$min & data_lm$sic[i] <= sic_codes_to_industry$max)]
        data_lm$r_i[i] = as.numeric(industry_daily[which(industry_daily$V1 == data_lm[i, "day"]), which(industry_daily[3,] == data_lm$industry[i])])
        if (data_lm$r_i[i] == -99.99 || data_lm$r_i[i] == -999){
          without_industry = rbind(without_industry, c(all_metrics$cik[company_year],company_year, data_lm$r_i[i]))
          flag_break = 1
          break
        }
      }
      if(flag_break == 0){
      #index=1
      #for (day in company_sics$day){
      #  company_sics$day[1] = subset(sic_codes, sic_codes$PERMNO == PERMNO_company & day > sic_codes$NAMEDT & day < sic_codes$NAMEENDDT,)#sic_codes$day > date_start & FF_Factors_daily$day < date_end)
      #  index=index+1   
      #}
      
      model_yc = lm(r_j ~ 1 + lag(r_m) + lag(r_i) + r_m + r_i, data = data_lm)
      
      Rj = model_yc$residuals #R_j_t
      Rju = subset(Rj, Rj > mean(Rj))
      Rjd = subset(Rj, Rj < mean(Rj))
      
      n = length(Rj)
      nu = length(Rju)
      nd = length(Rjd)
      
      #Crash risk metrics
      all_metrics$NCSKEW[company_year] = -(n*(n-1)^(3/2)*sum(Rj^3))/((n-1)*(n-2)*(sum(Rj^2)^(3/2)))
      all_metrics$DUVOL[company_year] = log( ((nu - 1) * sum(Rjd^2)) / ((nd-1) * sum(Rju^2)) )
      
      #if ( sd(Rj) > mean(Rj) + 3.09){
      #  CRASH_COUNT_up = CRASH_COUNT_up + 1
      #}
      #if ( sd(Rj) < mean(Rj) - 3.09){
      #  CRASH_COUNT_down = CRASH_COUNT_down + 1
      #}
      CRASH_COUNT_up = sum(Rj > (mean(Rj) + 3.09*sd(Rj))) #replace count by sum
      CRASH_COUNT_down = sum(Rj < (mean(Rj) - 3.09*sd(Rj))) #replace count by sum
      all_metrics$CRASH_COUNT[company_year] = CRASH_COUNT_down - CRASH_COUNT_up
}}}}}

all_metrics %>%
  filter(NCSKEW < -100) %>%
  select(cik, company, year, NCSKEW)

all_metrics<-all_metrics[!(all_metrics$NCSKEW==-999.999),]

path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\dataset\\"
write_rds(all_metrics, paste(path, "generated_files\\all_metrics_out.rds", sep = ""))

write.csv2(all_metrics,file=paste0(path,"generated_files\\Companies1.csv"), row.names=FALSE)#,,col.names=TRUE



library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = FF_Factors_daily[1:10,]
png(paste(path, "FF_Factors_daily.png"), height = 1000, width = 1000, units = "px", res=200, pointsize = 12)
grid.table(df)
dev.off()


library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = industry_daily[1:15,1:10]
png(paste(path, "industry_daily.png"), height = 1500, width = 2000, units = "px", res=200, pointsize = 12)
grid.table(df)
dev.off()


library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = sic_codes[1:10,]
png(paste(path, "sic_codes_day.png"), height = 1000, width = 2000, units = "px", res=200, pointsize = 12)
grid.table(df)
dev.off()


library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = sic_codes_to_industry[1:10,]
png(paste(path, "sic_codes_to_industry.png"), height = 1000, width = 1000, units = "px", res=200, pointsize = 12)
grid.table(df)
dev.off()


library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = stocks_daily[1:10,]
png(paste(path, "stocks_daily.png"), height = 1000, width = 1000, units = "px", res=200, pointsize = 12)
grid.table(df)
dev.off()


library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = stocks_sample[1:10,]
png(paste(path, "stocks_sample.png"), height = 1000, width = 2000, units = "px", res=200, pointsize = 12)
grid.table(df)
dev.off()