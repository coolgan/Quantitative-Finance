setwd("D://codes//Rfile//investment theory and practice")
library(data.table)
library(purrr)
library(tidyverse)
stk_daily <- fread("D://pyfile//投资理论实践//stk_daily2.csv")
stk_daily[,Stkcd:=as.numeric(substr(ts_code,0,6))]
stk_daily[,':='(year = substr(as.character(trade_date),1,4),month = as.numeric(substr(as.character(trade_date),5,6)))]
stk_daily[,Q:= ifelse(month%in%c(1,2,3),paste0(year,"Q1"),
                      ifelse(month%in%c(4,5,6),paste0(year,"Q2"),
                             ifelse(month%in%c(7,8,9),paste0(year,"Q3"),
                                    ifelse(month%in%c(10,11,12),paste0(year,"Q4"),NA))))]
stk_daily <- stk_daily[,-c("year","month")]
stk_daily[,month:= as.numeric(substr(as.character(trade_date),1,6) )]
#temp <- stk_daily[Stkcd==1]

stk_month <- fread("D://pyfile//投资理论实践//stk_monthly2.csv")
stk_month[,Stkcd:=as.numeric(substr(ts_code,0,6))]
stk_month[,':='(year = substr(as.character(trade_date),1,4),month = as.numeric(substr(as.character(trade_date),5,6)))]
stk_month[,Q:= ifelse(month%in%c(1,2,3),paste0(year,"Q4"),
                      ifelse(month%in%c(4,5,6),paste0(year,"Q1"),
                             ifelse(month%in%c(7,8,9),paste0(year,"Q2"),
                                    ifelse(month%in%c(10,11,12),paste0(year,"Q3"),NA))))]
stk_month <- stk_month[,-c("year","month")]
stk_month[,month:= as.numeric(substr(as.character(trade_date),1,6) )]


profit <- fread("profit.csv")
profit <- profit[Typrep == "A"]
profit <- rename(profit,ROA=F050204C,ROE= F050504C)
profit[,':='(year = as.numeric( substr(Accper,1,4) ),month = as.numeric(substr(Accper,6,7)))]
profit[,Q:=ifelse(month%in%c(1,2,3),paste0(year,"Q1"),
                  ifelse(month%in%c(4,5,6),paste0(year,"Q2"),
                         ifelse(month%in%c(7,8,9),paste0(year,"Q3"),
                                ifelse(month%in%c(10,11,12),paste0(year,"Q4"),NA))))]
profit[,':='(d.ROA=(ROA-lag(ROA))/lag(ROA),d.ROE = (ROE-lag(ROE))/lag(ROE)),by=c("Stkcd")]
#temp2 <- profit[Stkcd == 1]

profit_sub <- profit[,.(Stkcd,Q,ROA,ROE,d.ROA,d.ROE)]
stk_daily_total <- profit_sub[stk_daily,on=.(Stkcd,Q)]
stk_month_total <- profit_sub[stk_month,on=.(Stkcd,Q)]
fwrite(stk_daily_total,"stk_daily_merged.csv")
fwrite(stk_month_total,"stk_monthly_merged2.csv")
