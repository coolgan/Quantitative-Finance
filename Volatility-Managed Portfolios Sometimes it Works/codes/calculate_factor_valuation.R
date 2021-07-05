library(data.table)
library(moments)
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(sandwich)
library(tseries)
library(rollRegres)
library(roll)
library(zoo)
library(moments)

setwd("D://codes//Rfile//investment theory and practice//")

stk_daily <- fread("D://pyfile//投资理论实践//stk_daily2.csv")
#stk_daily <- fread("stk_daily_merged.csv")

stk_daily$date <- ymd(stk_daily$trade_date)

stk_daily <- stk_daily[,month:= as.numeric(substr(as.character(trade_date),0,6))]

stk_month <- fread("D://pyfile//投资理论实践//stk_month2_cleaned.csv")

stk_month$month <- stk_month[,as.numeric(substr(as.character(trade_date),0,6))]

stk_month <- fread("stk_monthly_merged2_cleaned.csv")

stk_month <- stk_month[(!is.na(ROE))&(!is.na(ROA))&(!is.na(d.ROE))&(!is.na(d.ROA))]



# calculate stock selection indicator -------------------------------------

# monthly turnover
var_df <- stk_daily[,.(var = mean(turnover_rate)),by=c("ts_code","month")]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]


# abnormal turnover rate
stk_daily[,':='(turnover.240 = frollmean(turnover_rate,240),
                turnover.21 = frollmean(turnover_rate,21)),by=c("ts_code")][,
                                                                            turnover.abn:=turnover.21-turnover.240,]
var_df <- stk_daily[,.(var = tail(.SD$turnover.abn,1)),by=c("ts_code","month")]

stk_month <- stk_month[var_df,on = .(ts_code,month)] %>% na.omit()

# mean pb
var_df <- stk_daily[,.(var = mean(pb)),by=c("ts_code","month")]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# mean dv_ttm
var_df <- stk_daily[,.(var = mean(dv_ttm)),by=c("ts_code","month")]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# mean total mv
var_df <- stk_daily[,.(var = mean(total_mv)),by=c("ts_code","month")]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# total skewness
var_df <- stk_daily[,.(var = skewness(ret)),by=c("ts_code","month")]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# MAX5d
stk_daily_temp <- stk_daily[,.SD[ret>quantile(ret,0.3)],by=c("month")]

var_df <- stk_daily_temp[,.(var = mean(tail(.SD$ret,5))),by=c("ts_code","month")]

stk_month <- stk_month[var_df,on = .(ts_code,month)] %>% na.omit()

# CAPM beta ---------------------------------------------------------------

ff3 <- fread("factor//ff3_daily.csv")
ff3 <- ff3[(MarkettypeID=="P9706")][,-c("MarkettypeID")][
  ,c("TradingDate","RiskPremium1","SMB1","HML1")] %>% na.omit()

colnames(ff3) <- c("date","MKT","SMB","HML")

stk_daily <- ff3[stk_daily,on="date"]

stk_daily <- stk_daily[(.N>15)&(!is.na(ret)),,by=.(ts_code,month)]

temp <- stk_daily[ts_code=='000001.SZ']

temp2 <- temp[month == 200001]

var_df <- stk_daily[,.(var=coef(lm.fit(y=as.matrix(.SD$ret),x=as.matrix(cbind(1,.SD$MKT))))[2]),by=.(ts_code,month)]

stk_month <- stk_month[var_df,on = .(ts_code,month)] %>% na.omit()


# ivol, iskew (run CAPM beta to initialize)--------------------------------------------------------------------

# ivol
var_df <- stk_daily[,.(var=sd(lm.fit(y = as.matrix(.SD$ret),x = as.matrix(cbind(1,.SD$MKT,.SD$SMB,.SD$HML)))$residuals)),by=.(ts_code,month)]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# iskew
var_df <- stk_daily[,.(var=skewness(lm.fit(y = as.matrix(.SD$ret),x = as.matrix(cbind(1,.SD$MKT,.SD$SMB,.SD$HML)))$residuals)),by=.(ts_code,month)]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# calculate factor return ------------------------------------------------

month_list <- sort(unique(stk_month$month))
q_size <- 10
value_dt <- as.data.table(data.frame("decile"=seq(1,q_size,1)))

for (i in 2:length(month_list)){
  rebalance_group <- stk_month[(month == month_list[i-1]),]
  #rebalance_group$decile <-ntile(rebalance_group$var,q_size)
  rebalance_group$decile <-ntile(rebalance_group$ROA,q_size)
  #rebalance_group$decile <-ntile(rebalance_group[,eval(factor)],q_size)
  
  current_group <- stk_month[(month == month_list[i]),]
  names(current_group)[names(current_group) == "total_mv"] <- "total_mv_end"
  current_group <- current_group[rebalance_group[,.(ts_code,decile,total_mv)],on=.(ts_code)]
  current_group <- current_group[(!is.na(current_group$decile))&(!is.na(current_group$pct_chg))]
  current_group$vw_ret <- current_group$total_mv * current_group$pct_chg
  
  group_value <- current_group[,(mean(pb)),by=decile][order(decile)]
  names(group_value)[names(group_value) == "V1"] <- month_list[i]
  value_dt <- value_dt[group_value,on='decile']
}


# create return dt --------------------------------------------------------
# low minus high
long_m_short <- (value_dt[decile==1] - value_dt[decile==q_size])[,decile:=10086]
# high minus low
long_m_short <- (value_dt[decile==q_size] - value_dt[decile==1])[,decile:=10086]

value_dt <- rbind(value_dt,long_m_short)
value_dt <- melt(value_dt,id="decile")
colnames(value_dt) <- c("decile","date","pb")
value_dt$date <- ymd(value_dt$date,truncated = 1)
value_dt <- value_dt %>% na.omit()
#value_dt <- value_dt[,cum_ret:=cumprod(1+ret),by=c("decile")][,c("date","decile","ret","cum_ret")] %>% setorder(decile,date)

# result analysis
result.analysis <- value_dt[,.(annual_mean_ret = 12*mean(pb),
                             annual_sd_ret = sqrt(240/12)*sd(pb),
                             t_ret = t.test(pb,mu=0)$statistic,
                             max.drawdown = maxdrawdown(pb)$maxdrawdown,
                             max.drawdown_from = .SD[ maxdrawdown(.SD[,pb])$from,date],
                             max.drawdown_to = .SD[ maxdrawdown(.SD[,pb])$to,date],
                             win_ratio = length(.SD[pb>0,pb])/nrow(.SD)),by=c("decile")] %>% setorder(decile)
result.analysis$sharpe <- (result.analysis$annual_mean_ret)/(result.analysis$annual_sd_ret)

# visualization
ggplot(data = value_dt[decile!=10086][,decile:=as.character(decile)],
       aes(x = date,y = pb,group=decile,color=decile))+
  geom_line()

ao <- ret_dt[maxdrawdown(ret_dt[(decile==1),ret])$from]

#fwrite(result.analysis,"./backtest_result2/valuation/ROA_result.csv")
fwrite(value_dt,"./backtest_result2/valuation/ROA_pb.csv")

# draft -------------------------------------------------------------------

f <- function(dt){
  mean_list <- rep(0,nrow(dt))
  for (i in 1:nrow(dt)){
    date_i <- dt[i,date]
    date_i_1y <- dt[i,date_1y]
    if (date_i_1y > dt[1,date]){
      window <- dt[(date>date_i_1y)&(date<date_i)]
      if (nrow(window) > 120){
        mean.turnover <- mean(window$turnover_rate)
      }
      else{
        mean.turnover <- NA
      }
    }
    else{
      mean.turnover <- NA
    }
    mean_list[i] <- mean.turnover
  }
  return(mean_list)
}

system.time(
  ao <- stk_daily[,mean.turnover:=f(.SD),by=c("ts_code")]
)

# roll_regres
# temp <- stk_daily[ts_code=="000001.SZ",c("ret","turnover_rate")] %>% na.omit()%>% as.matrix()
# 
# temp <- stk_daily[,ivol:=sd(roll_regres(ret~turnover_rate,.SD %>% na.omit(),250,do_compute = c("r.residuals"))$residuals),by=c("ts_code")] 
# 
# ao <- rollapplyr(temp,250,FUN=function(dd) sd( lm.fit(y=as.matrix(dd[,"ret"]),x=as.matrix(dd[,"turnover_rate"]))$residuals ),by.column=FALSE)
# 
# woc <- stk_daily[,(ivol= rollapplyr(.SD %>% na.omit(),250,FUN= function(dd) sd( lm.fit(y=as.matrix(dd[c("ret")]),x=as.matrix(dd[c("turnover_rate")]))$residuals ))),by=c("ts_code") ]

# yearly turnover
# stk_daily <- stk_daily[,mean.turnover:=frollmean(turnover_rate,240),by=c("ts_code")]
# stk_daily <- stk_daily[,flag:=frollapply(date,240,function(x) fifelse( tail(x,1)-x[1]<420 ,1,0) ),by=c("ts_code")]
# stk_daily <- stk_daily[,mean.turnover:=ifelse(flag==1,mean.turnover,NA)]
