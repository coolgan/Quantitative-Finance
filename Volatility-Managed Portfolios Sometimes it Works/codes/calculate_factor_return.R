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

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

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

# variance
var_df <- stk_daily[,.(var = sd(ret)),by=c("ts_code","month")]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

stk_month[,risk_price:=pct_chg/var]
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

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]



# ivol, iskew (run CAPM beta to initialize)--------------------------------------------------------------------

# ivol
var_df <- stk_daily[,.(var=sd(lm.fit(y = as.matrix(.SD$ret),x = as.matrix(cbind(1,.SD$MKT,.SD$SMB,.SD$HML)))$residuals)),by=.(ts_code,month)]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# iskew
var_df <- stk_daily[,.(var=skewness(lm.fit(y = as.matrix(.SD$ret),x = as.matrix(cbind(1,.SD$MKT,.SD$SMB,.SD$HML)))$residuals)),by=.(ts_code,month)]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# calculate factor return ------------------------------------------------

month_list <- sort(unique(stk_month$month))
q_size <- 5
ret_dt <- as.data.table(data.frame("decile"=seq(1,q_size,1)))

for (i in 2:length(month_list)){
  rebalance_group <- stk_month[(month == month_list[i-1]),]
  rebalance_group$decile <-ntile(rebalance_group$var,q_size)
  #rebalance_group$decile <-ntile(rebalance_group$ROA,q_size)
  #rebalance_group$decile <-ntile(rebalance_group[,eval(factor)],q_size)
  
  current_group <- stk_month[(month == month_list[i]),]
  names(current_group)[names(current_group) == "total_mv"] <- "total_mv_end"
  current_group <- current_group[rebalance_group[,.(ts_code,decile,total_mv)],on=.(ts_code)]
  current_group <- current_group[(!is.na(current_group$decile))&(!is.na(current_group$pct_chg))]
  current_group$vw_ret <- current_group$total_mv * current_group$pct_chg
  
  group_ret <- current_group[,(mean(pct_chg)),by=decile][order(decile)]
  #group_ret <- current_group[,(mean(risk_price)),by=decile][order(decile)]
  names(group_ret)[names(group_ret) == "V1"] <- month_list[i]
  ret_dt <- ret_dt[group_ret,on='decile']
}


# create return dt --------------------------------------------------------
# low minus high
long_m_short <- (ret_dt[decile==1] - ret_dt[decile==q_size])[,decile:=10086]
# high minus low
long_m_short <- (ret_dt[decile==q_size] - ret_dt[decile==1])[,decile:=10086]

ret_dt <- rbind(ret_dt,long_m_short)
ret_dt <- melt(ret_dt,id="decile")
colnames(ret_dt) <- c("decile","date","ret")
ret_dt$date <- ymd(ret_dt$date,truncated = 1)
ret_dt <- ret_dt %>% na.omit()
ret_dt <- ret_dt[,cum_ret:=cumprod(1+ret),by=c("decile")][,c("date","decile","ret","cum_ret")] %>% setorder(decile,date)

# result analysis
result.analysis <- ret_dt[,.(annual_mean_ret = 12*mean(ret),
                             annual_sd_ret = sqrt(240/12)*sd(ret),
                          t_ret = t.test(ret,mu=0)$statistic,
                          max.drawdown = maxdrawdown(ret)$maxdrawdown,
                          max.drawdown_from = .SD[ maxdrawdown(.SD[,ret])$from,date],
                          max.drawdown_to = .SD[ maxdrawdown(.SD[,ret])$to,date],
                          win_ratio = length(.SD[ret>0,ret])/nrow(.SD)),by=c("decile")] %>% setorder(decile)
result.analysis$sharpe <- (result.analysis$annual_mean_ret)/(result.analysis$annual_sd_ret)

# visualization
ggplot(data = ret_dt[decile!=10086][,decile:=as.character(decile)],
       aes(x = date,y = cum_ret,group=decile,color=decile))+
  geom_line()

ggplot(data = result.analysis[decile!=10086][,decile:=as.character(decile)],
       aes(x = decile,y = annual_mean_ret))+
  geom_bar(stat = 'identity',fill="blue")

ao <- ret_dt[maxdrawdown(ret_dt[(decile==1),ret])$from]

fwrite(result.analysis,"./backtest_result2/analysis/ROA_result.csv")
fwrite(ret_dt,"./backtest_result2/ret/ROA_ret.csv")
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
