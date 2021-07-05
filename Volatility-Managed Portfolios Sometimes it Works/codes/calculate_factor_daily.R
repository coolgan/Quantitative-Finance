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
library(fts)
library(Rmisc)

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

# turnover rate
var_df <- stk_daily[,.(var = mean(turnover_rate)),by=c("ts_code","month")]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# mean pb
var_df <- stk_daily[,.(var = 1/mean(pb)),by=c("ts_code","month")]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# mean dv_ttm
var_df <- stk_daily[,.(var = mean(dv_ttm)),by=c("ts_code","month")]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# mean total mv
var_df <- stk_daily[,.(var = mean(total_mv)),by=c("ts_code","month")]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# total skewness
var_df <- stk_daily[,.(var = moments::skewness(ret)),by=c("ts_code","month")]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# volatility
var_df <- stk_daily[,.(var = sd(ret)),by=c("ts_code","month")]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]


# ivol  ---------------------------------------------------------------
ff3 <- fread("factor//ff3_daily.csv")
ff3 <- ff3[(MarkettypeID=="P9706")][,-c("MarkettypeID")][
  ,c("TradingDate","RiskPremium1","SMB1","HML1")] %>% na.omit()

colnames(ff3) <- c("date","MKT","SMB","HML")

stk_daily <- ff3[stk_daily,on="date"]

stk_daily <- stk_daily[(.N>15)&(!is.na(ret)),,by=.(ts_code,month)]

var_df <- stk_daily[,.(var=sd(lm.fit(y = as.matrix(.SD$ret),x = as.matrix(cbind(1,.SD$MKT,.SD$SMB,.SD$HML)))$residuals)),by=.(ts_code,month)]

stk_month <- var_df[stk_month,on = .(ts_code,month)][(!is.na(pct_chg))&(!is.na(var))]

# get monthly stock selection result ------------------------------------------------

month_list <- sort(unique(stk_month$month))
q_size <- 5
ret_dt <- as.data.table(data.frame("decile"=seq(1,q_size,1)))
stock.selection <- NULL

for (i in 2:length(month_list)){
  rebalance_group <- stk_month[(month == month_list[i-1]),]
  rebalance_group$decile <-ntile(rebalance_group$var,q_size)
  #rebalance_group$decile <-ntile(rebalance_group$ROE,q_size)
  #rebalance_group$decile <-ntile(rebalance_group[,eval(factor)],q_size)
  
  current_group <- stk_month[(month == month_list[i]),]
  names(current_group)[names(current_group) == "total_mv"] <- "total_mv_end"
  current_group <- current_group[rebalance_group[,.(ts_code,decile,total_mv)],on=.(ts_code)]
  stock.selection <- rbind(stock.selection,current_group[,.(ts_code,month,decile)])
}


# calculate daily factor return -------------------------------------------

ret_dt_daily <- NULL
for (i in 2:length(month_list)){
  current_group <- stk_daily[month == month_list[i],]
  stock.list <- stock.selection[month == month_list[i],]
  current_group <- current_group[stock.list,on=.(ts_code,month)]
  ret_month <- current_group[,.(ret = mean(ret)),by=.(trade_date,decile)] %>% setorder(trade_date,decile)
  ret_dt_daily <- rbind(ret_dt_daily,ret_month)
}

long_short.data <- dcast(ret_dt_daily[decile %in% c(1,q_size),.(decile,trade_date,ret)],trade_date~decile) %>% na.omit()
# kow minus high
long_short <- long_short.data$"1" - long_short.data$"5"
# high minus low
long_short <- long_short.data$"10" - long_short.data$"1"

long_short_dt <- data.table(trade_date = long_short.data$trade_date,
                            decile = 10086,
                            ret = long_short)

ret_dt_daily <- rbind(ret_dt_daily,long_short_dt) %>% setorder(trade_date,decile)
ret_dt_daily[,cum.ret:=cumprod(1+ret),by=decile]
ret_dt_daily$trade_date <- ymd(ret_dt_daily$trade_date)

result.analysis <- ret_dt_daily[,.(annual_mean_ret = 240*mean(ret),
                             annual_sd_ret = sqrt(240)*sd(ret),
                             t_ret = t.test(ret,mu=0)$statistic,
                             max.drawdown = maxdrawdown(ret)$maxdrawdown,
                             max.drawdown_from = .SD[ maxdrawdown(.SD[,ret])$from,trade_date],
                             max.drawdown_to = .SD[ maxdrawdown(.SD[,ret])$to,trade_date],
                             win_ratio = length(.SD[ret>0,ret])/nrow(.SD)),by=c("decile")] 
result.analysis$sharpe <- (result.analysis$annual_mean_ret)/(result.analysis$annual_sd_ret)


ggplot(data = ret_dt_daily[decile!=10086][,decile:=as.character(decile)],
       aes(x = trade_date,y = cum.ret,group=decile,color=decile))+
  geom_line()

ggplot(data = ret_dt_daily[decile == 10086][,decile := as.character(decile)],
       aes(x = trade_date,y = cum.ret))+
  geom_line()

factor_name <- "ROE"
ret_dt_daily$name <- factor_name
filename1 <- paste0("./factor_daily/analysis/",factor_name,"_result.csv")
filename2 <- paste0("./factor_daily/return/",factor_name,"_ret.csv")
fwrite(result.analysis,filename1)
fwrite(ret_dt_daily,filename2)

result.analysis[,mu_var:=annual_mean_ret/annual_sd_ret]
ggplot(data = result.analysis[decile!=10086][,decile:=as.character(decile)],
       aes(x = decile,y = mu_var))+
  geom_bar(stat = 'identity',fill="blue")

# volatility-timing -------------------------------------------------------
# # low minus high
# long_short <- ret_dt_daily[decile == 1,ret] - ret_dt_daily[decile == 10,ret]
# # high minus low
# long_short <- ret_dt_daily[decile == 10,ret] - ret_dt_daily[decile == 1,ret]
# 
# long_short_dt <- data.table(trade_date = ymd(ret_dt_daily[decile==1,trade_date]),
#                             decile = 10086,
#                             ret = long_short)

long_short.data <- dcast(ret_dt_daily[decile %in% c(1,10),.(decile,trade_date,ret)],trade_date~decile) %>% na.omit()
# kow minus high
long_short <- long_short.data$"1" - long_short.data$"10"
# high minus low
long_short <- long_short.data$"10" - long_short.data$"1"

long_short_dt <- data.table(trade_date = ymd(long_short.data$trade_date),
                            decile = 10086,
                            ret = long_short)

long_short_dt[,vol := frollapply(ret,22,var)]
long_short_dt[,month := as.numeric(substr(gsub('-','',as.character(trade_date)),1,6))]

vol_dt <- long_short_dt[,.(weights = 1/vol[1]),by=month]
long_short_dt <- vol_dt[long_short_dt,on=c("month")]
long_short_dt$wret <- long_short_dt$weights * long_short_dt$ret
long_short_dt <- na.omit(long_short_dt)
c <- sd(long_short_dt$ret)/sd(long_short_dt$wret)
long_short_dt$wret <- long_short_dt$wret * c
long_short_dt$cum.ret <- cumprod(1+long_short_dt$ret)
long_short_dt$wcum.ret <- cumprod(1+long_short_dt$wret)

sqrt(240)*mean(long_short_dt$wret)/sd(long_short_dt$wret)

sqrt(240)*mean(long_short_dt$ret)/sd(long_short_dt$ret)

summary( lm(long_short_dt$wret~long_short_dt$ret) )

# vol-managed visualization -----------------------------------------------

p_list <- list()
# net value curve
p_list[[1]] <- ggplot()+
  geom_line(data = long_short_dt,aes(x = trade_date,y = wcum.ret,color="vol managed"))+
  geom_line(data = long_short_dt,aes(x = trade_date,y = cum.ret,color="buy and hold"))+
  ggtitle("Cumulative Performance")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9,0.3),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"), 
        legend.key = element_rect(colour = "transparent", fill = "transparent"))+
  scale_colour_manual("",values = c("vol managed" = "red","buy and hold"="blue"))+
  xlab("")+ylab("Cumulative return")+
  scale_y_log10()


# one-year rolling average return
p_list[[2]] <- ggplot()+
  geom_line(data = long_short_dt[,roll_wret:=frollmean(wret,240)] %>% na.omit(),aes(x = trade_date,y = roll_wret,color = "vol managed"))+
  geom_line(data = long_short_dt[,roll_ret:=frollmean(ret,240)] %>% na.omit(),aes(x = trade_date,y = roll_ret,color = "buy and hold"))+
  ggtitle("One-Year Rolling Average Returns")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.4,0.15),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"), # 线条宽度
        legend.key = element_rect(colour = "transparent", fill = "transparent"))+
  scale_colour_manual("",values = c("vol managed" = "red","buy and hold"="blue"))+
  xlab("")+ylab("Average Return")

# maxdrawdown curve
temp <- fts(index = long_short_dt$trade_date,data = long_short_dt[,.(wcum.ret,cum.ret)])
temp$wcum.drawdown <- (temp$wcum.ret - expanding.max(temp$wcum.ret))/expanding.max(temp$wcum.ret)
temp$cum.drawdown <- (temp$cum.ret - expanding.max(temp$cum.ret))/temp$cum.ret
temp2 <- as.data.table(temp)

p_list[[3]] <- ggplot()+
  geom_line(data = temp2,aes(x = asofdate,y = wcum.drawdown,color="vol managed"))+
  geom_line(data = temp2,aes(x = asofdate,y = cum.drawdown,color = "buy and hold"))+
  ggtitle("Drawdowns")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8,0.15),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"), 
        legend.key = element_rect(colour = "transparent", fill = "transparent"))+
  scale_colour_manual("",values = c("vol managed" = "red","buy and hold"="blue"))+
  xlab("")+ylab("Drawdowns")

multiplot(plotlist = p_list[1:3], layout = matrix(c(1,1,2,3), nrow = 2, byrow = T)) 

sqrt(2.1e-5)
# draft -------------------------------------------------------------------
# data(EuStockMarkets)
# dax <- log(EuStockMarkets[,"DAX"])
# mdd <- maxdrawdown(dax)
# mdd
# 
# plot(dax)
# segments(time(dax)[mdd$from], dax[mdd$from],
#          time(dax)[mdd$to], dax[mdd$from], col="grey")
# segments(time(dax)[mdd$from], dax[mdd$to],
#          time(dax)[mdd$to], dax[mdd$to], col="grey")
# mid <- time(dax)[(mdd$from + mdd$to)/2]
# arrows(mid, dax[mdd$from], mid, dax[mdd$to], col="red", length = 0.16)
install.packages("gtools")
library(gtools)
?combinations
ao <- combinations(4,2,c("turnover","ivol","pb","dv"))
as.vector(ao[1,])
