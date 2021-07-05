library(data.table)
library(ggplot2)
library(fts)
library(Rmisc)
library(lubridate)
library(gtools)
library(dplyr)
library(zoo)

setwd("D://codes//Rfile//investment theory and practice//")

files <- list.files(".//factor_daily//return//")

factor.data <- NULL
for (file in files){
  path <- paste0(".//factor_daily//return//",file)
  factors <- fread(path)
  factor.data <- rbind(factor.data,factors)
}

factor.data[,month:= as.numeric(substr(gsub("-","",as.character(trade_date)),1,6))]


# 多因子组合回测 （样本内）-----------------------------------------------------------------
# 判断vol.managed是否产生正显著的alpha
# 127/255的组合会产生正alpha

#factor.list <- unique(factor.data$name)
total.result <- NULL
factor.list <- c("turnover","pb","dv_ttm","tol_mv","tol_skew","ivol","dROA","dROE","ROE","ROA")
for (n.comb in 1:10){
  factor.group <- combinations(length(factor.list),n.comb,factor.list)
  result <- NULL
  
  for (i in 1:nrow(factor.group)){
    factor_to_test <- as.vector(factor.group[i,])
      
    long_short_dt <- factor.data[(decile == 1)&(month>200301)]
    
    test_dt <- dcast(long_short_dt[name%in%factor_to_test,.(trade_date,name,ret)],trade_date~name) %>% na.omit()
    
    test_dt$mv <- rowMeans(test_dt[,factor_to_test,with=F])
    
    test_dt <- test_dt[,c("trade_date","mv")]
    colnames(test_dt) <- c("trade_date","ret")
    
    test_dt[,vol := frollapply(ret,22,var)]
    test_dt[,month := as.numeric(substr(gsub('-','',as.character(trade_date)),1,6))]
    
    vol_dt <- test_dt[,.(weights = 1/vol[1]),by=month]
    test_dt <- vol_dt[test_dt,on=c("month")]
    test_dt$wret <- test_dt$weights * test_dt$ret
    test_dt <- na.omit(test_dt)
    c <- sd(test_dt$ret)/sd(test_dt$wret)
    test_dt$wret <- test_dt$wret * c
    test_dt$cum.ret <- cumprod(1+test_dt$ret)
    test_dt$wcum.ret <- cumprod(1+test_dt$wret)
    
    vol.managed.sharpe <- sqrt(240)*mean(test_dt$wret)/sd(test_dt$wret)
    
    raw.sharpe <- sqrt(240)*mean(test_dt$ret)/sd(test_dt$ret)
    
    # monthly return
    # ao <- test_dt[,.(vol.cum.Mret = cumprod(1+wret),raw.cum.Mret = cumprod(1+ret)),by=month]
    # ao <- ao[,.(vol.Mret = tail(vol.cum.Mret-1,1),raw.Mret = tail(raw.cum.Mret-1,1)),by=month]
    # model <- summary(lm(ao$vol.Mret~ao$raw.Mret))
    
    model <- summary(lm(test_dt$wret~test_dt$ret))
    t <- model$coefficients[1,3]
    p <- model$coefficients[1,4]
    
    #rows <- c(factor_to_test,vol.managed.sharpe,row.sharpe,t,p)
    rows <- data.table("var1" = factor_to_test[1],
                       "var2" = factor_to_test[2],
                       "var3" = factor_to_test[3],
                       "var4" = factor_to_test[4],
                       "var5" = factor_to_test[5],
                       "var6" = factor_to_test[6],
                       "var7" = factor_to_test[7],
                       "var8" = factor_to_test[8],
                       "var9" = factor_to_test[9],
                       "var10" = factor_to_test[10],
                       "vol.sharpe" = vol.managed.sharpe,
                       "raw.sharpe" = raw.sharpe,
                       "t" = t,
                       "p" = p)
    result <- rbind(result,rows)
  }
  total.result <- rbind(total.result,result)
}

ao <- total.result[(p<0.1)&(t>0),]
ao1 <- total.result[(p<0.05)&(t>0),]
ao2 <- total.result[(p<0.01)&(t>0),]
ao3 <- total.result[(p<0.001)&(t>0),]
ao4 <- total.result[(vol.sharpe>raw.sharpe),]

# out of sample -----------------------------------------------------------
total.result <- NULL
factor.list <- c("turnover","pb","dv_ttm","tol_mv","tol_skew","ivol","dROA","dROE","ROE","ROA")
for (n.comb in 1:10){
  factor.group <- combinations(length(factor.list),n.comb,factor.list)
  result <- NULL
  
  for (i in 1:nrow(factor.group)){
    factor_to_test <- as.vector(factor.group[i,])
    
    long_short_dt <- factor.data[(decile == 1)]
    
    test_dt <- dcast(long_short_dt[name%in%factor_to_test,.(trade_date,name,ret)],trade_date~name) %>% na.omit()
    
    test_dt$mv <- rowMeans(test_dt[,factor_to_test,with=F])
    
    test_dt <- test_dt[,c("trade_date","mv")]
    colnames(test_dt) <- c("trade_date","ret")
    test_dt[,month := as.numeric(substr(gsub('-','',as.character(trade_date)),1,6))]
    
    test_dt[,vol := frollapply(ret,22,var)]
    vol_dt <- test_dt[,.(weights = 1/vol[1]),by=month]
    test_dt <- vol_dt[test_dt,on=c("month")]
    test_dt$wret <- test_dt$weights * test_dt$ret
    test_dt <- na.omit(test_dt)
    
    test_dt[,cc:=frollapply(ret,960,sd)/frollapply(wret,960,sd)]
    cc_dt <- test_dt[,.(c = cc[1]),by=month]
    test_dt <- cc_dt[test_dt,on=c("month")]
    test_dt$wret <- test_dt$wret * test_dt$c
    test_dt <- na.omit(test_dt)
    
    test_dt$cum.ret <- cumprod(1+test_dt$ret)
    test_dt$wcum.ret <- cumprod(1+test_dt$wret)
    
    vol.managed.return <- 240 * mean(test_dt$wret)
    
    raw.return <- 240 * mean(test_dt$ret)
    
    vol.managed.sigma <- sqrt(240) * sd(test_dt$wret)
    
    raw.sigma <- sqrt(240) * sd(test_dt$ret)
    
    vol.managed.sharpe <- sqrt(240)*mean(test_dt$wret)/sd(test_dt$wret)
    
    raw.sharpe <- sqrt(240)*mean(test_dt$ret)/sd(test_dt$ret)
    
    #monthly return
    # ao <- test_dt[,.(vol.cum.Mret = cumprod(1+wret),raw.cum.Mret = cumprod(1+ret)),by=month]
    # ao <- ao[,.(vol.Mret = tail(vol.cum.Mret-1,1),raw.Mret = tail(raw.cum.Mret-1,1)),by=month]
    # model <- summary(lm(ao$vol.Mret~ao$raw.Mret))
    
    model <- summary(lm(test_dt$wret~test_dt$ret))
    t <- model$coefficients[1,3]
    p <- model$coefficients[1,4]
    
    #rows <- c(factor_to_test,vol.managed.sharpe,row.sharpe,t,p)
    rows <- data.table("var1" = factor_to_test[1],
                       "var2" = factor_to_test[2],
                       "var3" = factor_to_test[3],
                       "var4" = factor_to_test[4],
                       "var5" = factor_to_test[5],
                       "var6" = factor_to_test[6],
                       "var7" = factor_to_test[7],
                       "var8" = factor_to_test[8],
                       "var9" = factor_to_test[9],
                       "var10" = factor_to_test[10],
                       "vol.return" = vol.managed.return,
                       "raw.return" = raw.return,
                       "vol.sigma" = vol.managed.sigma,
                       "raw.sigma" = raw.sigma,
                       "vol.sharpe" = vol.managed.sharpe,
                       "raw.sharpe" = raw.sharpe,
                       "t" = t,
                       "p" = p)
    result <- rbind(result,rows)
  }
  total.result <- rbind(total.result,result)
}

ao <- total.result[(p<0.1)&(t>0),]
ao1 <- total.result[(p<0.05)&(t>0),]
ao2 <- total.result[(p<0.01)&(t>0),]
ao3 <- total.result[(p<0.001)&(t>0),]
ao4 <- total.result[(vol.sharpe>raw.sharpe),]
#excess_sharpe <- 
mean(total.result$vol.sharpe - total.result$raw.sharpe)
#excess_return <- 
mean(total.result$vol.return - total.result$raw.return)

#describe <- 
fwrite(total.result,"c://users//coolgan//desktop//total_result.csv")




t.test(total.result$vol.sigma - total.result$raw.sigma)

ao <- total.result$vol.return - total.result$raw.return

length(ao[(ao)>0])

# in-sample visualization -----------------------------------------------------------

factor_to_test <- c("turnover")

long_short_dt <- factor.data[(decile == 1)&(month>200501)]

test_dt <- dcast(long_short_dt[name%in%factor_to_test,.(trade_date,name,ret)],trade_date~name) %>% na.omit()

test_dt$mv <- rowMeans(test_dt[,factor_to_test,with=F])

test_dt <- test_dt[,c("trade_date","mv")]
colnames(test_dt) <- c("trade_date","ret")

test_dt[,vol := frollapply(ret,22,var)]
test_dt[,month := as.numeric(substr(gsub('-','',as.character(trade_date)),1,6))]

vol_dt <- test_dt[,.(weights = 1/vol[1]),by=month]
test_dt <- vol_dt[test_dt,on=c("month")]
test_dt$wret <- test_dt$weights * test_dt$ret
test_dt <- na.omit(test_dt)
c <- sd(test_dt$ret)/sd(test_dt$wret)
test_dt$wret <- test_dt$wret * c
test_dt$cum.ret <- cumprod(1+test_dt$ret)
test_dt$wcum.ret <- cumprod(1+test_dt$wret)

long_short_dt <- test_dt
long_short_dt$trade_date <- ymd(long_short_dt$trade_date)

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
temp$cum.drawdown <- (temp$cum.ret - expanding.max(temp$cum.ret))/expanding.max(temp$cum.ret)
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


View(temp)



# out-of-sample visualization ---------------------------------------------

ao4[vol.sharpe==max(vol.sharpe)]

factor_to_test <- c("tol_mv","ROE","tol_skew","turnover")

long_short_dt <- factor.data[(decile == 1)]

test_dt <- dcast(long_short_dt[name%in%factor_to_test,.(trade_date,name,ret)],trade_date~name) %>% na.omit()

test_dt$mv <- rowMeans(test_dt[,factor_to_test,with=F])

test_dt <- test_dt[,c("trade_date","mv")]
colnames(test_dt) <- c("trade_date","ret")
test_dt[,month := as.numeric(substr(gsub('-','',as.character(trade_date)),1,6))]

test_dt[,vol := frollapply(ret,22,var)]
vol_dt <- test_dt[,.(weights = 1/vol[1]),by=month]
test_dt <- vol_dt[test_dt,on=c("month")]
test_dt$wret <- test_dt$weights * test_dt$ret
test_dt <- na.omit(test_dt)

test_dt[,cc:=frollapply(ret,240,sd)/frollapply(wret,240,sd)]
cc_dt <- test_dt[,.(c = cc[1]),by=month]
test_dt <- cc_dt[test_dt,on=c("month")]
test_dt$wret <- test_dt$wret * test_dt$c
test_dt <- na.omit(test_dt)


# c <- sd(test_dt$ret)/sd(test_dt$wret)
# test_dt$wret <- test_dt$wret * c
test_dt$cum.ret <- cumprod(1+test_dt$ret)
test_dt$wcum.ret <- cumprod(1+test_dt$wret)

long_short_dt <- test_dt
long_short_dt$trade_date <- ymd(long_short_dt$trade_date)

index <- fread("index_daily.csv")
index <- index[,':='(trade_date=ymd(index$trade_date),
                     pct_chg = pct_chg/100)][,c("trade_date","pct_chg")]
long_short_dt <- index[long_short_dt,on=c("trade_date")]
long_short_dt$bench.cumret <- cumprod(long_short_dt$pct_chg+1)

#240*mean(long_short_dt$wret)/(sqrt(240)*sd(long_short_dt$wret))

p_list <- list()
# net value curve
p_list[[1]] <- ggplot()+
  geom_line(data = long_short_dt,aes(x = trade_date,y = wcum.ret,color="vol managed"))+
  geom_line(data = long_short_dt,aes(x = trade_date,y = cum.ret,color="buy and hold"))+
  geom_line(data = long_short_dt,aes(x = trade_date,y = bench.cumret,color="benchmark"))+
  ggtitle("Cumulative Performance")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"), 
        legend.key = element_rect(colour = "transparent", fill = "transparent"))+
  scale_colour_manual("",values = c("vol managed" = "red",
                                    "buy and hold"="blue",
                                    "benchmark"="black"))+
  xlab("")+ylab("Cumulative return")#+
  scale_y_log10()


# one-year rolling average return
p_list[[2]] <- ggplot()+
  geom_line(data = long_short_dt[,roll_wret:=frollmean(wret,240)] %>% na.omit(),aes(x = trade_date,y = roll_wret,color = "vol managed"))+
  geom_line(data = long_short_dt[,roll_ret:=frollmean(ret,240)] %>% na.omit(),aes(x = trade_date,y = roll_ret,color = "buy and hold"))+
  geom_line(data = long_short_dt[,roll_ret:=frollmean(pct_chg,240)] %>% na.omit(),aes(x = trade_date,y = roll_ret,color = "benchmark"))+

  ggtitle("One-Year Rolling Average Returns")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
        #legend.title = element_blank(),
        #legend.background = element_rect(fill = "transparent"), # 线条宽度
        #legend.key = element_rect(colour = "transparent", fill = "transparent"))+
  scale_colour_manual("",values = c("vol managed" = "red",
                                    "buy and hold"="blue",
                                    "benchmark"="black"))+
  xlab("")+ylab("Average Return")

# maxdrawdown curve
temp <- fts(index = long_short_dt$trade_date,data = long_short_dt[,.(wcum.ret,cum.ret,bench.cumret)])
temp$wcum.drawdown <- (temp$wcum.ret - expanding.max(temp$wcum.ret))/expanding.max(temp$wcum.ret)
temp$cum.drawdown <- (temp$cum.ret - expanding.max(temp$cum.ret))/expanding.max(temp$cum.ret)
temp$bench.drawdown <- (temp$bench.cumret - expanding.max(temp$bench.cumret))/expanding.max(temp$bench.cumret)
temp2 <- as.data.table(temp)


p_list[[3]] <- ggplot()+
  geom_line(data = temp2,aes(x = asofdate,y = wcum.drawdown,color="vol managed"))+
  geom_line(data = temp2,aes(x = asofdate,y = cum.drawdown,color = "buy and hold"))+
  geom_line(data = temp2,aes(x = asofdate,y = bench.drawdown,color = "benchmark"))+
  ggtitle("Drawdowns")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
        #legend.title = element_blank(),
        #legend.background = element_rect(fill = "transparent"), 
        #legend.key = element_rect(colour = "transparent", fill = "transparent"))+
  scale_colour_manual("",values = c("vol managed" = "red",
                                    "buy and hold"="blue",
                                    "benchmark" = "black"))+
  xlab("")+ylab("Drawdowns")

multiplot(plotlist = p_list[1:3], layout = matrix(c(1,1,2,3), nrow = 2, byrow = T)) 



# sharpe histogram
ggplot(data=melt(total.result[,c("vol.sharpe","raw.sharpe")]),aes(x=value))+
  geom_histogram(aes(color = variable,fill = variable),alpha=0.4)+
  scale_color_manual(values = c("#e9ecef", "#e9ecef"))+
  scale_fill_manual(values = c("red", "blue"))+
  xlab("sharpe ratio")+ylab("frequency")+
  theme(legend.position = "bottom",
        legend.title = element_blank())
ggplot()+
  geom_histogram(data = total.result[,d.sharpe:=vol.sharpe-raw.sharpe],
                 aes(x=d.sharpe),fill="red",
                 color="#e9ecef",alpha=0.8)+
  xlab("Difference of sharpe ratio")+ylab("frequency")

# return histogram
ggplot(data=melt(total.result[,c("vol.return","raw.return")]),aes(x=value))+
  geom_histogram(aes(color = variable,fill = variable),alpha=0.4)+
  scale_color_manual(values = c("#e9ecef", "#e9ecef"))+
  scale_fill_manual(values = c("red", "blue"))+
  xlab("return")+ylab("frequency")+
  theme(legend.position = "bottom",
        legend.title = element_blank())
ggplot()+
  geom_histogram(data = total.result[,d.return:=vol.return-raw.return],
                 aes(x=d.return),fill="red",
                 color="#e9ecef",alpha=0.8)+
  xlab("Difference of return")+ylab("frequency")

# sigma histogram
ggplot(data=melt(total.result[,c("vol.sigma","raw.sigma")]),aes(x=value))+
  geom_histogram(aes(color = variable,fill = variable),alpha=0.4)+
  scale_color_manual(values = c("#e9ecef", "#e9ecef"))+
  scale_fill_manual(values = c("red", "blue"))+
  xlab("sigma")+ylab("frequency")+
  theme(legend.position = "bottom",
        legend.title = element_blank())
ggplot()+
  geom_histogram(data = total.result[,d.sigma:=vol.sigma-raw.sigma],
                 aes(x=d.sigma),fill="red",
                 color="#e9ecef",alpha=0.8)+
  xlab("Difference of sigma")+ylab("frequency")



# c time series
ggplot()+
  geom_line(data = unique(test_dt[,month:=ymd(month,truncated = 1)][,.(month,c)]),
            aes(x = month,y = c))


fwrite(total.result,"./simulation_result/t_180.csv")



# further discussion ------------------------------------------------------

simu_result <- data.table(win_length = c(30,60,90,150,180,240,270,300,360,390,420,450,480,720,960,1200,2400),
                          win_ratio = c(480,595,708,902,1017,1017,1019,1012,839,367,420,167,283,0,0,0,0),
                          excess_sharpe = c(-0.003535527,0.003213709,0.005967851,
                                            0.02625643,0.1284329,0.1194,0.1229948,0.09944962,
                                            0.01896007,-0.01205235,-0.008979446,-0.02769992,-0.01759528,
                                            -0.1536956,-0.28415,-0.2488994,-0.1833913),
                          excess_return = c(0.0198818,0.02240382,0.02683424,0.02906138,
                                            0.06561102,0.0685,0.06833286,0.06559085,0.0305349,
                                            0.01940536,0.01530657,0.00762139,0.01106782,-0.04177113,
                                            -0.08740466,-0.0741429,-0.0417174
))

ggplot()+
  geom_line(data = simu_result[win_length<=1200],aes(x = win_length,y = win_ratio/1023),colour="blue",size=0.6)+
  geom_point(data = simu_result[win_length<=1200],aes(x = win_length,y = win_ratio/1023),colour="blue",size=2.3)+
  xlab("训练窗口长度（天）")+ylab("胜率")+scale_y_continuous(labels = scales::percent)+
  theme_set(theme_bw())
  #theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggplot()+
  geom_line(data = simu_result[win_length<=1200],aes(x = win_length,y = excess_sharpe),colour="blue",size=0.6)+
  geom_point(data = simu_result[win_length<=1200],aes(x = win_length,y = excess_sharpe),colour="blue",size=2.3)+
  xlab("训练窗口长度（天）")+ylab("平均超额夏普")+scale_y_continuous(labels = scales::percent)+
  theme_set(theme_bw())

ggplot()+
  geom_line(data = simu_result[win_length<=1200],aes(x = win_length,y = excess_return),colour="blue",size=0.6)+
  geom_point(data = simu_result[win_length<=1200],aes(x = win_length,y = excess_return),colour="blue",size=2.3)+
  xlab("训练窗口长度（天）")+ylab("平均超额年化收益")+scale_y_continuous(labels = scales::percent)+
  theme_set(theme_bw())

ggplot()+
  geom_line(data = simu_result,aes(x = win_length,y = win_ratio))


# try betaplus 100 index --------------------------------------------------
factor.data <- fread("C://users//coolgan//desktop//betaplus-1000-indexdaily-1.csv")

factor.data$trade_date <- as.Date(factor.data$trade_date,format="%Y/%m/%d")

factor.ret <- as.data.table(lapply(factor.data[,2:9],diff))/na.omit(lag(factor.data[,2:9],1))

factor.ret$trade_date <- factor.data$trade_date[2:nrow(factor.data)]

factor.list <- colnames(factor.ret)[1:length(colnames(factor.ret))-1]

total.result <- NULL

for (n.comb in 1:8){

  factor.group <- combinations(length(factor.list),n.comb,factor.list)
  result <- NULL
  
  for (i in 1:nrow(factor.group)){
    factor_to_test <- as.vector(factor.group[i,])
    test_dt <- factor.ret[,c("trade_date",factor_to_test),with=F]
    
    test_dt$mv <- rowMeans(factor.ret[,factor_to_test,with=F])
    
    test_dt <- test_dt[,c("trade_date","mv")]
    colnames(test_dt) <- c("trade_date","ret")
    
    test_dt[,vol:=frollapply(ret,22,var)]
    
    test_dt[,month := as.numeric(substr(gsub('-','',as.character(trade_date)),1,6))]
    
    vol_dt <- test_dt[,.(weights = 1/vol[1]),by=month]
    test_dt <- vol_dt[test_dt,on=c("month")]
    test_dt$wret <- test_dt$weights * test_dt$ret
    test_dt <- na.omit(test_dt)
    c <- sd(test_dt$ret)/sd(test_dt$wret)
    test_dt$wret <- test_dt$wret * c
    test_dt$cum.ret <- cumprod(1+test_dt$ret)
    test_dt$wcum.ret <- cumprod(1+test_dt$wret)
    
    vol.managed.sharpe <- sqrt(240)*mean(test_dt$wret)/sd(test_dt$wret)
    
    raw.sharpe <- sqrt(240)*mean(test_dt$ret)/sd(test_dt$ret)
    model <- summary(lm(test_dt$wret~test_dt$ret))
    t <- model$coefficients[1,3]
    p <- model$coefficients[1,4]
    
    #rows <- c(factor_to_test,vol.managed.sharpe,row.sharpe,t,p)
    rows <- data.table("var1" = factor_to_test[1],
                       "var2" = factor_to_test[2],
                       "var3" = factor_to_test[3],
                       "var4" = factor_to_test[4],
                       "var5" = factor_to_test[5],
                       "var6" = factor_to_test[6],
                       "var7" = factor_to_test[7],
                       "var8" = factor_to_test[8],
                       "var9" = factor_to_test[9],
                       "var10" = factor_to_test[10],
                       "vol.sharpe" = vol.managed.sharpe,
                       "raw.sharpe" = raw.sharpe,
                       "t" = t,
                       "p" = p)
    result <- rbind(result,rows)
  }
  total.result <- rbind(total.result,result)
}

ao <- total.result[(p<0.1)&(vol.managed.sharpe>raw.sharpe)]

#

total <- NULL

ret_dt <- factor.data[(decile==10)&(name=="ROE")]

temp <- fts(index = ymd(ret_dt$trade_date),data = ret_dt[,.(cum.ret,ret)])

temp$cum.drawdown <- (temp$cum.ret - expanding.max(temp$cum.ret))/expanding.max(temp$cum.ret)

result.analysis <- ret_dt[,.(name="dv_ttm",
                             annual_mean_ret = 240*mean(ret),
                             annual_sd_ret = sqrt(240)*sd(ret),
                             t_ret = t.test(ret,mu=0)$statistic,
                             max.drawdown = max(-temp$cum.drawdown),
                             # max.drawdown = maxdrawdown(ret)$maxdrawdown,
                             # max.drawdown_from = .SD[ maxdrawdown(.SD[,ret])$from,date],
                             # max.drawdown_to = .SD[ maxdrawdown(.SD[,ret])$to,date],
                             win_ratio = length(.SD[ret>0,ret])/nrow(.SD)),by=c("decile")] %>% setorder(decile)
result.analysis$sharpe <- (result.analysis$annual_mean_ret)/(result.analysis$annual_sd_ret)
total <- rbind(total,result.analysis)

fwrite(total,"c://users//coolgan//desktop//long_description.csv")
