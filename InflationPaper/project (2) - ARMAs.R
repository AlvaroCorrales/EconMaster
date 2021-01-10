
#set working directory to source file locations
setwd("C:/Users/USUARIO/Google Drive/Barcelona GSE/Financial Econometrics/Project")
library(moments)
library(tseries)

#upload data

money <- read.csv("money2.csv", sep=",", header=T, stringsAsFactors = F)
unemp <- read.csv("unemp.csv", sep=",", header=T, stringsAsFactors = F)
indpro <- read.csv("indpro.csv", sep=",", header=T, stringsAsFactors = F)
inflation <- read.csv("inflation.csv", sep=",", header=T, stringsAsFactors = F)
irateb <- read.csv("irateb.csv", sep=",", header=T, stringsAsFactors = F)
iratefed <- read.csv("iratefed.csv", sep=",", header=T, stringsAsFactors = F)
wages <- read.csv("wages.csv", sep=",", header=T, stringsAsFactors = F)
cons_conf <- read.csv("cons_conf.csv", sep=",", header=T, stringsAsFactors = F)
importsprice <- read.csv("importsprice.csv", sep=",", header=T, stringsAsFactors = F)


#look for the data that starts the latest
#select first date of each variable and the max of all these
starting_date <- max(c(indpro$DATE[1], inflation$DATE[1], irateb$DATE[1], 
                    iratefed$DATE[1], money$DATE[1], unemp$DATE[1], wages$DATE[1],
                    cons_conf$DATE[1]))

#check which one is
date_min <- c(indpro$DATE[1], inflation$DATE[1], irateb$DATE[1], iratefed$DATE[1], 
  money$DATE[1], unemp$DATE[1], wages$DATE[1],cons_conf$DATE[1])

rbind(c("indpro", "inflation", "irateb", "iratefed", "money","unemp", "wages", "cons_conf"),date_min )


ending_date <- min(c(indpro$DATE[nrow(indpro)], inflation$DATE[nrow(inflation)], 
                     irateb$DATE[nrow(irateb)], iratefed$DATE[nrow(iratefed)], 
                       money$DATE[nrow(money)], unemp$DATE[nrow(unemp)], wages$DATE[nrow(wages)],
                     cons_conf$DATE[nrow(cons_conf)]))
                    # importsprice$DATE[nrow(importsprice)]))


#Cut data

inflation1<- inflation$CPIAUCSL[as.Date(inflation$DATE)>= starting_date & 
                         as.Date(inflation$DATE)<= ending_date]

irateb1<- irateb$MPRIME[as.Date(irateb$DATE)>= starting_date & 
                         as.Date(irateb$DATE)<= ending_date]

iratefed1<- iratefed$FEDFUNDS[as.Date(iratefed$DATE)>= starting_date & 
                    as.Date(iratefed$DATE)<= ending_date]

money1<- money$M2NS[as.Date(money$DATE)>= starting_date & 
                     as.Date(money$DATE)<= ending_date]

unemp1<- unemp$UNRATE[as.Date(unemp$DATE)>= starting_date & 
                      as.Date(unemp$DATE)<= ending_date]

indpro1<- indpro$INDPRO[as.Date(indpro$DATE)>= starting_date & 
              as.Date(indpro$DATE)<= ending_date]

wages1 <- wages$AHETPI[as.Date(wages$DATE)>= starting_date & 
                            as.Date(wages$DATE)<= ending_date]

cons_conf1 <- cons_conf$CSCICP03USM665S[as.Date(cons_conf$DATE)>= starting_date & 
                           as.Date(cons_conf$DATE)<= ending_date]

importsprice1 <- importsprice$IR_CHG[as.Date(importsprice$DATE)>= starting_date & 
                           as.Date(importsprice$DATE)<= ending_date]

#create new dats
dates<- seq(as.Date(starting_date), as.Date(ending_date), by = "month")

#paste dates and data
D <- data.frame(dates, indpro1, inflation1, irateb1, iratefed1, 
                money1, unemp1, wages1, cons_conf1)

#stats
means <- colMeans(D[2:ncol(D)])#colum means ignoring dates 

sds <- apply(as.matrix(D[,2:ncol(D)]),2, sd)#column standard deviation

skews <- apply(as.matrix(D[,2:ncol(D)]),2, skewness)
kurts <- apply(as.matrix(D[,2:ncol(D)]),2, kurtosis)

stats <- round(rbind(means, sds, skews, kurts),3)

#Change column names of stats matrix
colnames(stats) <- c("Indpro", "Inflation", "Irate Bsness", "Irate Fed"
                     , "Money2", "Unemployment", "Wages", "Cons Confidence")

#change row names of stats matrix
rownames(stats) <- c("Mean", "SD", "Skewness", "Kurtosis")
stats

#export to csv file
write.csv(stats, file='stats.csv')

# ADF test (null of unit root (non-stationarity))
adfs = 1:(ncol(D)-1)
for (i in 2:ncol(D)){   #loop through all columns except dates
    
    test_adf <- adf.test(D[,i]) #generate a variable with the test every iteration
    
    adfs[i-1] = test_adf$p.value #generate new variable by extracting p-values,
    #adding one element to adfs with every iteration
    #note that inflation1's p-value is .05 -> in the limit of the unit root -> take 2nd diff
    }
adfs = unlist(adfs, use.names=T, recursive=T)  #transform adfs from list to vector

names(adfs) = colnames(D[,2:ncol(D)]) 

#Jarque-bera test (null of normality)
jb = 1:(ncol(D)-1)
for (i in 2:ncol(D)){   #loop through all columns except dates
  
  test_jb <- jarque.bera.test(D[,i]) #generate a variable with the test every iteration
  
  jb[i-1] = test_jb$p.value #generate new variable by extracting p-values,
  #adding one element to adfs with every iteration
  
}
jb = unlist(jb, use.names=T, recursive=T)  #transform adfs from list to vector

names(jb) = colnames(D[,2:ncol(D)]) 

# Ljung-Box test (null of independence)
lb = 1:(ncol(D)-1)
for (i in 2:ncol(D)){   #loop through all columns except dates
  
  test_lb <- Box.test(D[,i], type='Ljung-Box') #generate a variable with the test every iteration
  
  lb[i-1] = test_lb$p.value #generate new variable by extracting p-values,
  #adding one element to adfs with every iteration
  
}
lb = unlist(lb, use.names=T, recursive=T)  #transform adfs from list to vector

names(lb) = colnames(D[,2:ncol(D)]) 

stats_test = round(rbind(adfs, jb, lb),3)
colnames(stats_test) = colnames(stats)
rownames(stats_test) = c("ADF", "JB", "LB")
write.csv(stats_test, file='stats_test.csv')

#Create new dataset, with log differences and one observation left
D_diff <- D
for (i in 2:ncol(D_diff)){
    D_diff[,i] <- c(0, 100*diff(log(D_diff[,i])))
}
D_diff <- D_diff[2:nrow(D_diff),]

#stats of differenced data

means_diff <- colMeans(D_diff[2:ncol(D_diff)])#colum means ignoring dates 

sds_diff <- apply(as.matrix(D_diff[,2:ncol(D_diff)]),2, sd)#column standard deviation

skews_diff <- apply(as.matrix(D_diff[,2:ncol(D_diff)]),2, skewness)
kurts_diff <- apply(as.matrix(D_diff[,2:ncol(D_diff)]),2, kurtosis)

stats_diff <- round(rbind(means_diff, sds_diff, skews_diff, kurts_diff),3)


#Change column names of stats matrix
colnames(stats_diff) <- c("Indpro", "Inflation", "Irate Bsness", "Irate Fed"
                     , "Money2", "Unemployment", "Wages", "Cons Confidence")

#change row names of stats matrix
rownames(stats_diff) <- c("Mean", "SD", "Skewness", "Kurtosis")
stats_diff
write.csv(stats_diff, 'stats_diff.csv')

# ADF test (null of unit root (non-stationarity))
adfs_diff = 1:(ncol(D_diff)-1)
for (i in 2:ncol(D_diff)){   #loop through all columns except dates
    
    test_adf <- adf.test(D_diff[,i]) #generate a variable with the test every iteration
    
    adfs_diff[i-1] = test_adf$p.value #generate new variable by extracting p-values,
    #adding one element to adfs with every iteration
    #note that inflation1's p-value is .05 -> in the limit of the unit root -> take 2nd diff
}
adfs_diff = unlist(adfs_diff, use.names=T, recursive=T)  #transform adfs from list to vector

names(adfs_diff) = colnames(D_diff[,2:ncol(D_diff)]) 
adfs_diff

#Jarque-bera test (null of normality)
jb_diff = 1:(ncol(D_diff)-1)
for (i in 2:ncol(D_diff)){   #loop through all columns except dates
    
    test_jb <- jarque.bera.test(D_diff[,i]) #generate a variable with the test every iteration
    
    jb_diff[i-1] = test_jb$p.value #generate new variable by extracting p-values,
    #adding one element to adfs with every iteration
    
}
jb_diff = unlist(jb_diff, use.names=T, recursive=T)  #transform adfs from list to vector

names(jb_diff) = colnames(D_diff[,2:ncol(D_diff)]) 
jb_diff

# Ljung-Box test (null of independence)
lb_diff = 1:(ncol(D_diff)-1)
for (i in 2:ncol(D_diff)){   #loop through all columns except dates
    
    test_lb <- Box.test(D_diff[,i], type='Ljung-Box') #generate a variable with the test every iteration
    
    lb_diff[i-1] = test_lb$p.value #generate new variable by extracting p-values,
    #adding one element to adfs with every iteration
    
}
lb_diff = unlist(lb_diff, use.names=T, recursive=T)  #transform adfs from list to vector

names(lb_diff) = colnames(D_diff[,2:ncol(D_diff)]) 
lb_diff

stats_test_diff = round(rbind(adfs_diff, jb_diff, lb_diff),3)
colnames(stats_test_diff) = colnames(stats_diff)
rownames(stats_test_diff) = c("ADF", "JB", "LB")
write.csv(stats_test_diff, file="stats_test_diff.csv")

stats_test_diff

#Dynamic properties
acf(D_diff$inflation1, main = "Inflation", col='black')
pacf(D_diff$inflation1, main = "Inflation")


for (i in 2:ncol(D_diff)){
    jpeg(paste('plots/acf','_', colnames(D_diff[i]),'.jpg', sep=""))    #open image object
    acf(D_diff[,i], main = variable.names(stats_diff)[i-1], col="darkorange", lwd=3) #run acf plot
    dev.off()   #close image
}

for (i in 2:ncol(D_diff)){
        jpeg(paste('plots/pacf','_', colnames(D_diff[i]),'.jpg', sep=""))
        pacf(D_diff[,i], main = variable.names(stats_diff)[i-1], col="skyblue4", lwd=3)
        dev.off()
}



#Kernel DATA (inflation)

kernel_data <- density((D_diff$inflation1-mean(D_diff$inflation1))/sqrt(var(D_diff$inflation1)))
plot( kernel_data , main='')
polygon( kernel_data, col="tomato" , border='darkred')
lines( seq(-10,20,0.1) , dnorm( seq(-10,20,0.1) ) , col='darkblue' ,lwd=2 )
abline(v=mean(D_diff$inflation1),lwd=2)



#qqplot DATA (inflation)
qqnorm(D_diff$inflation1,col='tomato',main='')
qqline(D_diff$inflation1,lwd=2,lty=3)

# inflation plot

png('plots/inflation_timeseries.jpg')
plot(D_diff$dates,D_diff$inflation1, col="darkorange", type="l", xlab="Date", ylab="Inflation", main="")
grid(col="black")
abline(h=mean(D_diff$inflation1), col="darkblue", lwd=2)
grid(col="black")
dev.off()


# Kernel with ARIMA

kernel_arima <- density(reg_arima$residuals/sqrt(reg_arima$sigma2))
plot( kernel_arima , main='arima' )
polygon( kernel_arima , col="tomato" , border='darkred')
abline(h=0,lwd=2)
lines( seq(-10,20,0.1) , dnorm( seq(-10,20,0.1) ) , col='darkblue' ,lwd=2 )
dev.off()

nrow(D_diff)*0.8
D_diff$dates[nrow(D_diff)*0.8]

# Split data - ALL SAMPLE
# 80% of 647 = 517.6 ~ 518
inf_in <- D_diff$inflation1[1:517]
inf_out <- D_diff$inflation1[518:length(D_diff$inflation1)]
dates_in <- D_diff$dates[1:517]
dates_out <- D_diff$dates[518:length(D_diff$inflation1)]

# Fit ARMA models
arma11 = arima(inf_in, order=c(1,0,1))
ar1 = arima(inf_in, order=c(1,0,0))
ma1 = arima(inf_in, order=c(0,0,1))

# Diagnostics and residuals analysis
ar1_res <- as.numeric(ar1$residuals)
ma1_res <- as.numeric(ma1$residuals)
arma11_res <- as.numeric(arma11$residuals)

moments_arma11 <- round(c(mean(arma11_res), var(arma11_res), skewness(arma11_res), kurtosis(arma11_res) ),3)
moments_ar1 <- round(c(mean(ar1_res), var(ar1_res), skewness(ar1_res), kurtosis(ar1_res) ),3)
moments_ma1 <- round(c(mean(ma1_res), var(ma1_res), skewness(ma1_res), kurtosis(ma1_res) ),3)

moments <- rbind(moments_arma11, moments_ar1, moments_ma1)
colnames(moments) <- c('MEAN', 'VARIANCE', 'SKEWNESS', 'KURTOSIS')
rownames(moments) <- c('Resiudals ARMA(1,1)', 'Residuals AR(1)', 'Residuals MA(1)')

moments

Box.test(arma11_res, lag=24, type='Ljung-Box') 
Box.test(ar1_res, lag=24, type='Ljung-Box')
Box.test(ma1_res, lag=24, type='Ljung-Box')

## Simple models do not seem to clean the residuals enough, some dependence still remains

arma21 = arima(inf_in, order=c(2,0,1))
arma21_res <- as.numeric(arma21$residuals)
Box.test(arma21_res, lag=24, type='Ljung-Box') 
box_arma21 <- Box.test(arma21_res, lag=24, type='Ljung-Box')
box_arma21_pvalue <- box_arma21$p.value
acf(arma21_res, lag.max = 24, plot=T)

ar6 = arima(inf_in, order=c(6,0,0))
ar6_res <- as.numeric(ar6$residuals)
Box.test(ar6_res, lag=24, type='Ljung-Box') 
box_ar6 <- Box.test(ar6_res, lag=24, type='Ljung-Box') 
box_ar6_pvalue <- box_ar6$p.value
acf(ar6_res, lag.max = 24, plot=T)

arma61 = arima(inf_in, order=c(6,0,1))
arma61_res <- as.numeric(arma61$residuals)
Box.test(arma61_res, lag=24, type='Ljung-Box') 
box_arma61 <- Box.test(arma61_res, lag=24, type='Ljung-Box')
box_arma61_pvalue <- box_arma61$p.value
acf(arma61_res, lag.max = 24, plot=T)

arma33 = arima(inf_in, order=c(3,0,3))
arma33_res <- as.numeric(arma33$residuals)
Box.test(arma33_res, lag=24, type='Ljung-Box') 
box_arma33 <- Box.test(arma33_res, lag=24, type='Ljung-Box')
box_arma33_pvalue <- box_arma33$p.value
acf(arma33_res, lag.max = 24, plot=T)

arma66 = arima(inf_in, order=c(6,0,6))
arma66_res <- as.numeric(arma66$residuals)
Box.test(arma66_res, lag=24, type='Ljung-Box') 
box_arma66 <- Box.test(arma66_res, lag=24, type='Ljung-Box')
box_arma66_pvalue <- box_arma66$p.value
acf(arma66_res, lag.max = 24, plot=T)

ar9 = arima(inf_in, order=c(9,0,0))
ar9_res <- as.numeric(ar9$residuals)
Box.test(ar9_res, lag=24, type='Ljung-Box') 
box_ar9 <- Box.test(ar9_res, lag=24, type='Ljung-Box') 
box_ar9_pvalue <- box_ar9$p.value
acf(ar9_res, lag.max = 24, plot=T)

arma91 = arima(inf_in, order=c(9,0,1))
arma91_res <- as.numeric(arma91$residuals)
Box.test(arma91_res, lag=24, type='Ljung-Box') 
box_arma91 <- Box.test(arma91_res, lag=24, type='Ljung-Box')
box_arma91_pvalue <- box_arma91$p.value
acf(arma91_res, lag.max = 24, plot=T)

arma93 = arima(inf_in, order=c(9,0,3))
arma93_res <- as.numeric(arma93$residuals)
Box.test(arma93_res, lag=24, type='Ljung-Box') 
box_arma93 <- Box.test(arma93_res, lag=24, type='Ljung-Box')
box_arma93_pvalue <- box_arma93$p.value
acf(arma93_res, lag.max = 24, plot=T)

arma96 = arima(inf_in, order=c(9,0,6))
arma96_res <- as.numeric(arma96$residuals)
Box.test(arma96_res, lag=24, type='Ljung-Box') #Do not reject H0: The residuals are iid
box_arma96 <- Box.test(arma96_res, lag=24, type='Ljung-Box')
box_arma96_pvalue <- box_arma96$p.value
acf(arma96_res, lag.max = 24, plot=T)

ar12 = arima(inf_in, order=c(12,0,0))
ar12_res <- as.numeric(ar12$residuals)
Box.test(ar12_res, lag=24, type='Ljung-Box') #Do not reject H0: The residuals are iid
box_ar12 <- Box.test(ar12_res, lag=24, type='Ljung-Box') 
box_ar12_pvalue <- box_ar12$p.value
acf(ar12_res, lag.max = 24, plot=T)

arma12_1 = arima(inf_in, order=c(12,0,1))
arma12_1_res <- as.numeric(arma12_1$residuals)
Box.test(arma12_1_res, lag=24, type='Ljung-Box') #Do not reject H0: The residuals are iid
box_arma12_1 <- Box.test(arma12_1_res, lag=24, type='Ljung-Box')
box_arma12_1_pvalue <- box_arma12_1$p.value
acf(arma12_1_res, lag.max = 24, plot=T)

arma18_1 = arima(inf_in, order=c(18,0,1)) #Way less parsimonious than previous models
arma18_1_res <- as.numeric(arma18_1$residuals)
Box.test(arma18_1_res, lag=24, type='Ljung-Box') #Do not reject H0: The residuals are iid
box_arma18_1 <- Box.test(arma18_1_res, lag=24, type='Ljung-Box')
box_arma18_1_pvalue <- box_arma18_1$p.value
acf(arma18_1_res, lag.max = 24, plot=T)

lb_pvalues <- round(cbind(box_ar6_pvalue, box_arma61_pvalue, box_arma66_pvalue, box_ar9_pvalue, box_arma91_pvalue, box_arma96_pvalue, box_ar12_pvalue, box_arma12_1_pvalue),3)
colnames(lb_pvalues) <- c('AR(6)', 'ARMA(6,1)', 'ARMA(6,6)', 'AR(9)', 'ARMA(9,1)', 'ARMA(9,6)', 'AR(12)', 'ARMA(12,1)')
rownames(lb_pvalues) <- 'LB: p-value'
lb_pvalues

## Intuition for 12 lags: Persistent price stickiness - Prices at month t will be highly dependent on previous months levels
## Choose the AR(12), ARMA(12,1), ARMA(9,6) to compute AIC and BIC

N_in <- length(inf_in) 
  
ar12_aic    <- (-2*ar12$loglik+2*14)/N_in
arma12_1_aic    <- (-2*arma12_1$loglik+2*15)/N_in
arma96_aic <- (-2*arma96$loglik+2*17)/N_in

ar12_bic    <- (-2*ar12$loglik+log(N_in)*14)/N_in
arma12_1_bic    <- (-2*arma12_1$loglik+log(N_in)*15)/N_in
arma96_bic <- (-2*arma96$loglik+log(N_in)*17)/N_in

gof <- round( rbind( c(ar12$loglik,arma12_1$loglik,arma96$loglik), 
                     c(ar12_aic,arma12_1_aic,arma96_aic) , 
                     c(ar12_bic,arma12_1_bic,arma96_bic) ) ,  3 )

colnames(gof) <- c('ar12', 'arma12_1', 'arma96')
rownames(gof) <- c('nLL', 'AIC', 'BIC')

gof #AR(12) presents the best in-sample performance

# Plot fitted values
ar12_fit <- inf_in-ar12$residuals
arma12_1_fit <- inf_in-arma12_1$residuals
arma96_fit <- inf_in-arma96$residuals

par( mar=c(2,2,3,2) , mfrow=c(2,2) )

myplot <- function( dates , y , col='darkblue' , t='l' , lwd=2 , ylim=NULL , main=NULL ){
  if( is.null(main) ){ par( mar=c(2,2,0.1,0.1) ) }
  plot( dates , y , t=t , col=col , lwd=lwd , axes=F , xlab='' , ylab='' , xaxs="i" , ylim=ylim , main=main )
  xticks <- axis.Date(1, x=dates, at=seq(dates[1], dates[length(dates)], "year") , lwd=0, lwd.tick=1, tck=0.02)
  yticks <- axis(2 , lwd=0, lwd.tick=1, tck=0.02)
  axis.Date(3, x=dates, at=seq(dates[1], dates[length(dates)], "year"), lwd=0, lwd.tick=1, tck=0.02, lab=F)
  axis(4, lwd=0, lwd.tick=1, tck=0.02, lab=F)
  abline( h=yticks , lty=3 )
  abline( v=xticks , lty=3 )
  box()
}

myplot1 <- function( dates , y , col='darkblue' , t='l' , lwd=2 , ylim=NULL , main=NULL ){
  if( is.null(main) ){ par( mar=c(2,2,0.1,0.1) ) }
  plot( dates , y , t=t , col=col , lwd=lwd , axes=F , xlab='' , ylab='' , xaxs="i" , ylim=ylim , main=main )
  xticks <- axis.Date(1, x=dates, at=seq(dates[1], dates[length(dates)], "year") , lwd=0, lwd.tick=1, tck=0.02)
  yticks = FALSE
  axis.Date(3, x=dates, at=seq(dates[1], dates[length(dates)], "year"), lwd=0, lwd.tick=1, tck=0.02, lab=F)
  axis(4, lwd=0, lwd.tick=1, tck=0.02, lab=F)
  abline( h=yticks , lty=3 )
  abline( v=xticks , lty=3 )
  box()
}

par( mar=c(2,2,3,2) , mfrow=c(2,2) )

myplot(dates_in, inf_in , t='p' , col='darkorange2')
par(new=TRUE)
lines(dates_in, arma96_fit , t='l' , lwd=2, col='blue1')
legend('topright', c('Inflation', 'ARMA(9,6)'), col=c('darkorange2', 'blue1'), lwd=3)
myplot(dates_in, inf_in , t='p' , col='darkorange2')
par(new=TRUE)
lines(dates_in, arma12_1_fit , t='l' , lwd=2 , col='green1' )
legend('topright', c('Inflation', 'ARMA(12,1)'), col=c('darkorange2', 'green1'), lwd=3)
myplot(dates_in, inf_in , t='p' , col='darkorange2')
par(new=TRUE)
lines(dates_in, ar12_fit , t='l' , lwd=2 , col='red2' )
legend('topright', c('Inflation', 'AR(12)'), col=c('darkorange2', 'red2'), lwd=3)
dev.off()

par( mar=c(2,2,3,2) , mfrow=c(2,2) )

qqnorm(arma96_res,col='blue3', main="")
qqline(arma96_res,lwd=2,lty=3)
grid(col='gray')
legend('topleft', 'ARMA(9,6)', col='blue1', lwd=3)
qqnorm(arma12_1_res,col='green1',main="")
qqline(arma12_1_res,lwd=2,lty=3)
grid(col='gray')
legend('topleft', 'ARMA(12,1)', col='green1', lwd=3)
qqnorm(ar12_res,col='red2',main="")
qqline(ar12_res,lwd=2,lty=3)
grid(col='gray')
legend('topleft', 'AR(12)', col='red2', lwd=3)


# Forecast
N <- length(inf_in)
H <- length(inf_out)

## Dynamic forecast
ar12_predd    <- predict( ar12 , n.ahead=H )$pred
arma12_1_predd    <- predict( arma12_1 , n.ahead=H )$pred
arma96_predd <- predict( arma96 , n.ahead=H )$pred

## 1-step ahead forecast
N <- length(inf_in)
H <- length(inf_out)

ar12_pred1 <- rep(0,H)
arma12_1_pred1 <- rep(0,H)
arma96_pred1 <- rep(0,H)

for( m in 0:(H-1) ){
  
  y <- D_diff$inflation1[1:(N+m)] #Update sample adding the m-th observation
  
  ar12    = arima(y,order=c(12,0,0))
  arma12_1    = arima(y,order=c(12,0,1))
  arma96    = arima(y,order=c(9,0,6))
  
  ar12_pred1[1+m]    <- predict( ar12 , n.ahead=1 )$pred
  arma12_1_pred1[1+m]    <- predict( arma12_1 , n.ahead=1 )$pred
  arma96_pred1[1+m]    <- predict( arma96 , n.ahead=1 )$pred
}


myplot(dates_out, inf_out, col="black")
abline(h=mean(inf_out), col="chocolate1", lwd=2)
par(new=TRUE)
lines( dates_out, arma96_pred1 , t='l' , lwd=2, col='blue2')
par(new=TRUE)
lines( dates_out, arma12_1_pred1, t='l' , lwd=2 , col='green1')
par(new=TRUE)
lines( dates_out, ar12_pred1, t='l', lwd=2, col='red2')
legend( 'bottomright' , c('Inflation','ARMA(9,6)', "ARMA(12,1)", "AR(12)", "Out-of-sample unconditional mean")  
        , col=c('black','blue2','green1', "red2", "chocolate1") , lwd=2)

myplot(dates_out, inf_out, col="black")
abline(h=mean(inf_out), col="chocolate1", lwd=2)
abline(h=mean(inf_in), col='chocolate3', lwd=2)
par(new=TRUE)
lines( dates_out, arma96_predd , t='l' , lwd=2, col='blue2')
par(new=TRUE)
lines( dates_out, arma12_1_predd, t='l' , lwd=2 , col='green1')
par(new=TRUE)
lines( dates_out, ar12_predd, t='l', lwd=2, col='red2')
legend( 'bottomright' , c('Inflation','ARMA(9,6)', "ARMA(12,1)", "AR(12)", "Out-of-sample unconditional mean", "In-sample unconditional mean")  
        , col=c('black','blue2','green1', "red2", "chocolate1", "chocolate3") , lwd=2)


# OUt-of-sample goodness of fit: MSE and Diebold-Mariano test
library(sandwich)
ar12_mse <- mean((inf_out-ar12_pred1)**2)
arma12_1_mse <- mean((inf_out-arma12_1_pred1)**2)
arma96_mse <- mean((inf_out-arma96_pred1)**2)
mse <- round(cbind(arma96_mse, ar12_mse, arma12_1_mse),3)
mse

bench <- (inf_out-rep(mean(inf_in), times=length(inf_out))**2)
d_ar12 <- (inf_out-ar12_pred1)**2 - bench
d_arma12_1 <- (inf_out-arma12_1_pred1)**2 - bench
d_arma96 <- (inf_out-arma96_pred1)**2 - bench

DM_ar12 <- mean(d_ar12)/sqrt(lrvar(d_ar12))
DM_arma12_1 <- mean(d_arma12_1)/sqrt(lrvar(d_arma12_1))
DM_arma96 <- mean(d_arma96)/sqrt(lrvar(d_arma96))

DM_ar12.pval <- 1-pnorm(DM_ar12)
DM_arma12_1.pval <- 1-pnorm(DM_arma12_1)
DM_arma96.pval <- 1-pnorm(DM_arma96)

DM.pval <- round(cbind(DM_arma96.pval, DM_ar12.pval, DM_arma12_1.pval),3)
DM.pval

d4_ar12 <- (inf_out-ar12_pred1)**2 - (inf_out-arma96_pred1)**2
d5_arma12_1 <- (inf_out-arma12_1_pred1)**2 - (inf_out-arma96_pred1)**2

DM4_ar12 <- mean(d4_ar12)/sqrt(lrvar(d4_ar12))
DM5_arma12_1 <- mean(d5_arma12_1)/sqrt(lrvar(d5_arma12_1))

DM4.pval <- 1-pnorm(DM_ar12)
DM5.pval <- 1-pnorm(DM5_arma12_1)

d6_arma12_1 <- (inf_out-arma12_1_pred1)**2 - (inf_out-ar12_pred1)**2

DM6_arma12_1 <- mean(d6_arma12_1)/sqrt(lrvar(d6_arma12_1))
DM6.pval <- 1-pnorm(DM6_arma12_1)

bench_v <- cbind(1, DM_arma96.pval, DM_ar12.pval, DM_arma12_1.pval)
arma96_v <- cbind(DM_arma96.pval, 1, DM4.pval, DM5.pval)
ar12_v <- cbind(DM_ar12.pval, DM4.pval, 1, DM6.pval)
arma12_1_v <- cbind(DM_arma12_1.pval, DM5.pval, DM6.pval,1)

DieboldMarianos <- round(rbind(bench_v, arma96_v, ar12_v, arma12_1_v),3)

colnames(DieboldMarianos) <- c("Uncond. Mean", "ARMA(9,6)", "AR(12)", "ARMA(12,1)")
rownames(DieboldMarianos)<- c("Uncond. Mean", "ARMA(9,6)", "AR(12)", "ARMA(12,1)")
DieboldMarianos

