data <- read.csv("forecast-competition-training.csv", header=TRUE)

predictor.team <- function(y){

f <- mean(y[,1])
 return( f )
}

predictor.team(data)

gleb <- function(y){
    D <- read.csv("forecast-competition-training.csv", header=TRUE)
    reg <- lm(TARGET~IMBFM+HPDYO+XUPP+RBEQZ+UKSKO+ZZICN+OIRTT+EGTZY, data=D)
    reg_pred    <- predict( reg , newdata=data.oos )
    
    return( reg_pred )
}

gleb(data)

library(tseries)
library(lmtest)

#looking at TARGET
plot(data$TARGET, type="l")
acf(data$TARGET)  #10-11 lags, exponential decay?
pacf(data$TARGET) #only 2 significant

acf(abs(data$TARGET))
acf((data$TARGET)**2) #not much, maybe garch not necessary

adf.test(data$TARGET) #stationary
jarque.bera.test(data$TARGET) # not normal
Box.test(data$TARGET, lag=22, type="Ljung-Box") #dependence

cor(data$TARGET, data$EDVDM) #we can do more correlations with other variables
cor(data)

# LEV PLOT

T <- length(data$TARGET)
r.plus  <- data$TARGET[1:(T-1)]*(data$TARGET[1:(T-1)]>0) 
r.minus <- data$TARGET[1:(T-1)]*(data$TARGET[1:(T-1)]<0) 
levreg  <- lm( abs(data$TARGET[2:T]) ~ 0+r.plus + r.minus )

plot( data$TARGET[1:(T-1)] , abs(data$TARGET[2:T]) , pch=16 , col='darkorange' , 
      yaxs='i' , tck=0.02 , xlab='lagged target', ylab='absolute target')
abline( a=0 , b=coef(levreg)['r.plus']  , col='red2' , lwd=2 )
abline( a=0 , b=coef(levreg)['r.minus'] , col='red2' , lwd=2 )
grid()

#no asymmetry?

#ARIMA models
ar1    = arima(data$TARGET,order=c(1,0,0))
ar2    = arima(data$TARGET,order=c(2,0,0))
ma1    = arima(data$TARGET,order=c(0,0,1))
arma11 = arima(data$TARGET,order=c(1,0,1))

#GOF
N <- length(data$TARGET)

ar1_aic    <- (-2*ar1$loglik+2*3)/N
ar2_aic    <- (-2*ar2$loglik+2*4)/N
ma1_aic    <- (-2*ma1$loglik+2*3)/N
arma11_aic <- (-2*arma11$loglik+2*4)/N

ar1_bic    <- (-2*ar1$loglik+log(N)*3)/N
ar2_bic    <- (-2*ar2$loglik+log(N)*4)/N
ma1_bic    <- (-2*ma1$loglik+log(N)*3)/N
arma11_bic <- (-2*arma11$loglik+log(N)*4)/N

ar1_aic
ar2_aic
ma1_aic
arma11_aic

plot(ar1$residuals, type="l", col="purple") #doesn't capture dependence
plot(ar2$residuals, type="l", col="purple")
plot(ma1$residuals, type="l", col="purple")
plot(arma11$residuals, type="l", col="purple")


# FITTED VALUES
ar1_mu     <- data$TARGET-ar1$residuals
ar2_mu     <- data$TARGET-ar2$residuals
ma1_mu     <- data$TARGET-ma1$residuals
arma11_mu  <- data$TARGET-arma11$residuals
ar1_res    <- as.numeric(ar1$residuals)
ar2_res    <- as.numeric(ar2$residuals)
ma1_res    <- as.numeric(ma1$residuals)
arma11_res <- as.numeric(arma11$residuals)

acf(ar1$residuals)
acf(ar2$residuals)
acf(ma1$residuals) #worst residuals
acf(arma11$residuals) #best

jarque.bera.test(ar1$residuals) #normal
jarque.bera.test(ar2$residuals) #more normal
jarque.bera.test(ma1$residuals) # not normal but less
jarque.bera.test(arma11$residuals) #most normal

Box.test(ar1$residuals, lag=25, type="Ljung-Box") #dependence
Box.test(ar2$residuals, lag=25, type="Ljung-Box") #independence
Box.test(ma1$residuals, lag=25, type="Ljung-Box") #dependence
Box.test(arma11$residuals, lag=25, type="Ljung-Box") # best independence //*//

I <- ncol(data)
for(i in data[,1:I]){
  print(cor(data$TARGET, i))
}

#check which columns have more than 50% correlation
abs(cor(data$TARGET, data[,]))
cors <- which(abs(cor(data$TARGET, data[,]))>0.5)

cor(data$TARGET[2:N], data$TARGET[1:(N-1)])
#cor with lag = 0.5, include??


#extract useful variables
data_1 <- data[, cors]

#check regression
reg <- lm(TARGET~.,data=data_1)
coeftest(reg)   #only 3 significant - sad

plot(reg$residuals, type="l", col="purple")

#compare residuals with arma11

jarque.bera.test(reg$residuals) #more normal
jarque.bera.test(arma11$residuals) #normal

Box.test(reg$residuals, lag=25, type="Ljung-Box") #dependence
Box.test(arma11$residuals, lag=25, type="Ljung-Box") #way more independent

#regression has more normal residuals but arma more independent

#split the data

data.is <- data_1[1:(nrow(data_1)*0.8),]
data.oos <- data_1[((nrow(data_1)*0.8)+1):nrow(data_1),]

y <- data.is[2:nrow(data.is),1]
x= data.is[1:(nrow(data.is)-1),2:ncol(data.is)]

model <- cbind(y,x)
model2 <- cbind(y,x,data.is[2:(nrow(data.is)),1])

reg_manel <- lm(y~.,data=model)
reg_alvaro <- lm(y~.,data=model2)

#models
reg <- lm(TARGET~.,data=data.is)
ar1    = arima(data.is$TARGET,order=c(1,0,0))
ar2    = arima(data.is$TARGET,order=c(2,0,0))
ma1    = arima(data.is$TARGET,order=c(0,0,1))
arma11 = arima(data.is$TARGET,order=c(1,0,1))

arma11_pred <- predict( arma11 , n.ahead=nrow(data.oos) )

arma11_pred2 <- predict(arma11, newdata=data.oos, n.ahead=nrow(data.oos))

ar1_pred    <- predict( ar1 , n.ahead=nrow(data.oos) )
ar2_pred    <- predict( ar2 , n.ahead=nrow(data.oos) )
ma1_pred    <- predict( ma1 , n.ahead=nrow(data.oos) )
reg_pred    <- predict( reg , newdata=data.oos )
reg_manel_pred <- predict(reg_manel, newdata=data.oos)
reg_alvaro_pred <- predict(reg_alvaro, newdata=data.oos)

ar1_mse    = mean( (data.oos$TARGET - as.numeric(ar1_pred$pred) )**2 )
ar2_mse    = mean( (data.oos$TARGET - as.numeric(ar2_pred$pred) )**2 )
ma1_mse    = mean( (data.oos$TARGET - as.numeric(ma1_pred$pred) )**2 )
arma11_mse = mean( (data.oos$TARGET - as.numeric(arma11_pred$pred) )**2 )
reg_mse    = mean( (data.oos$TARGET - as.numeric(reg_pred) )**2 )
reg_mse_manel    = mean( (data.oos$TARGET - as.numeric(reg_manel_pred) )**2 )

round(arma11_mse,3)
round(ar1_mse,3)
round(ar2_mse,3)
round(ma1_mse,3)
round(reg_mse,3)
round(reg_mse_manel,3)
#ar1 slightly better than arma11
#reg the best
#double check reg predict


par(mfrow=c(2,2))
plot(data.oos$TARGET, col="darkorange", type="l")
lines(reg_pred, col="purple")

plot(data.oos$TARGET, col="darkorange", type="l")
lines(arma11_pred2$pred, col="purple")

plot(data.oos$TARGET, col="darkorange", type="l")
lines(ar2_pred$pred, col="purple")

plot(data.oos$TARGET, col="darkorange", type="l")
lines(reg_pred, col="purple")

#superLOOP

#setup vectors
ar <- 1:3
ma <- 1:3
i <- 0

#limits
ar_lim <- max(ar)
ma_lim <- max(ma)
i_lim <- max(i)

#iterations
ar_loop = min(ar)
ma_loop = min(ma)

#matrix
mat <- matrix(0L, nrow = 3, ncol =ar_lim * ma_lim )
rownames(mat) <- c("intercept", "ar1", "ma1")
colnames(mat) <- rep("X", ar_lim*ma_lim)   #setup names, will be replaced during loop

while(ma_loop <= ma_lim)){
        #run arma
        arma <- arima(data$TARGET, c(ar_loop,0,ma_loop))
        
        #put coefs in matrix
        mat["intercept",ar_loop] <- arma$coef["intercept"]
        mat["ar1",ar_loop] <- arma$coef["ar1"]
        mat["ma1",ar_loop] <- arma$coef["ma1"]
        
        #replace colname
        colnames(mat)[ar_loop] = paste("ARMA", ar_loop, i_lim, ma_loop, sep="") 
        
        ar_loop = ar_loop+1
        
        if((ar_loop != ar_lim) & (ma_loop != ma_lim)){
            ar_loop = min(ar)
            ma_loop = ma_loop+1
            else
                break
        }
}

mat
#arma.loop
library(HH)
params = c(4,0,2)

X <- data$TARGET
X.loop <- if.R(
    s= arma.loop(X, model=list(order=params)),
    r= arma.loop(X, order=params)
)

X.dal <- diag.arma.loop(X.loop, x=X)
X.diag <- rearrange.diag.arma.loop(X.dal)
X.diagplot <- tsdiagplot(armas=X.loop, ts.diag=X.diag, lwd=1)
X.diagplot

sprintf("%.0f",params)
X.loop[["1","1"]]$coef
