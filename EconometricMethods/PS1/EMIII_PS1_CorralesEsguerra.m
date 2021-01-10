%% EM III - Problem Set 1 - Question 3
% Álvaro Corrales Cano & Emilio Esguerra

%Housekeeping
clear all;

%Set Seed
rng(4560);

%Set Observations
T = 100;
t = (1:T)';

%% (a)

%(i) White Noise
WN = normrnd(0,0.5,T,1);

%(ii) Moving Average MA(1)
MA1 = arima('Constant',1.2,'MA',{0.6},'Variance',0.5);
MA1_val = simulate(MA1,T);

%(iii) Autoregressive Process AR(1)
AR1 = arima('Constant',0.2,'AR',{0.9},'Variance',0.5);
AR1_val = simulate(AR1,T);

%(iv) Autoregressive Process AR(2)
AR2 = arima('Constant',1,'AR',{0.6,0.2},'Variance',0.5);
AR2_val = simulate(AR2,T);

%% (b)

%(i) Plot the series in a graph
figure(1)
subplot(2,2,1)
plot(t,WN,'m')
title('White Noise Process WN')
subplot(2,2,2)
plot(t,MA1_val,'c')
title('Moving Average Process MA(1)')
subplot(2,2,3)
plot(t,AR1_val,'g')
title('Autoregressive Process AR(1)')
subplot(2,2,4)
plot(t,AR2_val,'r')
title('Autoregressive Process AR(2)')

%(ii) Compute Sample Mean, Variance and Autocovariances up to order j=10
A = horzcat(WN,MA1_val,AR1_val,AR2_val);  % Generate matrix with the four models - Every column contains the values of one model
A_mean = mean(A)                          % Mean of each column of matrix A (WN, MA(1), AR(1), AR(2)
A_var = var(A)                            % Variance of each column of matrix A (WN, MA(1), AR(1), AR(2))

acf_WN = autocorr(WN,10);
acf_MA1 = autocorr(MA1_val,10);
acf_AR1 = autocorr(AR1_val,10);
acf_AR2 = autocorr(AR2_val,10);
acov_WN = acf_WN*A_var(1,1);
acov_MA1 = acf_MA1*A_var(1,2);
acov_AR1 = acf_AR1*A_var(1,3);
acov_AR2 = acf_AR2*A_var(1,4);

A_autocov = [acov_WN,acov_MA1,acov_AR1,acov_AR2] %Each column contains autocovariances of order 0 to 10 for the models (WN, MA(1), AR(1), AR(2))
clear acf* acov*

%(iii) Compare Sample Moments with Population mean, variance and
%autocovariances

%See solutions in LaTex-File

%(d) Sample and Population Autocorrelation
%Generate sample autocorrelation
[acf_WN,bounds_WN]=autocorr(WN,10,2);       
[acf_MA1,bounds_MA1]=autocorr(MA1_val,10,2);
[acf_AR1,bounds_AR1]=autocorr(AR1_val,10,2);
[acf_AR2,bounds_AR2]=autocorr(AR2_val,10,2);

%Generate Vectors with calculated population autocorrelations
x=0:10;
Pop_Autocorr_WN = [1,0,0,0,0,0,0,0,0,0,0]';
Pop_Autocorr_MA1= [1,0.4412,0,0,0,0,0,0,0,0,0]';
Pop_Autocorr_AR1= [1,0.9,0.81,0.729,0.6561,0.5905,0.5314,0.4783,0.4305,0.3874,0.3487]';
Pop_Autocorr_AR2= [1,0.75,0.65,0.54,0.4740,0.3804,0.3190,0.2675,0.2243,0.1881,0.1577]';

%Plot sample v population autocorrelations
figure(2)
subplot(2,2,1)
autocorr(WN,10)
hold on 
stem(x,Pop_Autocorr_WN)
hold off
title('Autocorrelation - WN')
subplot(2,2,2)
autocorr(MA1_val,10)
hold on 
stem(x,Pop_Autocorr_MA1)
hold off
title('Autocorrelation - MA(1)')
subplot(2,2,3)
autocorr(AR1_val,10)
hold on 
stem(x,Pop_Autocorr_AR1)
hold off
title('Autocorrelation - AR(1)')
subplot(2,2,4)
autocorr(AR2_val,10)
hold on 
stem(x,Pop_Autocorr_AR2)
hold off
title('Autocorrelation - AR(2)')

%% (c)

%(i) Estimate regression model and report coefficients

Mdl_AR1 = arima(1,0,0);
[EstMdl_AR1,EstParamCov_AR1] = estimate(Mdl_AR1, AR1_val,'Display','off');
print(EstMdl_AR1,EstParamCov_AR1)

%(ii) Report 95% CI
c =  EstMdl_AR1.AR;
beta1 = cell2mat(c);
sd = sqrt(EstParamCov_AR1(2,2));
CI = [beta1-1.96*sd,beta1+1.96*sd]

%% (d)

%(i) Estimate AR(1) regression model and report coefficients
Mdl_AR1 = arima(1,0,0);
[EstMdl_AR2,EstParamCov_AR2] = estimate(Mdl_AR1,AR2_val,'Display','off');
print(EstMdl_AR2,EstParamCov_AR2)

%(ii) Compute residuals
EstMdl_AR2 = estimate(Mdl_AR1,AR2_val);
res = infer(EstMdl_AR2,AR2_val)

%(iii) Test on autocorrelation of residuals
[lbtw, pval] = lbqtest(res,'lags',1);         % Ljung-Box test of autocorrelation of order 1
pval                                          % We can reject the null of
                                              % no autocorrelation at 5%
                                              % confidence level
                                         

%(iv) Estimate AR(2) regression model and report coefficients
Mdl_AR2 = arima(2,0,0);
[EstMdl_AR2_2,EstParamCov_AR2_2] = estimate(Mdl_AR2,AR2_val,'Display','off');
print(EstMdl_AR2_2,EstParamCov_AR2_2)

%(v) Compute residuals
res_2 = infer(EstMdl_AR2_2,AR2_val)

%(vi) Test on autocorrelation of residuals
[lbtw, pval_2] = lbqtest(res_2,'lags',1);
pval_2                                      % Ljung-Box test of autocorrelation of order 1
                                            % We can't reject the null of
                                            % no autocorrelation at 5%
                                            % confidence level