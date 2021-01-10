%% EMIII - Problem Set #2 - Question 2
% Álvaro Corrales Cano & Emilio Esguerra

% Preparation
clear all; clearvars -global;
rng(1);

%% Part A: Preliminaries

% 1. Download series from FRED
% 2. Load series into Matlab and transform
    % Set Timerange
    range = timerange('1960-01-01','2017-10-01','closed');
    range_lag = timerange('1959-10-01','2017-10-01','closed');
    
    %(a) US real GDP
    GDPC1 = xlsread('GDPC1.xls','read');
    t1 = datetime(1947,1,1);
    t2 = datetime(2018,1,1);
    t = (t1:calmonths(3):t2)'; 
    
    RGDP_tt = timetable(t,GDPC1);
    RGDP = table2array(RGDP_tt(range_lag,1));
    
    y = 100*diff(log(RGDP));
    
    %(b) 10Y Treasury Constant Maturity Rate (downloaded monthly)
    GS10_data = xlsread('GS10.xls','read');
    t1 = datetime(1953,4,1);
    t2 = datetime(2018,1,1);
    t = (t1:calmonths(3):t2)';
    GS10_tt = timetable(t,GS10_data);
    GS10 = table2array(GS10_tt(range,1));
    
    %(c) 3M Treasury Bill (downloaded monthly)
    TB3MS_data = xlsread('TB3MS.xls','read');
    t1 = datetime(1934,1,1);
    t2 = datetime(2018,1,1);
    t = (t1:calmonths(3):t2)';
    TB3MS_tt = timetable(t,TB3MS_data);
    TB3MS = table2array(TB3MS_tt(range,1));
    
    x = GS10-TB3MS;
    
%3. Plot the series
date_start = datetime(1960,1,1);
date_end = datetime(2017,10,1);
time = linspace(date_start,date_end,232)';
figure(2147483646)
plot(time,y,time,x);
legend('GDP growth', 'Spread')

%% Part B: Forecasting with AR(1) and ARDL

T=232;
R=100;
P=132;

%(1) Forecast for M1 (AR(1)) (Exec. 4-10)

y_M1 = NaN(P,1);
fe_M1 = NaN(P,1);

for m = 1:P
    X = [ones(R-2+m,1),y(1:R-2+m)];
    Y = y(2:R-1+m);
    b = (X'*X)\(X'*Y);
    y_M1(m,1)=b(1,1)+b(2,1)*y(R-1+m,1); 
    fe_M1(m,1)=y(R+m,1)-y_M1(m,1);
end


%(2) Forecast for M2 (ARDL) (Exec. 4-10)

y_M2 = NaN(P,1);
fe_M2 = NaN(P,1);

for m = 1:P
    X = [ones(R-2+m,1),y(1:R-2+m),x(1:R-2+m)];
    Y = y(2:R-1+m);
    b = (X'*X)\(X'*Y);
    y_M2(m,1)=b(1,1)+b(2,1)*y(R-1+m,1)+b(3,1)*x(R-1+m,1); 
    fe_M2(m,1)=y(R+m,1)-y_M2(m,1);
end

%(3) Combined Forecast (Exec. 11)
y_c = 0.5*y_M1+0.5*y_M2;
fe_c = y(101:T) - y_c;

%% Part C: FORECAST EVALUATION 
% 12. Plot
startDate = datetime('Q1-1985','Format','QQ-yyyy');
endDate = datetime('Q4-2017','Format','QQ-yyyy');
time = linspace(startDate,endDate,132)';
y_true = y(101:T);

figure(1)
plot(time, y_true, time, y_M1, time, y_M2, time, y_c)
legend('Actual data', 'Model 1', 'Model 2' , 'Combined model')

l2 = fe_M1 - fe_M2;
lc = fe_M1 - fe_c;

figure(2)
plot(time, l2, time, lc)
legend('l2', 'lc')

% 13. Plot
figure(3)
subplot(3,2,1)
plot(time, y_true)
title('Actual data')

subplot(3,2,2)
plot(time, y_M1, 'c')
title('Model 1 - Forecast')

subplot(3,2,3)
plot(time, y_true)
title('Actual data')

subplot(3,2,4)
plot(time, y_M2, 'g')
title('Model 2 - Forecast')

subplot(3,2,5)
plot(time, y_true)
title('Actual data')

subplot(3,2,6)
plot(time, y_c, 'r')
title('Combined Forecast')

% (14) MSE
mse_M1 = (sum(fe_M1.^2))/P;   % Smallest!
mse_M2 = (sum(fe_M2.^2))/P;
mse_c = (sum(fe_c.^2))/P;

mse = [mse_M1,mse_M2,mse_c]';
mse_table = table(mse)

% (15)
DM_12 = dmtest(fe_M1, fe_M2, 1)
if abs(DM_12)<1.96
    disp('Do not reject H0')
else disp('Reject H0')
end

DM_1c = dmtest(fe_M1, fe_c, 1)
if abs(DM_1c)<1.96
    disp('Do not reject H0')
else disp('Reject H0')
end

%(16) ME, MAE, RMSE

%(i) Model 1 - AR(1)
me_M1 = mean(fe_M1);
mae_M1 = mean(abs(fe_M1));
rmse_M1 = sqrt(mse_M1);

%(ii) Model 2 - ARDL
me_M2 = mean(fe_M2);
mae_M2 = mean(abs(fe_M2));
rmse_M2 = sqrt(mse_M2);

%(iii) Combined Model
me_c = mean(fe_c);
mae_c = mean(abs(fe_c));
rmse_c = sqrt(mse_c);

me=[me_M1,me_M2,me_c]';
mae=[mae_M1,mae_M2,mae_c]';
rmse=[rmse_M1,rmse_M2,rmse_c]';

stats=table(me,mae,rmse)