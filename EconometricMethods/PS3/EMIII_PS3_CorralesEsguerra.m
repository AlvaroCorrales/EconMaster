%% EMIII - Problem Set #3 - Question 2
% % Álvaro Corrales Cano & Emilio Esguerra

% Preparation
clear all; clearvars -global;
rng(1);

%Set parameters
lambda = 1600;

%% (1) Load the Data
%Set Time Range
t1 = datetime(1948,1,1);
t2 = datetime(2018,1,1);
time = (t1:calmonths(3):t2)';

%Load Data
data = xlsread('DataPS3.xls');
CNP16OV = data(:,1);
GDPC1 = data(:,2);
PCECC96 = data(:,3);
GDPIC1 = data(:,4);
PCDG = data(:,5);
PCECTPI = data(:,6);

%% (2) Transform Data
% (i) Series for Real Consumption of Durable Goods (CDG)
CDG = (PCDG./PCECC96)*100;

% (ii) Series for Investmeent (I)
I = GDPIC1+CDG;

% (iii) Creat logs
RGDPC = log(GDPC1./CNP16OV);
RCPC = log(PCECC96./CNP16OV);
RIPC = log(I./CNP16OV);

%Create Timetables
RGDPC = timetable(time,RGDPC);
RCPC = timetable(time,RCPC);
RIPC = timetable(time,RIPC);

%Test figure to check time series
figure(1);
plot(time,RGDPC.RGDPC,time,RCPC.RCPC,time,RIPC.RIPC);
legend('Output','Consumption','Investment','Location','Southeast');

%% (3) Detrend three obtained series by using HP Filter

[~,RGDPC_hp]=hpfilter(RGDPC.RGDPC,1600);
[~,RCPC_hp]=hpfilter(RCPC.RCPC,1600);
[~,RIPC_hp]=hpfilter(RIPC.RIPC,1600);

%% (4) Compute Statistics

% Output
RGDPC_hp_std = std(RGDPC_hp);                            %Standard Deviation
acf = autocorr(RGDPC_hp,1);
RGDPC_hp_ac1 = acf(2,1);                                 %Autocorrelation of order 1
r = corrcoef(RGDPC_hp,RGDPC_hp);
RGDPC_hp_corr = r(1,1);                                  %Correlation with RGDPC_hp
RGDPC_hp_tt = timetable(time,RGDPC_hp);
RGDPC_hp_lag4 = lag(RGDPC_hp_tt,4);
RGDPC_hp_lag4 = table2array(RGDPC_hp_lag4);
RGDPC_hp_lead4 = lag(RGDPC_hp_tt,-4);
RGDPC_hp_lead4 = table2array(RGDPC_hp_lead4);
r = corrcoef(RGDPC_hp(5:length(RGDPC_hp),:),RGDPC_hp(1:length(RGDPC_hp)-4,:));                          %Correlation with lag4 of RGDPC_hp - ALTERNATIVE
RGDPC_hp_lag4_corr = r(2,1);
r = corrcoef(RGDPC_hp(1:length(RGDPC_hp)-4,:),RGDPC_hp_lead4(1:length(RGDPC_hp)-4,:));                  %Correlation with lead4 of RGDPC_hp
RGDPC_hp_lead4_corr = r(2,1);

%Consumption
RCPC_hp_std = std(RCPC_hp);                             %Standard Deviation
acf = autocorr(RCPC_hp,1);
RCPC_hp_ac1 = acf(2,1);                                 %Autocorrelation of order 1
r = corrcoef(RCPC_hp,RGDPC_hp);
RCPC_hp_corr = r(2,1);                                  %Correlation with RGDPC_hp
r = corrcoef(RCPC_hp(5:length(RGDPC_hp),:),RGDPC_hp(1:length(RGDPC_hp)-4,:));                   %Correlation with lag4 of RGDPC_hp
RCPC_hp_lag4_corr = r(2,1);
r = corrcoef(RCPC_hp(1:length(RGDPC_hp)-4,:),RGDPC_hp(5:length(RGDPC_hp),:));                  %Correlation with lead4 of RGDPC_hp
RCPC_hp_lead4_corr = r(2,1);

%Investment
RIPC_hp_std = std(RIPC_hp);                             %Standard Deviation
acf = autocorr(RIPC_hp,1);
RIPC_hp_ac1 = acf(2,1);                                 %Autocorrelation of order 1
r = corrcoef(RIPC_hp,RGDPC_hp);
RIPC_hp_corr = r(2,1);                                  %Correlation with RGDPC_hp
r = corrcoef(RIPC_hp(5:length(RGDPC_hp),:),RGDPC_hp(1:length(RGDPC_hp)-4,:));                   %Correlation with lag4 of RGDPC_hp
RIPC_hp_lag4_corr = r(2,1);
r = corrcoef(RIPC_hp(1:length(RGDPC_hp)-4,:),RGDPC_hp(5:length(RGDPC_hp),:));%Correlation with lead4 of RGDPC_hp
RIPC_hp_lead4_corr = r(2,1);

%% (5) Display Table

stddev_hp = [RGDPC_hp_std,RCPC_hp_std,RIPC_hp_std]';
ac1_hp = [RGDPC_hp_ac1,RCPC_hp_ac1,RIPC_hp_ac1]';
corr_hp = [RGDPC_hp_corr,RCPC_hp_corr,RIPC_hp_corr]';
corr_lag4_hp = [RGDPC_hp_lag4_corr,RCPC_hp_lag4_corr,RIPC_hp_lag4_corr]';
corr_lead4_hp = [RGDPC_hp_lead4_corr,RCPC_hp_lead4_corr,RIPC_hp_lead4_corr]';

table_hp = table(stddev_hp,ac1_hp,corr_hp,corr_lag4_hp,corr_lead4_hp)

%% (6) Hamilton Filter

% Output
RGDPC_hf = NaN(length(data),1);
RGDPC_hf(1,:) = 0;
RGDPC_hf(2:12,:) = RGDPC.RGDPC(2:12) - RGDPC.RGDPC(1:11);

y = RGDPC.RGDPC;
X = [ones(length(RGDPC_hf)-12,1),y(1:length(RGDPC_hf)-12),y(2:length(RGDPC_hf)-11),y(3:length(RGDPC_hf)-10),y(4:length(RGDPC_hf)-9)];
Y = RGDPC.RGDPC(13:length(RGDPC_hf));
b = (X'*X)\(X'*Y);

for m = 13:length(RGDPC_hf)
    RGDPC_hf(m,:) = y(m,:) - (b(1,:) + b(2,:)*y(m-8,:) + b(3,:)*y(m-9,:) + b(4,:)*y(m-10,:) + b(5,:)*y(m-11,:));
end

figure(2)
plot(time, RGDPC_hf, time, RGDPC_hp)
legend('Hamilton filter', 'HP filter')

% Consumption
RCPC_hf = NaN(length(data),1);
RCPC_hf(1,:) = 0;
RCPC_hf(2:12,:) = RCPC.RCPC(2:12)- RCPC.RCPC(1:11);

y = RCPC.RCPC;
X = [ones(length(RCPC_hf)-12,1),y(1:length(RCPC_hf)-12),y(2:length(RCPC_hf)-11),y(3:length(RCPC_hf)-10),y(4:length(RCPC_hf)-9)];
Y = RCPC.RCPC(13:length(RCPC_hf));
b = (X'*X)\(X'*Y);

for m = 13:length(RCPC_hf)
    RCPC_hf(m,:) = y(m,:) - (b(1,:) + b(2,:)*y(m-8,:) + b(3,:)*y(m-9,:) + b(4,:)*y(m-10,:) + b(5,:)*y(m-11,:));
end

figure(3)
plot(time, RCPC_hf, time, RCPC_hp)
legend('Hamilton filter', 'HP filter')

% Investment
RIPC_hf = NaN(length(data),1);
RIPC_hf(1,:) = 0;
RIPC_hf(2:12,:) = RIPC.RIPC(2:12) - RIPC.RIPC(1:11);

y = RIPC.RIPC;
X = [ones(length(RIPC_hf)-12,1),y(1:length(RIPC_hf)-12),y(2:length(RIPC_hf)-11),y(3:length(RIPC_hf)-10),y(4:length(RIPC_hf)-9)];
Y = RIPC.RIPC(13:length(RIPC_hf));
b = (X'*X)\(X'*Y);

for m = 13:length(RIPC_hf)
    RIPC_hf(m,:) = y(m,:) - (b(1,:) + b(2,:)*y(m-8,:) + b(3,:)*y(m-9,:) + b(4,:)*y(m-10,:) + b(5,:)*y(m-11,:));
end

figure(4)
plot(time, RIPC_hf, time, RIPC_hp)
legend('Hamilton filter', 'HP filter')

%% (7) Stats for new table

% Output
RGDPC_hf_std = std(RGDPC_hf);                           %Standard Deviation
acf = autocorr(RGDPC_hf,1);
RGDPC_hf_ac1 = acf(2,1);                                 %Autocorrelation of order 1
r = corrcoef(RGDPC_hf,RGDPC_hf);
RGDPC_hf_corr = r(1,1);                                  %Correlation with RGDPC_hf
RGDPC_hf_tt = timetable(time,RGDPC_hf);
RGDPC_hf_lag4 = lag(RGDPC_hf_tt,4);
RGDPC_hf_lag4 = table2array(RGDPC_hf_lag4);
RGDPC_hf_lead4 = lag(RGDPC_hf_tt,-4);
RGDPC_hf_lead4 = table2array(RGDPC_hf_lead4);
r = corrcoef(RGDPC_hf(5:length(RGDPC_hf),:),RGDPC_hf(1:length(RGDPC_hf)-4,:));                          %Correlation with lag4 of RGDPC_hf - ALTERNATIVE
RGDPC_hf_lag4_corr = r(2,1);
r = corrcoef(RGDPC_hf(1:length(RGDPC_hf)-4,:),RGDPC_hf_lead4(1:length(RGDPC_hf)-4,:));                  %Correlation with lead4 of RGDPC_hf
RGDPC_hf_lead4_corr = r(2,1);

%Consumption
RCPC_hf_std = std(RCPC_hf);                             %Standard Deviation
acf = autocorr(RCPC_hf,1);
RCPC_hf_ac1 = acf(2,1);                                 %Autocorrelation of order 1
r = corrcoef(RCPC_hf,RGDPC_hf);
RCPC_hf_corr = r(2,1);                                  %Correlation with RGDPC_hf
r = corrcoef(RCPC_hf(5:length(RGDPC_hf),:),RGDPC_hf(1:length(RGDPC_hf)-4,:));                   %Correlation with lag4 of RGDPC_hf
RCPC_hf_lag4_corr = r(2,1);
r = corrcoef(RCPC_hf(1:length(RGDPC_hf)-4,:),RGDPC_hf(5:length(RGDPC_hf),:));                  %Correlation with lead4 of RGDPC_hf
RCPC_hf_lead4_corr = r(2,1);

%Investment
RIPC_hf_std = std(RIPC_hf);                             %Standard Deviation
acf = autocorr(RIPC_hf,1);
RIPC_hf_ac1 = acf(2,1);                                %Autocorrelation of order 1
r = corrcoef(RIPC_hf,RGDPC_hf);
RIPC_hf_corr = r(2,1);                                 %Correlation with RGDPC_hf
r = corrcoef(RIPC_hf(5:length(RGDPC_hf),:),RGDPC_hf(1:length(RGDPC_hf)-4,:));                   %Correlation with lag4 of RGDPC_hf
RIPC_hf_lag4_corr = r(2,1);
r = corrcoef(RIPC_hf(1:length(RGDPC_hf)-4,:),RGDPC_hf(5:length(RGDPC_hf),:));%Correlation with lead4 of RGDPC_hf
RIPC_hf_lead4_corr = r(2,1);

%% (8) Display Table Hamilton Filter

stddev_hf = [RGDPC_hf_std,RCPC_hf_std,RIPC_hf_std]';
ac1_hf = [RGDPC_hf_ac1,RCPC_hf_ac1,RIPC_hf_ac1]';
corr_hf = [RGDPC_hf_corr,RCPC_hf_corr,RIPC_hf_corr]';
corr_lag4_hf = [RGDPC_hf_lag4_corr,RCPC_hf_lag4_corr,RIPC_hf_lag4_corr]';
corr_lead4_hf = [RGDPC_hf_lead4_corr,RCPC_hf_lead4_corr,RIPC_hf_lead4_corr]';

table_hf = table(stddev_hf,ac1_hf,corr_hf,corr_lag4_hf,corr_lead4_hf)