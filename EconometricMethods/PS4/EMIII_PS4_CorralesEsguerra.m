%% EMIII - Problem Set #4 - Exercise 3
% % Álvaro Corrales Cano & Emilio Esguerra

% Preparation
clear all; clearvars -global;
rng(1);

%% (1) Load dataset
% Load dataset
data = load('VAR3.mat');

%Extract Time Series
gdp = data.gdp;
inf = data.inf;
str = data.str;

%Set Time Range
t1 = datetime(1948,1,1);
t2 = datetime(2015,10,1);
time = (t1:calmonths(3):t2)';

%% (2) Estimate trivariate VAR

%Set parameters
K = 3;                          %No. of Variables
S=length(gdp);                 %Full Sample length 68years x 4quarters = 272

%Creat empty Vectors for AIC and BIC
AIC = NaN(1,8);
BIC = NaN(1,8);

%First Round for p=1
for p=1  
    %Set counter
    numstor = strcat('lag',num2str(p));

    %Set Frame of Observations for p lags    
    T=S-p;                           

    %Select sample values and set up matrix Y
    gdp_sample = gdp(p+1:S);
    inf_sample = inf(p+1:S);
    str_sample = str(p+1:S);

    Y = [gdp_sample,inf_sample,str_sample]';
    Ystore.(numstor) = Y;

    %Select pre-sample values and set up matrix Z
    gdp_pre = gdp(1:T);
    inf_pre = inf(1:T);
    str_pre = str(1:T);
    ones = ones(T,1);

    Z = [ones,gdp_pre,inf_pre,str_pre]';
    Zstore.(numstor) = Z;

    %OLS Estimation
    A.(numstor) = (Y*Z')*inv((Z*Z'))
    clearvars ones
    
    %Error
    Uhat = Y - A.(numstor)*Z;
    
    %AIC
    AIC(:,p) = log(det((1/T)*Uhat*Uhat'))+(2/T)*(K^2*p+K);
    
    %BIC
    BIC(:,p) = log(det((1/T)*Uhat*Uhat'))+(log(T)/T)*(K^2*p+K);
end

%Next Rounds for p=2...8

for p=2:8
    %Continue counter
    numstor = strcat('lag',num2str(p));
    
    %Set Frame of Observations for p lags    
    T=S-p;                          

    %Select sample values and set up matrix Y
    gdp_sample = gdp(p+1:S);
    inf_sample = inf(p+1:S);
    str_sample = str(p+1:S);

    Y = [gdp_sample,inf_sample,str_sample]';
    Ystore.(numstor) = Y;
    
    %Select pre-sample values and set up matrix Z
    gdp_pre = gdp(1:T);
    inf_pre = inf(1:T);
    str_pre = str(1:T);
    
    Z_cut = Z(:,2:end);
    
    Z = [Z_cut',gdp_pre,inf_pre,str_pre]';
    Zstore.(numstor) = Z;
    
    %OLS Estimation
    A.(numstor) = (Y*Z')*inv((Z*Z'))
    
    %Error
    Uhat = Y - A.(numstor)*Z;
    
    %AIC
    AIC(:,p) = log(det((1/T)*Uhat*Uhat'))+(2/T)*(K^2*p+K);
    
    %BIC
    BIC(:,p) = log(det((1/T)*Uhat*Uhat'))+(log(T)/T)*(K^2*p+K);
end

%% (3) AIC and BIC criterion

disp(AIC)
disp(BIC)

%% (4) From now on p=2. Establish whether covariance stationary

Alag2 = A.lag2;  %Estimated A Matrix with constant
Zlag2 = Zstore.lag2;

%Get c, A1 and A2
c = Alag2(:,1);
A1 = Alag2(:,2:4);
A2 = Alag2(:,5:7);

I = eye(3);
O = zeros(3);
%Construct Companion Matrix
LS = [A1;I];
RS = [A2;O];
Gamma = [LS,RS];

%Get eigenvalues and calculate absolute values
eig = eig(Gamma);

eig_real = real(eig).^2;
eig_imag = imag(eig).^2;
eig_abs = (eig_real+eig_imag).^0.5;

%All Eigenvalues are less than 1 in absolute value -> covariance stationary

%% (5) Plot actual and fitted values for each variable in VAR (p=2)

%Fitted values
Yhat_lag2 = Alag2*Zlag2;

gdp_hat = [NaN,NaN,Yhat_lag2(1,:)]';
inf_hat = [NaN,NaN,Yhat_lag2(2,:)]';
str_hat = [NaN,NaN,Yhat_lag2(3,:)]';

%Plots
figure(1)
plot(time,gdp,time,gdp_hat)
legend('GDP (actual)','GDP (VAR(2))')
figure(2)
plot(time,inf,time,inf_hat)
legend('Inflation (actual)','Inflation (VAR(2))')
figure(3)
plot(time,str,time,str_hat)
legend('3M Treasury Bill (actual)','3M Treasury Bill (VAR(2))')

%% (6) Autocorrelation function up to order 20

%Calculate Residuals
Ylag2 = Ystore.lag2;
Ulag2 = Ylag2 - Alag2*Zlag2;

%Autocorrelation
figure(4)
subplot(2,2,1)
autocorr(Ulag2(1,:),20)       %GDP
title('Autocorrelation Residuals - GDP')
subplot(2,2,2)
autocorr(Ulag2(2,:),20)       %Inflation
title('Autocorrelation Residuals - Inflation')
subplot(2,2,3)
autocorr(Ulag2(3,:),20)       %Treasury Bill
title('Autocorrelation Residuals - Treasury Bill')

%% (7) Unit contractionary monetary policy shock

%Setup
eta = [0,0,1]';     %Define structural shock
Sigma = Ulag2*Ulag2'*(1/270);
P = chol(Sigma,'lower');

%IRF
IRF_t = P*eta;
IRF_t1 = A1*IRF_t;
IRF_t2 = A1*IRF_t1+A2*IRF_t;

IRF = NaN(3,21);
IRF(:,1)=IRF_t;
IRF(:,2)=IRF_t1;
IRF(:,3)=IRF_t2;

for m=4:21
   IRF(:,m)=A1*IRF(:,m-1)+A2*IRF(:,m-2);
end

lags=(0:1:20);
figure(7)
subplot(3,1,1)
plot(lags,IRF(1,:))
legend('IRF - GDP')
subplot(3,1,2)
plot(lags,IRF(2,:),'r')
legend('IRF - Inflation')
subplot(3,1,3)
plot(lags,IRF(3,:),'g')
legend('IRF - Treasury Bill')

%% (8) Confidence Intervals by Bootstraping

%Prepare Loop
IRF_booty_gdp = NaN(1000,21);
IRF_booty_inf = NaN(1000,21);
IRF_booty_str = NaN(1000,21);
rng(155);

for s=1:1000
% (1) Resample from VAR(2) residuals with replacement
Ulag2_star = NaN(3,270);
for r=1:270
    x =ceil(rand*270);
    Ulag2_star(:,r)=Ulag2(:,x);
end
%(2) Generate a sample of bootstrap data of size T
Yhat_lag2_star = NaN(3,270);
Yhat_lag2_star(:,1:2) = Alag2*Zlag2(:,1:2)+Ulag2_star(:,1:2);

for w=3:270
Yhat_lag2_star(:,w) = c + A1*Yhat_lag2_star(:,w-1) + A2*Yhat_lag2_star(:,w-2) + Ulag2_star(:,w);
end
%Yhat_lag2_star = Alag2*Zlag2+Ulag2_star;
%(3) Estimate VAR(2) Model
    %(3a) Generate Y and Z matrix                           
        %Select sample values and set up matrix Y
        Y_mf = Yhat_lag2_star(:,3:end);
        %Select pre-sample values and set up matrix Z
        Z_mf = vertcat(ones(1,268),Yhat_lag2_star(:,2:end-1),Yhat_lag2_star(:,1:end-2));
    %(3b) OLS Estimation
    A_mf = (Y_mf*Z_mf')*((Z_mf*Z_mf')\eye(size(Z_mf*Z_mf')));
    c_mf = A_mf(:,1);
    A1_mf = A_mf(:,2:4);
    A2_mf = A_mf(:,5:7);
    U_mf = Yhat_lag2_star(:,3:end) - A_mf*Z_mf;
    Sigma_mf = U_mf*U_mf'*(1/length(U_mf));
    %(4) Construct IRFs;
    P_mf = chol(Sigma_mf,'lower');
    IRF_t_mf = P_mf*eta;
    IRF_t1_mf = A1_mf*IRF_t_mf;
    IRF_t2_mf = A1_mf*IRF_t1_mf+A2_mf*IRF_t_mf;
    IRF_mf = NaN(3,21);
    IRF_mf(:,1)=IRF_t_mf;
    IRF_mf(:,2)=IRF_t1_mf;
    IRF_mf(:,3)=IRF_t2_mf;
    for m=4:21
       IRF_mf(:,m)=A1_mf*IRF_mf(:,m-1)+A2_mf*IRF_mf(:,m-2);
    end
    IRF_mf_gdp = IRF_mf(1,:);
    IRF_mf_inf = IRF_mf(2,:);
    IRF_mf_str = IRF_mf(3,:);
    %(5) Store impulse responses;
    IRF_booty_gdp(s,:)=IRF_mf_gdp;
    IRF_booty_inf(s,:)=IRF_mf_inf;
    IRF_booty_str(s,:)=IRF_mf_str;
end

% Confidence intervals

CI_upperbooty_gdp = NaN(1,21);
CI_lowerbooty_gdp = NaN(1,21);
CI_upperbooty_inf = NaN(1,21);
CI_lowerbooty_inf = NaN(1,21);
CI_upperbooty_str = NaN(1,21);
CI_lowerbooty_str = NaN(1,21);

for l=1:21
    CI_upperbooty_gdp(:,l)=prctile(IRF_booty_gdp(:,l),97.5);
    CI_lowerbooty_gdp(:,l)=prctile(IRF_booty_gdp(:,l),2.5);
    CI_upperbooty_inf(:,l)=prctile(IRF_booty_inf(:,l),97.5);
    CI_lowerbooty_inf(:,l)=prctile(IRF_booty_inf(:,l),2.5);
    CI_upperbooty_str(:,l)=prctile(IRF_booty_str(:,l),97.5);
    CI_lowerbooty_str(:,l)=prctile(IRF_booty_str(:,l),2.5);
end

lags=(0:1:20);
figure(8)
subplot(3,1,1)
plot(lags,IRF(1,:),'b',lags,CI_upperbooty_gdp,'k',lags,CI_lowerbooty_gdp,'k')
legend('IRF - GDP','CI Bounds')
subplot(3,1,2)
plot(lags,IRF(2,:),'r',lags,CI_upperbooty_inf,'k',lags,CI_lowerbooty_inf,'k')
legend('IRF - Inflation','CI Bounds')
subplot(3,1,3)
plot(lags,IRF(3,:),'y',lags,CI_upperbooty_str,'k',lags,CI_lowerbooty_str,'k')
legend('IRF - Treasury Bill','CI Bounds')
