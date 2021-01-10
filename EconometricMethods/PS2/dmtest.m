function DM = dmtest(e1, e2, h)
% Initialization
T = size(e1,1);
% Define the loss differential
d = e1.^2 - e2.^2;
% Ralculate the variance of the loss differential, taking into account
% autocorrelation.
dMean = mean(d);
gamma0 = var(d);
if h > 1
    gamma = zeros(h-1,1);
    for i = 1:h-1
        sampleCov = cov(d(1+i:T),d(1:T-i));
        gamma(i) = sampleCov(2);
    end
    varD = gamma0 + 2*sum(gamma);
else
    varD = gamma0;
end
% Retrieve the diebold mariano statistic DM ~N(0,1)
DM = dMean / sqrt ( (1/T)*varD );