%Part Two
clear all
close all
clc

%Question 1
data = xlsread('Keeling_CO2data_2017.xlsx',1);
CO2 = data(:,5)
timeVec = (1958:1/12:2016+9/12)
timeVec = transpose(timeVec)
figure
plot(timeVec,CO2)
xlim([timeVec(1),timeVec(end)])
xlabel('Time')
ylabel('Measured CO2')
%Appears to be trending seasonally. Periodic increases and decreases likely
%coincide with warm and cold seasons in developed countries

%Question 2

timeVec1 = timeVec(1:565)
CO2_2005 = CO2(1:565)
timeVec1sq = timeVec1.^2

%Linear_Model = LinearModel.fit(timeVec1,CO2_2005)
%beta = mvregress(timeVec1,CO2_2005)
model = fitlm(timeVec1,CO2_2005)
LinearFit = predict(model)
figure
plot(timeVec1,CO2_2005,'-',timeVec1,LinearFit,'r')
xlim([timeVec1(1),timeVec1(end)])
xlabel('Time')
ylabel('CO2')
%Would be good to graph the linear model prediction on the original data
%through 2016- keep looking to do this
%Clearly, the linear model has a high R2 and appears to follow the trend,
%but does not predict any of the seasonal variation
figure
plot(timeVec,CO2,'-',timeVec1,LinearFit,'r')
xlim([timeVec(1),timeVec(end)])
xlabel('Time')
ylabel('CO2')


%Question 3

data1 = table(timeVec1,timeVec1sq,CO2_2005,'VariableNames',{'timeVec1','timeVec1sq','Co2_2005'})
model1 = fitlm(data1)
QuadraticFit = predict(model1)
figure
plot(timeVec1,CO2_2005,'-',timeVec1,QuadraticFit,'r')
xlim([timeVec1(1),timeVec1(end)])
xlabel('Time')
ylabel('CO2')

figure
plot(timeVec,CO2,'-',timeVec1,QuadraticFit,'r')
xlim([timeVec(1),timeVec(end)])
xlabel('Time')
ylabel('CO2')

%Question 4- Done using seasonal differences

[h,pValue] = adftest(CO2_2005)
%P-Value=.9936, fail to reject null; we have a non stationary process which
%needs to be seasonally differenced

Diffs = CO2_2005 - lagmatrix(CO2_2005,12)
Adj_diffs = Diffs(14:end)
timeVec2 = timeVec1(14:end)
figure
autocorr(Adj_diffs,10)
xlim([0,10])

figure
parcorr(Adj_diffs,10)
xlim([0,10])

maxAR = 4; % max of 4 AR terms
maxMA = 4; % max of 4 MA terms

CO2ModelCriteria = zeros((maxAR+1)*(maxMA+1)-1,2);
ind = 1;
lagCombinations = CO2ModelCriteria;

% Double for loop over AR and MA lags
% Good practice to use 'ii' instead of 'i' as loop index (like in other
% programming languages) because MATLAB stores 'i' as the imaginary
% constant sqrt(-1).
for ii = 0:maxAR
    for jj = 0:maxMA
        if ii ~= 0 || jj ~= 0
            % Create ARIMA model object with 'ii' AR terms and 'jj' MA terms
            model2 = arima(ii,0,jj);
            % Estimate ARIMA model and save maximized value of log likelihood
            [~,~,logL] = estimate(model2,Adj_diffs);
            % Compute corresponding AIC and BIC values
            CO2ModelCriteria(ind,:) = aicbic(logL,ii+jj,length(Adj_diffs)-max(ii,jj));
            lagCombinations(ind,:) = [ii jj];
            ind = ind + 1;
        end
    end
end

% Find models with lowest AIC and BIC criteria, corresponding to best
% statistical fit
[~,minIndices] = min(CO2ModelCriteria);

% Store the number of AR and MA terms associated with best model according
% to AIC
bestAICModelLags = lagCombinations(minIndices(1),:);

% Estimate model chosen by AIC
model2 = arima(bestAICModelLags(1,1),0,bestAICModelLags(1,2));
modelEstimate = estimate(model2,Adj_diffs);

% Compute residuals from fitted ARMA model using 'infer' command
resid = infer(modelEstimate,Adj_diffs);
% Test residuals for serial correlation
[h,pValue] = lbqtest(resid,'lags',10)
% Visually inspect fitted residuals
figure
plot(timeVec2,resid)
xlim([timeVec2(1) timeVec2(end)])

% Construct fitted values from model residuals and original time series
CO2Fit = Adj_diffs - resid;

% Plot time series and fitted values
figure
plot(timeVec2,[CO2Fit Adj_diffs])
legend('Fitted values','Data')
xlim([timeVec2(1) timeVec2(end)])




ForecastY0 = [1,ForecastX0]*betaHat0;

Ym = min([y0;ForecastY0]);
YM = max([y0;ForecastY0]);

figure
hold on
plot(dates,y0,'k','LineWidth',2);
plot(D:D+1,[y0(end);ForecastY0],'*-.k','LineWidth',2)
fill([D D D+1 D+1],[Ym YM YM Ym],'b','FaceAlpha',0.1)
hold off
legend(respName0,'Location','NW')
xlabel('Year')
ylabel('Response Level')
title('{\bf Forecast Response}')
axis tight
grid on







%{
Question 4- Done using RESIDUALS method
First, test for stationarity
[h,pValue] = adftest(CO2_2005)
P-Value=.9936, fail to reject null; we have a non stationary process which
needs to be seasonally differenced
months = data(:,2)
months_2005 = months(1:565)
seasonality = dummyvar(months_2005)
jan = seasonality(:,1)
feb = seasonality(:,2)
mar = seasonality(:,3)
apr = seasonality(:,4)
may = seasonality(:,5)
jun = seasonality(:,6)
jul = seasonality(:,7)
aug = seasonality(:,8)
sep = seasonality(:,9)
oct = seasonality(:,10)
nov = seasonality(:,11)
dec = seasonality(:,12)

data2 = table(timeVec1,timeVec1sq,jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,CO2_2005,'VariableNames',{'timeVec1','timeVec1sq','jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','Co2_2005'})
model2 = fitlm(data2)
Residuals = model2.Residuals.Raw
%ypred = predict(model2,timeVec1)

model3 = arima(4,0,4)
%ModelEstimate = estimate(model3,CO2_2005,'jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov')
ModelEstimate = estimate(model3,Residuals)
% Compute residuals from fitted ARMA model using 'infer' command
resid = infer(ModelEstimate,CO2_2005);
% Test residuals for serial correlation
[h,pValue] = lbqtest(resid,'lags',10)
% Visually inspect fitted residuals
%figure
%plot(timeVec1,resid)
%xlim([timeVec1(1) timeVec1(end)])
CO2_2005_2 = CO2_2005(2:end)
timeVec2 = timeVec1(2:end)
% Construct fitted values from model residuals and original time series
CO2fit = CO2_2005_2 - resid;

% Plot time series and fitted values
figure
plot(timeVec2,[CO2fit CO2_2005_2])
legend('Fitted values','Data')
xlim([timeVec2(1) timeVec2(end)])
%}

%Question 5

%Question 6
loaddap('http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CMB/.GLOBAL/.Reyn_SmithOIv2/.monthly/.sst/DATA/2/STEP/dods')

