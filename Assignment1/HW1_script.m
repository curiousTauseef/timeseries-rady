clear all
close all
clc

%Part 1

%UNEMPLOYMENT DATA
%Question 1
data = xlsread('time_series_data_2017.xlsx',1);
urate = data(:,2);
timeVec = (1948:1/12:2016+10/12)';
figure
plot(timeVec,urate)
xlim([timeVec(1) timeVec(end)])
xlabel('Time')
ylabel('Unemployment rate')
[h,pValue] = adftest(urate)
% Fail to reject, indicates presence of unit root, need to log first diffs
% so as to make it a stationary process
log_urate = log(urate)
log_urate_1stdiffs = diff(log_urate)
timeVec1 = timeVec(2:end)
%time vector adjusted so that vectors are same length
figure
plot(timeVec1,log_urate_1stdiffs)
xlim([timeVec1(1) timeVec1(end)])
xlabel('Time')
ylabel('Unemployment Rate Log First Differences')
%Question 2
%Plot autocorrelations
figure
autocorr(log_urate_1stdiffs,10)
xlim([0 10])
figure
parcorr(log_urate_1stdiffs,10)
xlim([0 10])
%Display autocorrelation values
urateAutocorrs = autocorr(log_urate_1stdiffs,10)

%Test significance of autocorr using LBQtest
disp('Unemployment rate serial correlation tests')
disp('1 lag')
[h,pValue] = lbqtest(log_urate_1stdiffs,'lags',1)
disp('10 lags')
[h,pValue] = lbqtest(log_urate_1stdiffs,'lags',10)
%Question 3
% Test out different combinations of AR and MA terms and use AIC and BIC to
% determine fit
maxAR = 4; % max of 4 AR terms
maxMA = 4; % max of 4 MA terms

urateModelCriteria = zeros((maxAR+1)*(maxMA+1)-1,2);
ind = 1;
lagCombinations = urateModelCriteria;

% Double for loop over AR and MA lags
% Good practice to use 'ii' instead of 'i' as loop index (like in other
% programming languages) because MATLAB stores 'i' as the imaginary
% constant sqrt(-1).
for ii = 0:maxAR
    for jj = 0:maxMA
        if ii ~= 0 || jj ~= 0
            % Create ARIMA model object with 'ii' AR terms and 'jj' MA terms
            model = arima(ii,0,jj);
            % Estimate ARIMA model and save maximized value of log likelihood
            [~,~,logL] = estimate(model,log_urate_1stdiffs);
            % Compute corresponding AIC and BIC values
            urateModelCriteria(ind,:) = aicbic(logL,ii+jj,length(log_urate_1stdiffs)-max(ii,jj));
            lagCombinations(ind,:) = [ii jj];
            ind = ind + 1;
        end
    end
end

% Find models with lowest AIC and BIC criteria, corresponding to best
% statistical fit
[~,minIndices] = min(urateModelCriteria);

% Store the number of AR and MA terms associated with best model according
% to AIC
bestAICModelLags = lagCombinations(minIndices(1),:);
%Looks like from this the best model is ARMA(4,4)
%Question 4
model = arima(bestAICModelLags(1,1),0,bestAICModelLags(1,2));
modelEstimate = estimate(model,log_urate_1stdiffs);
% Compute residuals from fitted ARMA model using 'infer' command
resid = infer(modelEstimate,log_urate_1stdiffs);
% Test residuals for serial correlation
[h,pValue] = lbqtest(resid,'lags',10)
%NOTE: we fail to reject, so no serial correlation in residuals, which is
%good
% Visually inspect fitted residuals
figure
plot(timeVec1,resid)
xlim([timeVec1(1) timeVec1(end)])

% Construct fitted values from model residuals and original time series
urateFit = log_urate_1stdiffs - resid;

% Plot time series and fitted values
figure
plot(timeVec1,[urateFit log_urate_1stdiffs])
legend('Fitted values','Data')
xlim([timeVec1(1) timeVec1(end)])

%CPIUCSL SERIES

%Question 1
clear all
close all
clc
data = xlsread('time_series_data_2017.xlsx',2);
cpiaucsl = data(:,2)
timeVec = (1947:1/12:2016+10/12)';
figure
plot(timeVec,cpiaucsl)
xlim([timeVec(1),timeVec(end)])
xlabel('Time')
ylabel('CPI Urban Consumers')
%Appears to be trending over time? DFuller Test below to test
[h,pValue] = adftest(cpiaucsl)
%Fail to reject, high p-value, which indicates non-stationary process; need
%to take first differences
log_cpi = log(cpiaucsl)
dlog_cpi = diff(log_cpi)
timeVec1 = timeVec(2:end)
figure
plot(timeVec1,dlog_cpi)
xlim([timeVec1(1),timeVec(end)])
xlabel('Time')
ylabel('Inflation Rate')

%Question 2
figure
autocorr(dlog_cpi,10)
xlim([0,10])
figure
parcorr(dlog_cpi,10)
xlim([0,10])
%Appears to be highly persistent, especially in ACF. Now display

%autocorrelation
dlog_cpiAutocorrs = autocorr(dlog_cpi,10)

%Test significance of autocorr using LBQtest
disp('CPI serial correlation tests')
disp('1 lag')
[h,pValue] = lbqtest(dlog_cpi,'lags',1)
disp('10 lags')
[h,pValue] = lbqtest(dlog_cpi,'lags',10)

%Suggests highly significant autocorrelation at 1 lag & 10 lags


%Question 3
%We will use 4 lags as the max as in previous case

maxAR = 4; % max of 4 AR terms
maxMA = 4; % max of 4 MA terms

CPIModelCriteria = zeros((maxAR+1)*(maxMA+1)-1,2);
ind = 1;
lagCombinations = CPIModelCriteria;
% constant sqrt(-1).
for ii = 0:maxAR
    for jj = 0:maxMA
        if ii ~= 0 || jj ~= 0
            % Create ARIMA model object with 'ii' AR terms and 'jj' MA terms
            model = arima(ii,0,jj);
            % Estimate ARIMA model and save maximized value of log likelihood
            [~,~,logL] = estimate(model,dlog_cpi);
            % Compute corresponding AIC and BIC values
            CPIModelCriteria(ind,:) = aicbic(logL,ii+jj,length(dlog_cpi)-max(ii,jj));
            lagCombinations(ind,:) = [ii jj];
            ind = ind + 1;
        end
    end
end

% Find models with lowest AIC and BIC criteria, corresponding to best
% statistical fit
[~,minIndices] = min(CPIModelCriteria);

% Store the number of AR and MA terms associated with best model according
% to AIC
bestAICModelLags = lagCombinations(minIndices(1),:);

%Best model appears to be an ARMA(4,3)

model = arima(bestAICModelLags(1,1),0,bestAICModelLags(1,2));
modelEstimate = estimate(model,dlog_cpi);

% Compute residuals from fitted ARMA model using 'infer' command
resid = infer(modelEstimate,dlog_cpi);

% Test residuals for serial correlation

[h,pValue] = lbqtest(resid,'lags',10)
%P-value fails to reject the null, which means we do not have significant
%correlation in our model's residuals
%Visual look at residuals
figure
plot(timeVec1,resid)
xlim([timeVec1(1) timeVec1(end)])


%Question 4
% Construct fitted values from model residuals and original time series
cpiFit = dlog_cpi - resid;
% Plot time series and fitted values
figure
plot(timeVec1,[cpiFit dlog_cpi])
legend('Fitted values','Actual Inflation Rate')
xlim([timeVec1(1) timeVec1(end)])
xlabel('Time')
ylabel('Inflation Rate')
title('Fitted Values Against Actual Values')

%GOLD PRICES
clear all
close all
clc
data = xlsread('time_series_data_2017.xlsx',3);
%Question 1
gold = data(:,2)
timeVec = (1968+3/12:1/12:2016+11/12)';
figure
plot(timeVec,gold)
xlim([timeVec(1),timeVec(end)])
xlabel('Time')
ylabel('Gold Price')
%DFuller Test for stationarity
[h,pValue] = adftest(gold)
%Fail to reject, high p-value, indicating non-stationarity, need to take
%first differences
log_gold = log(gold)
dlog_gold = diff(log_gold)
timeVec1 = timeVec(2:end)
figure
plot(timeVec1,dlog_gold)
xlim([timeVec1(1),timeVec1(end)])
xlabel('Time')
ylabel('Gold Rate of Return (continuous compounding')
%Question 2
figure
autocorr(dlog_gold,10)
xlim([0,10])
figure
parcorr(dlog_gold,10)
xlim([0,10])
%Check autocorrelation values
dlog_gold_autocorr = autocorr(dlog_gold,10)
%Check significance of autocorrelation
disp('Gold Rate of Return Serial Correlation Tests')
disp('1 lag')
[h,pValue] = lbqtest(dlog_gold,'lags',1)
disp('10 lags')
[h,pValue] = lbqtest(dlog_gold,'lags',10)
%LBQ Test there is not significant autocorrelation between lags, fail to
%reject pvalue=.08
%Question 3
%We will use 4 lags as the max as in previous case

maxAR = 4; % max of 4 AR terms
maxMA = 4; % max of 4 MA terms

GoldModelCriteria = zeros((maxAR+1)*(maxMA+1)-1,2);
ind = 1;
lagCombinations = GoldModelCriteria;
% constant sqrt(-1).
for ii = 0:maxAR
    for jj = 0:maxMA
        if ii ~= 0 || jj ~= 0
            % Create ARIMA model object with 'ii' AR terms and 'jj' MA terms
            model = arima(ii,0,jj);
            % Estimate ARIMA model and save maximized value of log likelihood
            [~,~,logL] = estimate(model,dlog_gold);
            % Compute corresponding AIC and BIC values
            GoldModelCriteria(ind,:) = aicbic(logL,ii+jj,length(dlog_gold)-max(ii,jj));
            lagCombinations(ind,:) = [ii jj];
            ind = ind + 1;
        end
    end
end

% Find models with lowest AIC and BIC criteria, corresponding to best
% statistical fit
[~,minIndices] = min(GoldModelCriteria);

% Store the number of AR and MA terms associated with best model according
% to AIC
bestAICModelLags = lagCombinations(minIndices(1),:);

%Best model appears to be an ARMA(4,4)
model = arima(bestAICModelLags(1,1),0,bestAICModelLags(1,2));
modelEstimate = estimate(model,dlog_gold);
% Compute residuals from fitted ARMA model using 'infer' command
resid = infer(modelEstimate,dlog_gold);
% Test residuals for serial correlation
[h,pValue] = lbqtest(resid,'lags',10)
%P-value fails to reject the null, which means we do not have serial correlation
%in the model
%Visual look at residuals
figure
plot(timeVec1,resid)
xlim([timeVec1(1) timeVec1(end)])
%Question 4
% Construct fitted values from model residuals and original time series
GoldFit = dlog_gold - resid;
% Plot time series and fitted values
figure
plot(timeVec1,[GoldFit dlog_gold])
legend('Fitted values','Gold Rate of Return')
xlim([timeVec1(1) timeVec1(end)])
xlabel('Time')
ylabel('Gold Rate of Return (continuous compounding')
title('Fitted Values Against Actual Values')


%STOCK PRICES
clear all
close all
clc
data = xlsread('time_series_data_2017.xlsx',4);
%Question 1
stocks = data(:,2)
timeVec = (1871:1/12:2017)';
figure
plot(timeVec,stocks)
xlim([timeVec(1),timeVec(end)])
xlabel('Time')
ylabel('Stock Price')
%DFuller to see if we have non-stationarity
[h,pValue]= adftest(stocks)
%Pvalue= 0.9 so fail to reject the null, suggesting we have a
%non-stationary process and need to take first differences
log_stocks = log(stocks)
dlog_stocks = diff(log_stocks)
timeVec1 = timeVec(2:end)
figure
plot(timeVec1,dlog_stocks)
xlim([timeVec1(1),timeVec1(end)])
xlabel('Time')
ylabel('Stock Rate of Return (continuously compounded)')


%Question 2
figure
autocorr(dlog_stocks,10)
xlim([0,10])
figure
parcorr(dlog_stocks,10)
xlim([0,10])
%Display autocorr values
stockautocorr = autocorr(dlog_stocks,10)
%Test significance of autocorrelation using LBQ Test
disp ('Stock Serial Autocorrelation Tests')
disp ('1 lag')
[h,pValue] = lbqtest(dlog_stocks,'lags',1)
disp ('10 lags')
[h,pValue] = lbqtest(dlog_stocks,'lags',10)
%P-values for both close to zero. Highly significant of autocorrelation between
%lags
%Question 3
%We will use 4 lags as the max as in previous case

maxAR = 4; % max of 4 AR terms
maxMA = 4; % max of 4 MA terms

StockModelCriteria = zeros((maxAR+1)*(maxMA+1)-1,2);
ind = 1;
lagCombinations = StockModelCriteria;
% constant sqrt(-1).
for ii = 0:maxAR
    for jj = 0:maxMA
        if ii ~= 0 || jj ~= 0
            % Create ARIMA model object with 'ii' AR terms and 'jj' MA terms
            model = arima(ii,0,jj);
            % Estimate ARIMA model and save maximized value of log likelihood
            [~,~,logL] = estimate(model,dlog_stocks);
            % Compute corresponding AIC and BIC values
            StockModelCriteria(ind,:) = aicbic(logL,ii+jj,length(dlog_stocks)-max(ii,jj));
            lagCombinations(ind,:) = [ii jj];
            ind = ind + 1;
        end
    end
end

% Find models with lowest AIC and BIC criteria, corresponding to best
% statistical fit
[~,minIndices] = min(StockModelCriteria);

% Store the number of AR and MA terms associated with best model according
% to AIC
bestAICModelLags = lagCombinations(minIndices(1),:);
%Best model appears to be an ARMA(2,4)
model = arima(bestAICModelLags(1,1),0,bestAICModelLags(1,2));
modelEstimate = estimate(model,dlog_stocks);
% Compute residuals from fitted ARMA model using 'infer' command
resid = infer(modelEstimate,dlog_stocks);
% Test residuals for serial correlation
[h,pValue] = lbqtest(resid,'lags',10)
%P-value=.14 fails to reject the null, which means we do not have serial correlation
%in the model
%Visual look at residuals
figure
plot(timeVec1,resid)
xlim([timeVec1(1) timeVec1(end)])
%Question 4
StockFit = dlog_stocks - resid
figure
plot(timeVec1,[dlog_stocks,StockFit])
legend('Fitted values','Stock Rate of Return (continuously compounded)')
xlim([timeVec1(1),timeVec1(end)])
xlabel('Time')
ylabel('Actual Data')
title('Fitted Values Against Actual Values')

%Part Two
clear all
close all
clc

%Question 1
data = xlsread('Keeling_CO2data_2017_wv.xlsx',1);
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

Linear_Model = LinearModel.fit(timeVec1,CO2_2005)
figure
plot(timeVec1,CO2_2005)
xlim([timeVec1(1),timeVec1(end)])
xlabel('Time')
ylabel('CO2')
%Would be good to graph the linear model prediction on the original data
%through 2016- keep looking to do this
%Clearly, the linear model has a high R2 and appears to follow the trend,
%but does not predict any of the seasonal variation

%Question 3
%First, test for 
