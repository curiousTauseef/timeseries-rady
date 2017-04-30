% Assignment 1 starter code

% This file contains a partial solution to assignment 1 with examples of
% how to use all of the major MATLAB functions necessary for completion of
% the assignment

% Usually good practice to start MATLAB scripts with these commands
clear all % deletes all variables in memory
close all % closes all plot windows
clc % clears the command line


%% Part I. Constructing prediction models for different variables

% Read in data

data1 = xlsread('time_series_data_2017.xlsx',1);
data2 = xlsread('time_series_data_2017.xlsx',2);
data3 = xlsread('time_series_data_2017.xlsx',3);
data4 = xlsread('time_series_data_2017.xlsx',4);


% Second column of data is the unemployment rate
% NOTE: If for some reason you convert the Excel file to .xls format
% instead of .xlsx format, xlsread will throw away the first column because
% the dates are in a format it doesn't recognize. In this case this extra
% step isn't necessary

unrate = data1(:,2);


%% Question 1

% Plot raw data1
figure

% Create date vector
timeVec1 = (1948:1/12:2016+10/12)';

plot(timeVec1,unrate)
xlim([timeVec1(1) timeVec1(end)])
xlabel('Time')
ylabel('Unemployment rate')

%% Plot raw data2
figure 

consPrice = data2(:,2);
timeVec2 = (1947:1/12:2016+10/12)';
plot(timeVec2,consPrice)
xlim([timeVec2(1) timeVec2(end)])
xlabel('Time')
ylabel('Consumer Price Index')

%% Plot raw data3
figure 

goldPrice = data3(:,2);
timeVec = (1968+3/12:1/12:2016+11/12)';
plot(timeVec3,goldPrice)
xlim([timeVec3(1) timeVec3(end)])
xlabel('Time')
ylabel('Gold Price Index')

%% Augmented Dickey-Fuller test for non-stationarity
% A value of h = 0 indicates that we shouldn't reject the null hypothesis
% of non-stationarity. A value of h = 1 indicates we should reject the null
% hypohtesis. That is, h = 0 corresponds to a non-stationary series while
% h = 1 corresponds to a stationary series.
% Despite getting a value of h = 0 for the test, most economists DO NOT
% first difference the unemployment rate. However, for the purposes of this
% assignment either is fine
[h,pValue] = adftest(urate)

%% Question 2

% Plot the autocorrelation function for the first 10 lags
figure
autocorr(urate,10)
xlim([0 10])

% Display the first 10 autocorrelations
urateAutocorrs = autocorr(urate,10)

% Perform LBQ tests for significance of autocorrelations
disp('Unemployment rate serial correlation tests')
disp('1 lag')
[h,pValue] = lbqtest(urate,'lags',1)
disp('10 lags')
[h,pValue] = lbqtest(urate,'lags',10)

% Question 3

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
            [~,~,logL] = estimate(model,urate);
            % Compute corresponding AIC and BIC values
            urateModelCriteria(ind,:) = aicbic(logL,ii+jj,length(urate)-max(ii,jj));
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

% Question 4

% Estimate model chosen by AIC
model = arima(bestAICModelLags(1,1),0,bestAICModelLags(1,2));
modelEstimate = estimate(model,urate);

% Compute residuals from fitted ARMA model using 'infer' command
resid = infer(modelEstimate,urate);
% Test residuals for serial correlation
[h,pValue] = lbqtest(resid,'lags',10)
% Visually inspect fitted residuals
figure
plot(timeVec,resid)
xlim([timeVec(1) timeVec(end)])

% Construct fitted values from model residuals and original time series
urateFit = urate - resid;

% Plot time series and fitted values
figure
plot(timeVec,[urateFit urate])
legend('Fitted values','Data')
xlim([timeVec(1) timeVec(end)])