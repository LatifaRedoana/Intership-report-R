Predicting German electricity using Time Series Analysis
# Predicting German electricity using Time Series Analysis: SARIMA and Dynamic Harmonic Regression

This repository contains the implementation of time series forecasting techniques for predicting electricity production, with a focus on trend and seasonality identification and forecasting accuracy. The objectives outlined below were achieved through the use of graphical analysis, SARIMA modeling, and dynamic harmonic regression with Fourier terms.

## Objectives

### 1. Graphical Representation of Data
- **Objective**: Visualize the selected features to identify trends and seasonality in the data.
- **Method**: Use graphical plots (e.g., line plots, seasonal subseries plots, and decomposition plots) to explore the underlying patterns of the electricity production time series.

### 2. For Monthly Series: Decomposition and SARIMA
- **Objective**: Remove trend and seasonal components of the monthly series using differencing techniques and select the best-fitted SARIMA model based on forecast accuracy.
- **Method**: 
  - Perform seasonal decomposition and apply differencing to remove trend and seasonality.
  - Fit various SARIMA models and evaluate using forecast accuracy measures such as RMSE or AIC.
  - Select the best model based on the lowest error.

### 3. For Weekly and Daily Series: Handling Complex Seasonality
- **Objective**: Identify and apply appropriate statistical techniques to handle the complex seasonality patterns in high-frequency weekly and daily series.


### 4. Dynamic Harmonic Regression Model with Fourier Regressors
- **Objective**: Capture complex seasonal patterns in the weekly and daily series using dynamic harmonic regression with Fourier terms.
- **Method**: 
  - Apply dynamic harmonic regression with Fourier regressors to model and capture complex seasonality.
  - Select the best model based on forecast accuracy measures like RMSE or MAPE.

## Conclusion
This repository demonstrates the application of statistical and machine learning techniques to forecast electricity production data. By identifying trends and seasonality, applying SARIMA and dynamic harmonic regression models, we can make accurate predictions for future electricity generation.
