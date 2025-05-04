
## ♻️ Predicting German electricity using Time Series Analysis: SARIMA and Dynamic Harmonic Regression

This repository contains the implementation of time series forecasting techniques for predicting German electricity production, with a focus on **three renewable energy sources - Solar, Windonshore, Windoffshore**.  The objectives outlined below were achieved through the use of graphical analysis, SARIMA modeling, and dynamic harmonic regression with Fourier terms.

📘 **Author:** Latifa Redoana  
🎓 **University:** TU Dortmund, Germany  
🏢 **Company:** REMONDIS SmartRec GmbH

## 📊 Data Description

The dataset was provided by **REMONDIS Energy & Services GmbH & Co. KG**, Lünen, Germany and represents electricity production at various time granularities. A comparable public dataset is available from the **ENTSO-E Transparency Platform**, which provides comprehensive electricity system data for most European countries.
🔗 [ENTSO-E Transparency Platform](https://transparency.entsoe.eu/)

---

## 🎯 Project Objectives
trend and seasonality identification and forecasting accuracy
### 📈 Visual Exploration:
- Graphically represent selected features to identify **trend** and **seasonality** in the time series.
- ### 🗓️ Monthly Series:
- Apply **differencing** to remove trend and seasonal components.
- Fit and evaluate **SARIMA models**.
- Select the best-performing model based on forecast accuracy.

### 📅 Weekly and Daily Series:
- Use techniques such as **STL decomposition** and **TBATS models** to handle complex and high-frequency seasonal patterns.

### 🎵 Dynamic Harmonic Regression:
- Use **dynamic harmonic regression with Fourier regressors** to capture and forecast complex seasonality in high-frequency series.

### ✅ Model Evaluation:
- Evaluate forecasting models using accuracy metrics such as:
  - **RMSE** (Root Mean Square Error)
  - **AIC** (Akaike Information Criterion)
  - **MAPE** (Mean Absolute Percentage Error)

---

## 🧰 Tools and Libraries (in R)

This project utilizes the statistical programming language **R**, along with the following key packages:

- `forecast` – Classical time series models (ARIMA, ETS)
- `fpp2` / `fpp3` – Structured forecasting framework
- `tseries` – Time series decomposition and tests
- `BBmisc` – Utility tools for metrics and tuning
- `tidyverse` – Data wrangling and visualization
- `tsibble` – Tidy time series format
- `lubridate` – Date and time handling
- `prophet` *(optional)* – Additive models with multiple seasonalities


---
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


🧠 Tools & Libraries
- Python (Pandas, NumPy, statsmodels, TensorFlow/Keras, matplotlib)
- Jupyter Notebooks
- Darts (for frequency spectrum)

---
## Conclusion
This repository demonstrates the application of statistical and machine learning techniques to forecast electricity production data. By identifying trends and seasonality, applying SARIMA and dynamic harmonic regression models, we can make accurate predictions for future electricity generation.
