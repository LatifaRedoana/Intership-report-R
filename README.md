
## ♻️ Predicting German electricity using Time Series Analysis: SARIMA and Dynamic Harmonic Regression model

This repository contains the implementation of time series forecasting techniques for predicting German electricity production, with a focus on **three renewable energy sources - Solar, Windonshore, Windoffshore**.  The objectives outlined below were achieved through the use of graphical analysis, SARIMA modeling, and dynamic harmonic regression with Fourier terms.

📘 **Author:** Latifa Redoana 
🎓 **University:** TU Dortmund, Germany  
🏢 **Company:** REMONDIS SmartRec GmbH

---

## 📊 Data Description

The dataset was provided by **REMONDIS Energy & Services GmbH & Co. KG**, Lünen, Germany and represents electricity production at various time granularities. A comparable public dataset is available from the **ENTSO-E Transparency Platform**, which provides comprehensive electricity system data for most European countries.
🔗 [ENTSO-E Transparency Platform](https://transparency.entsoe.eu/)

---

## 🎯 Overview of Project

### 📈 Visual Exploration:
- Graphically represent selected features to identify **trend** and **seasonality** in the time series. Use graphical plots (e.g., line plots, seasonal subseries plots, and decomposition plots) to explore the underlying patterns.
  
- ### 🗓️ Monthly Series:
- Apply **differencing** to remove trend and seasonal components.
- Fit and evaluate **SARIMA models**.
- Select the best-performing model based on forecast accuracy.
  
### 📅 Weekly and Daily Series:
Classical time series models such as **ARIMA** and **ETS** work well for lower-frequency seasonality, such as:

- Monthly data (seasonal period `m = 12`)
- Quarterly data (`m = 4`)

However, for **weekly data** with a seasonal period around `52.18`, these models face challenges:

- The `ets()` function in R is limited to `m ≤ 24`
- The `arima()` function can theoretically support `m ≈ 350`, but in practice, seasonality above `200` is often unstable or inefficient
### 🎵 Handling Complex Seasonality with Harmonic Regression:
- Use **dynamic harmonic regression with Fourier regressors** to capture and forecast complex seasonality in high-frequency series.
  
### 🔁 Fourier Terms

- The **Fourier series** uses sine and cosine terms to approximate complex seasonal patterns.
- The number of Fourier pairs (`K`) determines the smoothness of the seasonal curve:
  - Start with `K = 1` (simple seasonal pattern)
  - Increase `K` to allow more complex, “wiggly” seasonal shapes
---
### ✅ Model Evaluation:
- Evaluate forecasting models using accuracy metrics such as:
  - **RMSE** (Root Mean Square Error)
  - **AIC** (Akaike Information Criterion)
  - **MAPE** (Mean Absolute Percentage Error)

---

## 🧰 Tools and Libraries (in R)

This project utilizes the statistical programming language **R**, along with the following key packages:

- `forecast` – Classical time series models (ARIMA, ETS),`Fourier()` function
- `fpp2` / `fpp3` – Structured forecasting framework
- `tseries` – Time series decomposition and tests
- `BBmisc` – Utility tools for metrics and tuning
- `tidyverse` – Data wrangling and visualization
- `tsibble` – Tidy time series format
- `lubridate` – Date and time handling
- `prophet` *(optional)* – Additive models with multiple seasonalities

---
## Conclusion
This repository demonstrates the application of statistical and machine learning techniques to forecast electricity production data for daily, weekly, monthly and combine of renewable energy sources. By identifying trends and seasonality, applying SARIMA and dynamic harmonic regression models, we can make accurate predictions for future electricity generation.
