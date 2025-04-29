# IAG Sandbox Project: Claims Inflation Forecasting

## Overview
This project was developed as part of the IAG Sandbox initiative, focusing on forecasting future claims inflation for commercial motor vehicle insurance portfolios. The objective was to support IAG’s capital reserve optimisation and pricing strategies by building predictive models for claims frequency and severity.

## Project Motivation
Accurately predicting claims inflation is crucial for insurers to maintain financial stability, optimise reserves, and price premiums appropriately. By using both internal policyholder data and key external economic indicators, our solution provides actionable forecasts for future claims costs.

## Data Sources
- **Internal Variables**: Claim exposure, policy tenure, vehicle age, total sum insured, vehicle class, state, and calendar month of claim (ACTL31425110AssignmentData2022.csv).
- **External Variables**:
  - Wage growth data (Annual wage growth - 1998 to 2022.csv)
  - Mogas 95 petrol futures price (Mogas_95.csv)
  - Goods and services imports index (Import data.csv)
  - Vehicle manufacturing producer price indexes (ProducerIndex.csv, ProducerIndex1.csv)
  - Unemployment rate (Unemployment rate.csv)

All external variables were lagged appropriately (monthly or quarterly) to ensure real-world prediction capability.

## Methodology
- **Model Construction**: 
  - Two generalised linear models (GLMs) were developed separately for:
    - **Claim Frequency** (assumed Poisson distribution)
    - **Claim Severity** (assumed Gamma distribution)
  - Both monthly aggregate and per-policy approaches were trialled.

- **Feature Selection**: 
  - Forward and backward stepwise selection using Akaike Information Criterion (AIC).
  - Additional insight from gradient-boosted models to assess variable importance.

- **Validation**: 
  - K-fold cross-validation to address variance and potential distortions due to the COVID-19 pandemic.
  - Model performance evaluated via root mean squared error (RMSE) and quantile plots.

## Results
- The **monthly aggregate model** outperformed the per-policy model in terms of prediction accuracy.
- Internal variables had greater economic significance, but external variables contributed meaningful predictive power.
- Forecasts suggested a **61% increase in claims inflation** over the 2022 financial year, assuming policyholder composition remains stable.

## Key Findings
- Both internal and external predictors were statistically significant at the 5% level.
- The model outputs were consistent with actuarial literature and industry expectations.
- Recommended action: IAG should increase its reserves significantly to accommodate the expected inflationary growth.

## Future Improvements
- Expand the predictor set to include broader macroeconomic and sector-specific indicators.
- Improve data granularity and volume for increased model robustness.
- Explore advanced machine learning models (e.g., XGBoost, Tweedie models) for potential accuracy gains.

## My Contribution
I was responsible for developing the Monthly Claim Numbers Model, a key component of the project’s claims frequency forecasting.
Specifically, my contributions included:
- Performing exploratory data analysis (EDA) and initial modelling of internal and external datasets.
- Building a Poisson regression model to predict the number of claims per month.
- Producing a one-year forecast of monthly claim counts.
- Validating the model’s predictions against actual historical data.

Relevant files:
- Assignment Code.R: Contains EDA, feature selection, and initial modelling.
- Monthly Claim Numbers.R: Final model construction, forecasting, and outputs.

Datasets Used:
- Internal claims data: ACTL31425110AssignmentData2022.csv
- External indicators: Annual wage growth - 1998 to 2022.csv, Mogas_95.csv, Import data.csv, ProducerIndex.csv, ProducerIndex1.csv, Unemployment rate.csv

Outputs Generated:
- Claim Numbers.csv: Comparison of true vs. predicted monthly claim numbers.
- Yearly Prediction.csv: One-year forecast of future claims volume.
- cloned data.csv: Sample cloned policyholder data for future projections.

## Demo
📽️ **Video Walkthrough of the Final Solution:**  
[Insert your YouTube/Google Drive/Vimeo link here]

*(If you prefer, you can embed a thumbnail with a clickable link too! I can show you how.)*

## Installation
1. Clone the repository:
  ```bash
  git clone https://github.com/Manjot44/IAG-Sandbox-Project.git

2. Pull Git LFS files:
  ```bash
  cd IAG-Sandbox-Project
  git lfs pull

