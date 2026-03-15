# E-Commerce Revenue Prediction Model

## Author

Druvitha H K
MSc Student – University of Leeds

## Project Overview

This project analyses customer behaviour and marketing factors to predict order-level revenue for an e-commerce fragrance retailer called SweetAroma.

Using transactional order data from July 2025, the project identifies key drivers of revenue and builds a predictive model that estimates the expected value of future customer orders.

The analysis follows the CRISP-DM data science methodology including business understanding, data preparation, modelling and evaluation.

## Technologies Used

* R Programming
* Linear Regression
* Data Cleaning
* Exploratory Data Analysis
* Predictive Modelling
* Business Analytics

## Dataset

The dataset contains 10,000 e-commerce orders with the following variables:

* revenue – order value in GBP
* ad_channel – marketing channel
* number_past_order – number of previous purchases
* past_spend – historical spending
* time_web – time spent on website
* voucher – discount usage

## Project Workflow

### 1. Data Understanding

Exploratory analysis was conducted to understand revenue distribution and customer behaviour patterns.

### 2. Data Preparation

* Converted categorical variables into factors
* Handled missing values using listwise deletion
* Cleaned dataset reduced to 9,419 complete observations

### 3. Exploratory Data Analysis

Visualisations were created to analyse:

* revenue distribution
* historical customer spending
* advertising channel distribution
* voucher usage patterns

### 4. Predictive Modelling

A multiple linear regression model was built to estimate order revenue using behavioural and marketing variables.

Model formula:

Revenue ~ Ad Channel + Past Orders + Past Spend + Website Time + Voucher

### 5. Model Evaluation

Model performance was evaluated using:

* R²
* Adjusted R²
* Root Mean Square Error (RMSE)
* Mean Absolute Error (MAE)

Residual analysis was also performed to validate model assumptions.

### 6. Revenue Prediction

The final model was applied to predict revenues for 20 new customer orders.

## Key Business Insights

* Customers with higher past spending tend to generate higher order revenue.
* Marketing channels influence order value.
* Website engagement time is associated with increased spending.
* Voucher usage can affect purchasing behaviour.

These insights can help SweetAroma target high-value customers and optimise marketing campaigns.

## Skills Demonstrated

* Business analytics
* Data preprocessing
* Statistical modelling
* Predictive analytics
* Data visualisation
* Customer behaviour analysis

## Future Improvements

* Test advanced machine learning models
* Deploy model as a business decision tool
* Integrate real-time marketing data

## License

MIT License
