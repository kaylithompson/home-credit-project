# home-credit-project

Kayli Thompson

This is the Home Credit Default Risk Kaggle project. The goal of this project is to predict the probability of a client defaulting on a loan using various features provided in the dataset.

---

## Credit Risk Preprocessing Functions

### What This Script Does

The `credit_risk_preprocessing.R` script provides a complete preprocessing pipeline for credit risk modeling, designed to work with Home Credit default risk data. It transforms raw application data into analysis-ready features while ensuring consistency between training and test datasets.

#### Data Cleaning
- **DAYS_EMPLOYED anomaly fix**: Replaces placeholder value (365243) with the training median and creates an indicator flag (`DAYS_EMPLOYED_ANOMALY`)
- **EXT_SOURCE imputation**: Fills missing values in `EXT_SOURCE_1`, `EXT_SOURCE_2`, and `EXT_SOURCE_3` with training medians
- **Missing value indicators**: Creates binary flags for all imputed variables to preserve missingness information

#### Feature Engineering

| Category | Features Created |
|----------|------------------|
| **Demographic** | `AGE_YEARS`, `EMPLOYED_YEARS`, `EMPLOYED_TO_AGE_RATIO` |
| **Financial Ratios** | `CREDIT_TO_INCOME_RATIO`, `ANNUITY_TO_INCOME_RATIO`, `LOAN_TO_VALUE_RATIO`, `DOWN_PAYMENT_RATIO`, `PAYMENT_PERIOD_MONTHS`, `GOODS_TO_INCOME_RATIO`, `INCOME_PER_FAMILY_MEMBER`, `CREDIT_PER_FAMILY_MEMBER`, `INCOME_PER_EMPLOYED_YEAR`, `CREDIT_PER_EMPLOYED_YEAR`, `BUREAU_INQUIRY_RATIO` |
| **External Score Combinations** | `EXT_SOURCE_MEAN`, `EXT_SOURCE_WEIGHTED`, `EXT_SOURCE_PRODUCT`, `EXT_SOURCE_MIN`, `EXT_SOURCE_MAX`, `EXT_SOURCE_RANGE` |
| **Binned Variables** | `AGE_BIN`, `CREDIT_RATIO_BIN`, `EXT_SOURCE_BIN`, `ANNUITY_RATIO_BIN` |

#### Supplementary Data Aggregation

| Source File | Features Created |
|-------------|------------------|
| **bureau.csv** | `BUREAU_CREDIT_COUNT`, `BUREAU_ACTIVE_COUNT`, `BUREAU_CLOSED_COUNT`, `BUREAU_ACTIVE_RATIO`, `BUREAU_AMT_CREDIT_SUM`, `BUREAU_AMT_DEBT_SUM`, `BUREAU_DEBT_RATIO`, `BUREAU_AMT_OVERDUE_SUM`, `BUREAU_AMT_OVERDUE_MEAN`, `BUREAU_HAS_OVERDUE`, `BUREAU_CREDIT_PROLONGED_SUM`, `BUREAU_DAYS_CREDIT_MEAN`, `BUREAU_DAYS_CREDIT_UPDATE_MEAN` |
| **previous_application.csv** | `PREV_APP_COUNT`, `PREV_APPROVED_COUNT`, `PREV_REFUSED_COUNT`, `PREV_CANCELED_COUNT`, `PREV_APPROVAL_RATE`, `PREV_REFUSAL_RATE`, `PREV_HAS_REFUSAL`, `PREV_AMT_APPLICATION_MEAN`, `PREV_AMT_CREDIT_MEAN`, `PREV_AMT_ANNUITY_MEAN`, `PREV_CREDIT_TO_APP_RATIO`, `PREV_DOWN_PAYMENT_MEAN`, `PREV_DAYS_DECISION_MEAN`, `PREV_DAYS_FIRST_DUE_MEAN`, `PREV_CNT_PAYMENT_MEAN`, `PREV_RATE_INTEREST_PRIMARY_MEAN`, `PREV_RATE_INTEREST_PRIV_MEAN`, `PREV_SELLERPLACE_AREA_MEAN` |
| **installments_payments.csv** | `INSTAL_COUNT`, `INSTAL_LATE_COUNT`, `INSTAL_LATE_RATIO`, `INSTAL_DAYS_LATE_MEAN`, `INSTAL_DAYS_LATE_MAX`, `INSTAL_PAYMENT_RATIO_MEAN`, `INSTAL_UNDERPAID_COUNT`, `INSTAL_UNDERPAID_RATIO`, `INSTAL_AMT_PAYMENT_SUM`, `INSTAL_AMT_INSTALMENT_SUM` |

#### Train/Test Consistency
- Computes imputation values and binning thresholds from **training data only**
- Saves parameters to `preprocessing_params.rds` for reuse on test data
- Ensures identical column structure between train and test (except `TARGET`)

---

### How to Run the Script

#### Prerequisites

```r
# Required packages
install.packages(c("tidyverse", "data.table"))
```

#### Basic Usage

```r
# 1. Source the functions
source("credit_risk_preprocessing.R")

# 2. Process training data (computes and saves all preprocessing parameters)
train_result <- process_training_data(
  app_train_path = "path/to/application_train.csv.zip",
  bureau_path = "path/to/bureau.csv.zip",
  prev_app_path = "path/to/previous_application.csv.zip",
  installments_path = "path/to/installments_payments.csv.zip"
)

# 3. Extract processed training data
train_data <- train_result$data

# 4. Process test data using saved parameters (ensures consistency)
test_data <- process_test_data(
  app_test_path = "path/to/application_test.csv.zip",
  train_result = train_result
)

# Alternative: Load parameters from file if train_result is not in memory
test_data <- process_test_data(
  app_test_path = "path/to/application_test.csv.zip",
  params_path = "preprocessing_params.rds",
  bureau_path = "path/to/bureau.csv.zip",
  prev_app_path = "path/to/previous_application.csv.zip",
  installments_path = "path/to/installments_payments.csv.zip"
)
```

---

### Inputs and Outputs

#### Required Input Files

| File | Description | Key Columns Used |
|------|-------------|------------------|
| `application_train.csv` | Main training dataset with target variable | `SK_ID_CURR`, `TARGET`, `DAYS_EMPLOYED`, `DAYS_BIRTH`, `EXT_SOURCE_1/2/3`, `AMT_CREDIT`, `AMT_INCOME_TOTAL`, `AMT_ANNUITY`, `AMT_GOODS_PRICE`, `CNT_FAM_MEMBERS` |
| `application_test.csv` | Test dataset for predictions | Same as train (excluding `TARGET`) |
| `bureau.csv` | Credit bureau history | `SK_ID_CURR`, `CREDIT_ACTIVE`, `AMT_CREDIT_SUM`, `AMT_CREDIT_SUM_DEBT`, `AMT_CREDIT_SUM_OVERDUE`, `CREDIT_DAY_OVERDUE` |
| `previous_application.csv` | Previous loan applications | `SK_ID_CURR`, `NAME_CONTRACT_STATUS`, `AMT_APPLICATION`, `AMT_CREDIT`, `AMT_ANNUITY`, `DAYS_DECISION` |
| `installments_payments.csv` | Payment history | `SK_ID_CURR`, `DAYS_INSTALMENT`, `DAYS_ENTRY_PAYMENT`, `AMT_INSTALMENT`, `AMT_PAYMENT` |

#### Outputs

| Output | Description |
|--------|-------------|
| `train_result$data` | Processed training dataframe with 220 columns (122 original + 98 engineered) |
| `train_result$params` | List containing imputation values, binning thresholds, and column names |
| `train_result$bureau_agg` | Bureau data aggregated to applicant level |
| `train_result$prev_app_agg` | Previous applications aggregated to applicant level |
| `train_result$installments_agg` | Installments data aggregated to applicant level |
| `preprocessing_params.rds` | Saved parameters file for processing test data |
| Processed test dataframe | Same structure as training data (excluding `TARGET`) |

---

### Function Reference

| Function | Purpose |
|----------|---------|
| `process_training_data()` | Main pipeline for training data |
| `process_test_data()` | Main pipeline for test data |
| `clean_application_data()` | Fixes anomalies and imputes missing values |
| `engineer_application_features()` | Creates demographic, financial, and binned features |
| `aggregate_bureau_data()` | Aggregates bureau.csv to applicant level |
| `aggregate_previous_applications()` | Aggregates previous_application.csv to applicant level |
| `aggregate_installments_payments()` | Aggregates installments_payments.csv to applicant level |
| `join_aggregated_features()` | Joins all aggregated features to application data |
| `ensure_column_consistency()` | Aligns test columns to match training data |
