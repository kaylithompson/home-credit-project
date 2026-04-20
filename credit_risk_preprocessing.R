# =============================================================================
# Credit Risk Data Preprocessing Functions
# =============================================================================
# 
# A comprehensive set of reusable functions for cleaning, transforming, and
# feature engineering credit risk application data. Designed to maintain
# consistency between training and test datasets.
#
# Author: Kayli Thompson
# Date: 2026-02-22
#
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)

# =============================================================================
# SECTION 1: DATA CLEANING FUNCTIONS
# =============================================================================

#' Fix DAYS_EMPLOYED Anomaly
#' 
#' The value 365243 in DAYS_EMPLOYED is a placeholder for missing/unemployed.
#' This function replaces it with NA and creates an indicator variable.
#'
#' @param df A data frame containing DAYS_EMPLOYED column
#' @return Data frame with cleaned DAYS_EMPLOYED and anomaly indicator
fix_days_employed_anomaly <- function(df) {
  df |>
    mutate(
      DAYS_EMPLOYED_ANOMALY = if_else(DAYS_EMPLOYED == 365243, 1L, 0L),
      DAYS_EMPLOYED = if_else(DAYS_EMPLOYED == 365243, NA_real_, DAYS_EMPLOYED)
    )
}


#' Compute Imputation Values from Training Data
#' 
#' Calculates median values for numeric columns that need imputation.
#' These values should be computed ONLY from training data and reused for test.
#'
#' @param df Training data frame
#' @param cols Character vector of column names to compute medians for
#' @return Named list of median values
compute_imputation_values <- function(df, cols = c("EXT_SOURCE_1", "EXT_SOURCE_2", 
                                                    "EXT_SOURCE_3", "DAYS_EMPLOYED")) {
  medians <- list()
  for (col in cols) {
    if (col %in% names(df)) {
      medians[[col]] <- median(df[[col]], na.rm = TRUE)
    }
  }
  medians
}


#' Impute Missing Values
#' 
#' Fills missing values using provided imputation values and creates
#' missing indicator variables (often predictive in credit risk).
#'
#' @param df Data frame to impute
#' @param imputation_values Named list of values to use for imputation
#' @return Data frame with imputed values and missing indicators
impute_missing_values <- function(df, imputation_values) {
  
  indicator_cols <- c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3", 
                      "AMT_REQ_CREDIT_BUREAU_YEAR", "AMT_ANNUITY",
                      "AMT_GOODS_PRICE", "OWN_CAR_AGE", "DAYS_EMPLOYED")
  
  for (col in indicator_cols) {
    if (col %in% names(df)) {
      indicator_name <- paste0(col, "_MISSING")
      df[[indicator_name]] <- if_else(is.na(df[[col]]), 1L, 0L)
    }
  }
  
  for (col in names(imputation_values)) {
    if (col %in% names(df)) {
      df[[col]] <- if_else(is.na(df[[col]]), imputation_values[[col]], df[[col]])
    }
  }
  
  df
}


#' Clean Application Data (Main Cleaning Function)
#' 
#' Applies all cleaning steps to application data.
#'
#' @param df Application data frame
#' @param imputation_values Pre-computed imputation values (NULL for training)
#' @param is_training Logical, TRUE if processing training data
#' @return List with cleaned data and imputation values
clean_application_data <- function(df, imputation_values = NULL, is_training = TRUE) {
  
  df <- fix_days_employed_anomaly(df)
  
  if (is_training || is.null(imputation_values)) {
    imputation_values <- compute_imputation_values(df)
  }
  
  df <- impute_missing_values(df, imputation_values)
  
  list(
    data = df,
    imputation_values = imputation_values
  )
}


# =============================================================================
# SECTION 2: FEATURE ENGINEERING FUNCTIONS
# =============================================================================

#' Create Demographic Features
#' 
#' Transforms demographic variables into more interpretable formats:
#' - Converts negative days to positive years
#' - Creates age groups and employment duration categories
#'
#' @param df Data frame with demographic columns
#' @return Data frame with new demographic features
create_demographic_features <- function(df) {
  df |>
    mutate(
      AGE_YEARS = abs(DAYS_BIRTH) / 365.25,
      EMPLOYED_YEARS = abs(DAYS_EMPLOYED) / 365.25,
      REGISTRATION_YEARS = abs(DAYS_REGISTRATION) / 365.25,
      ID_PUBLISH_YEARS = abs(DAYS_ID_PUBLISH) / 365.25,
      EMPLOYED_TO_AGE_RATIO = EMPLOYED_YEARS / AGE_YEARS,
      LAST_PHONE_CHANGE_YEARS = if ("DAYS_LAST_PHONE_CHANGE" %in% names(df)) {
        abs(DAYS_LAST_PHONE_CHANGE) / 365.25
      } else {
        NA_real_
      }
    )
}


#' Create Financial Ratios
#' 
#' Engineers financial ratio features commonly used in credit risk modeling:
#' - Debt ratios, Income ratios, Credit utilization metrics
#'
#' @param df Data frame with financial columns
#' @return Data frame with financial ratio features
create_financial_ratios <- function(df) {
  df |>
    mutate(
      # Core Credit Ratios
      CREDIT_TO_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
      ANNUITY_TO_INCOME_RATIO = AMT_ANNUITY / AMT_INCOME_TOTAL,
      LOAN_TO_VALUE_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
      
      # Income and Expense Ratios
      INCOME_PER_FAMILY_MEMBER = AMT_INCOME_TOTAL / (CNT_FAM_MEMBERS + 1),
      INCOME_PER_CHILD = if_else(CNT_CHILDREN > 0, AMT_INCOME_TOTAL / CNT_CHILDREN, AMT_INCOME_TOTAL),
      CREDIT_PER_FAMILY_MEMBER = AMT_CREDIT / (CNT_FAM_MEMBERS + 1),
      
      # Loan Structure Ratios
      PAYMENT_PERIOD_MONTHS = AMT_CREDIT / AMT_ANNUITY,
      GOODS_TO_INCOME_RATIO = AMT_GOODS_PRICE / AMT_INCOME_TOTAL,
      DOWN_PAYMENT_RATIO = (AMT_GOODS_PRICE - AMT_CREDIT) / AMT_GOODS_PRICE,
      
      # External Score Combinations
      EXT_SOURCE_MEAN = (EXT_SOURCE_1 + EXT_SOURCE_2 + EXT_SOURCE_3) / 3,
      EXT_SOURCE_WEIGHTED = (EXT_SOURCE_1 * 0.2 + EXT_SOURCE_2 * 0.5 + EXT_SOURCE_3 * 0.3),
      EXT_SOURCE_PRODUCT = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
      EXT_SOURCE_MIN = pmin(EXT_SOURCE_1, EXT_SOURCE_2, EXT_SOURCE_3, na.rm = TRUE),
      EXT_SOURCE_MAX = pmax(EXT_SOURCE_1, EXT_SOURCE_2, EXT_SOURCE_3, na.rm = TRUE),
      EXT_SOURCE_RANGE = EXT_SOURCE_MAX - EXT_SOURCE_MIN,
      
      # Employment and Stability Ratios
      INCOME_PER_EMPLOYED_YEAR = AMT_INCOME_TOTAL / (EMPLOYED_YEARS + 1),
      CREDIT_PER_EMPLOYED_YEAR = AMT_CREDIT / (EMPLOYED_YEARS + 1),
      
      # Document Count
      TOTAL_DOCUMENTS = rowSums(across(starts_with("FLAG_DOCUMENT_")), na.rm = TRUE),
      
      # Social Circle Defaults
      SOCIAL_CIRCLE_DEF_30 = DEF_30_CNT_SOCIAL_CIRCLE,
      SOCIAL_CIRCLE_DEF_60 = DEF_60_CNT_SOCIAL_CIRCLE,
      
      # Bureau Inquiry Ratios
      BUREAU_INQUIRY_RATIO = if_else(
        AMT_REQ_CREDIT_BUREAU_YEAR > 0,
        (AMT_REQ_CREDIT_BUREAU_QRT + AMT_REQ_CREDIT_BUREAU_MON) / AMT_REQ_CREDIT_BUREAU_YEAR,
        0
      ),
      TOTAL_BUREAU_INQUIRIES = AMT_REQ_CREDIT_BUREAU_HOUR + AMT_REQ_CREDIT_BUREAU_DAY +
        AMT_REQ_CREDIT_BUREAU_WEEK + AMT_REQ_CREDIT_BUREAU_MON +
        AMT_REQ_CREDIT_BUREAU_QRT + AMT_REQ_CREDIT_BUREAU_YEAR
    )
}


#' Compute Binning Thresholds from Training Data
#' 
#' Calculates quantile-based thresholds for binning continuous variables.
#'
#' @param df Training data frame
#' @return List of threshold values for each binned variable
compute_binning_thresholds <- function(df) {
  list(
    AGE_YEARS = quantile(df$AGE_YEARS, probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
    CREDIT_TO_INCOME_RATIO = quantile(df$CREDIT_TO_INCOME_RATIO, probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
    EXT_SOURCE_MEAN = quantile(df$EXT_SOURCE_MEAN, probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
    ANNUITY_TO_INCOME_RATIO = quantile(df$ANNUITY_TO_INCOME_RATIO, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )
}


#' Create Binned Variables
#' 
#' Creates categorical bins for continuous variables using provided thresholds.
#'
#' @param df Data frame with continuous variables
#' @param thresholds List of threshold values
#' @return Data frame with binned variables added
create_binned_variables <- function(df, thresholds) {
  df |>
    mutate(
      AGE_BIN = case_when(
        AGE_YEARS <= thresholds$AGE_YEARS[1] ~ "Young",
        AGE_YEARS <= thresholds$AGE_YEARS[2] ~ "Middle_Young",
        AGE_YEARS <= thresholds$AGE_YEARS[3] ~ "Middle_Old",
        TRUE ~ "Senior"
      ),
      CREDIT_INCOME_BIN = case_when(
        CREDIT_TO_INCOME_RATIO <= thresholds$CREDIT_TO_INCOME_RATIO[1] ~ "Low",
        CREDIT_TO_INCOME_RATIO <= thresholds$CREDIT_TO_INCOME_RATIO[2] ~ "Medium_Low",
        CREDIT_TO_INCOME_RATIO <= thresholds$CREDIT_TO_INCOME_RATIO[3] ~ "Medium_High",
        TRUE ~ "High"
      ),
      EXT_SCORE_BIN = case_when(
        EXT_SOURCE_MEAN <= thresholds$EXT_SOURCE_MEAN[1] ~ "Poor",
        EXT_SOURCE_MEAN <= thresholds$EXT_SOURCE_MEAN[2] ~ "Fair",
        EXT_SOURCE_MEAN <= thresholds$EXT_SOURCE_MEAN[3] ~ "Good",
        TRUE ~ "Excellent"
      ),
      DEBT_SERVICE_BIN = case_when(
        ANNUITY_TO_INCOME_RATIO <= thresholds$ANNUITY_TO_INCOME_RATIO[1] ~ "Low",
        ANNUITY_TO_INCOME_RATIO <= thresholds$ANNUITY_TO_INCOME_RATIO[2] ~ "Medium",
        ANNUITY_TO_INCOME_RATIO <= thresholds$ANNUITY_TO_INCOME_RATIO[3] ~ "High",
        TRUE ~ "Very_High"
      )
    )
}


#' Engineer Application Features (Main Feature Engineering Function)
#' 
#' Applies all feature engineering steps to application data.
#'
#' @param df Cleaned application data frame
#' @param binning_thresholds Pre-computed thresholds (NULL for training)
#' @param is_training Logical, TRUE if processing training data
#' @return List with engineered data and binning thresholds
engineer_application_features <- function(df, binning_thresholds = NULL, is_training = TRUE) {
  
  df <- create_demographic_features(df)
  df <- create_financial_ratios(df)
  
  if (is_training || is.null(binning_thresholds)) {
    binning_thresholds <- compute_binning_thresholds(df)
  }
  
  df <- create_binned_variables(df, binning_thresholds)
  
  list(
    data = df,
    binning_thresholds = binning_thresholds
  )
}


# =============================================================================
# SECTION 3: SUPPLEMENTARY DATA AGGREGATION FUNCTIONS
# =============================================================================

#' Aggregate Bureau Data
#' 
#' Aggregates bureau.csv data to the applicant level (SK_ID_CURR).
#' Creates features for prior credit history, active accounts, and debt.
#'
#' @param bureau_path Path to bureau.csv or bureau.csv.zip
#' @return Data frame with bureau features aggregated by SK_ID_CURR
aggregate_bureau_data <- function(bureau_path) {
  
  message("Loading and aggregating bureau data...")
  
  bureau <- read_csv(bureau_path, show_col_types = FALSE)
  
  bureau_agg <- bureau |>
    group_by(SK_ID_CURR) |>
    summarize(
      BUREAU_CREDIT_COUNT = n(),
      BUREAU_ACTIVE_COUNT = sum(CREDIT_ACTIVE == "Active", na.rm = TRUE),
      BUREAU_CLOSED_COUNT = sum(CREDIT_ACTIVE == "Closed", na.rm = TRUE),
      BUREAU_ACTIVE_RATIO = BUREAU_ACTIVE_COUNT / BUREAU_CREDIT_COUNT,
      BUREAU_CREDIT_TYPES = n_distinct(CREDIT_TYPE),
      BUREAU_AMT_OVERDUE_SUM = sum(AMT_CREDIT_SUM_OVERDUE, na.rm = TRUE),
      BUREAU_AMT_OVERDUE_MEAN = mean(AMT_CREDIT_SUM_OVERDUE, na.rm = TRUE),
      BUREAU_HAS_OVERDUE = as.integer(any(AMT_CREDIT_SUM_OVERDUE > 0, na.rm = TRUE)),
      BUREAU_DEBT_SUM = sum(AMT_CREDIT_SUM_DEBT, na.rm = TRUE),
      BUREAU_DEBT_MEAN = mean(AMT_CREDIT_SUM_DEBT, na.rm = TRUE),
      BUREAU_CREDIT_SUM = sum(AMT_CREDIT_SUM, na.rm = TRUE),
      BUREAU_DEBT_RATIO = if_else(BUREAU_CREDIT_SUM > 0, BUREAU_DEBT_SUM / BUREAU_CREDIT_SUM, 0),
      BUREAU_DAYS_CREDIT_MIN = min(DAYS_CREDIT, na.rm = TRUE),
      BUREAU_DAYS_CREDIT_MEAN = mean(DAYS_CREDIT, na.rm = TRUE),
      BUREAU_CREDIT_DURATION_MEAN = mean(CREDIT_DAY_OVERDUE, na.rm = TRUE),
      BUREAU_PROLONGATION_SUM = sum(CNT_CREDIT_PROLONG, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(across(where(is.numeric), ~if_else(is.infinite(.), NA_real_, .)))
  
  message(paste("Bureau aggregation complete:", nrow(bureau_agg), "applicants"))
  
  bureau_agg
}


#' Aggregate Previous Applications Data
#' 
#' Aggregates previous_application.csv to the applicant level.
#' Creates features for application history, approval rates, and refusals.
#'
#' @param prev_app_path Path to previous_application.csv or .zip
#' @return Data frame with previous application features by SK_ID_CURR
aggregate_previous_applications <- function(prev_app_path) {
  
  message("Loading and aggregating previous applications...")
  
  prev_app <- read_csv(prev_app_path, show_col_types = FALSE)
  
  prev_app_agg <- prev_app |>
    group_by(SK_ID_CURR) |>
    summarize(
      PREV_APP_COUNT = n(),
      PREV_APPROVED_COUNT = sum(NAME_CONTRACT_STATUS == "Approved", na.rm = TRUE),
      PREV_REFUSED_COUNT = sum(NAME_CONTRACT_STATUS == "Refused", na.rm = TRUE),
      PREV_CANCELED_COUNT = sum(NAME_CONTRACT_STATUS == "Canceled", na.rm = TRUE),
      PREV_UNUSED_COUNT = sum(NAME_CONTRACT_STATUS == "Unused offer", na.rm = TRUE),
      PREV_APPROVAL_RATE = PREV_APPROVED_COUNT / PREV_APP_COUNT,
      PREV_REFUSAL_RATE = PREV_REFUSED_COUNT / PREV_APP_COUNT,
      PREV_HAS_REFUSAL = as.integer(PREV_REFUSED_COUNT > 0),
      PREV_AMT_CREDIT_SUM = sum(AMT_CREDIT, na.rm = TRUE),
      PREV_AMT_CREDIT_MEAN = mean(AMT_CREDIT, na.rm = TRUE),
      PREV_AMT_CREDIT_MAX = max(AMT_CREDIT, na.rm = TRUE),
      PREV_AMT_APPLICATION_MEAN = mean(AMT_APPLICATION, na.rm = TRUE),
      PREV_CREDIT_TO_APP_RATIO = mean(AMT_CREDIT / AMT_APPLICATION, na.rm = TRUE),
      PREV_DOWN_PAYMENT_MEAN = mean(AMT_DOWN_PAYMENT, na.rm = TRUE),
      PREV_DOWN_PAYMENT_RATIO = mean(AMT_DOWN_PAYMENT / AMT_CREDIT, na.rm = TRUE),
      PREV_CONTRACT_TYPES = n_distinct(NAME_CONTRACT_TYPE),
      PREV_CASH_COUNT = sum(NAME_CONTRACT_TYPE == "Cash loans", na.rm = TRUE),
      PREV_REVOLVING_COUNT = sum(NAME_CONTRACT_TYPE == "Revolving loans", na.rm = TRUE),
      PREV_DAYS_DECISION_MIN = min(DAYS_DECISION, na.rm = TRUE),
      PREV_DAYS_DECISION_MEAN = mean(DAYS_DECISION, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(across(where(is.numeric), ~if_else(is.infinite(.), NA_real_, .)))
  
  message(paste("Previous applications aggregation complete:", nrow(prev_app_agg), "applicants"))
  
  prev_app_agg
}


#' Aggregate Installments Payments Data
#' 
#' Aggregates installments_payments.csv to the applicant level.
#' Creates features for payment behavior, late payments, and trends.
#'
#' @param installments_path Path to installments_payments.csv or .zip
#' @return Data frame with installment payment features by SK_ID_CURR
aggregate_installments_payments <- function(installments_path) {
  
  message("Loading and aggregating installments payments...")
  
  installments <- read_csv(installments_path, show_col_types = FALSE)
  
  installments <- installments |>
    mutate(
      DAYS_DIFF = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
      IS_LATE = as.integer(DAYS_DIFF > 0),
      DAYS_LATE = pmax(DAYS_DIFF, 0),
      PAYMENT_DIFF = AMT_PAYMENT - AMT_INSTALMENT,
      IS_UNDERPAID = as.integer(PAYMENT_DIFF < 0),
      PAYMENT_RATIO = AMT_PAYMENT / AMT_INSTALMENT
    )
  
  installments_agg <- installments |>
    group_by(SK_ID_CURR) |>
    summarize(
      INSTAL_PAYMENT_COUNT = n(),
      INSTAL_VERSIONS = n_distinct(NUM_INSTALMENT_VERSION),
      INSTAL_LATE_COUNT = sum(IS_LATE, na.rm = TRUE),
      INSTAL_LATE_RATIO = INSTAL_LATE_COUNT / INSTAL_PAYMENT_COUNT,
      INSTAL_DAYS_LATE_SUM = sum(DAYS_LATE, na.rm = TRUE),
      INSTAL_DAYS_LATE_MEAN = mean(DAYS_LATE, na.rm = TRUE),
      INSTAL_DAYS_LATE_MAX = max(DAYS_LATE, na.rm = TRUE),
      INSTAL_EARLY_COUNT = sum(DAYS_DIFF < 0, na.rm = TRUE),
      INSTAL_EARLY_RATIO = INSTAL_EARLY_COUNT / INSTAL_PAYMENT_COUNT,
      INSTAL_UNDERPAID_COUNT = sum(IS_UNDERPAID, na.rm = TRUE),
      INSTAL_UNDERPAID_RATIO = INSTAL_UNDERPAID_COUNT / INSTAL_PAYMENT_COUNT,
      INSTAL_PAYMENT_RATIO_MEAN = mean(PAYMENT_RATIO, na.rm = TRUE),
      INSTAL_PAYMENT_RATIO_MIN = min(PAYMENT_RATIO, na.rm = TRUE),
      INSTAL_PAYMENT_RATIO_STD = sd(PAYMENT_RATIO, na.rm = TRUE),
      INSTAL_AMT_PAYMENT_SUM = sum(AMT_PAYMENT, na.rm = TRUE),
      INSTAL_AMT_INSTALMENT_SUM = sum(AMT_INSTALMENT, na.rm = TRUE),
      INSTAL_TOTAL_PAYMENT_RATIO = INSTAL_AMT_PAYMENT_SUM / INSTAL_AMT_INSTALMENT_SUM,
      INSTAL_RECENT_LATE_RATIO = mean(IS_LATE[NUM_INSTALMENT_NUMBER >= quantile(NUM_INSTALMENT_NUMBER, 0.75)], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(across(where(is.numeric), ~if_else(is.infinite(.) | is.nan(.), NA_real_, .)))
  
  message(paste("Installments aggregation complete:", nrow(installments_agg), "applicants"))
  
  installments_agg
}


# =============================================================================
# SECTION 4: DATA JOINING FUNCTIONS
# =============================================================================

#' Join Aggregated Features to Application Data
#' 
#' Left joins all aggregated supplementary data to the main application data.
#' Fills NA values for applicants without supplementary records.
#'
#' @param app_data Main application data frame
#' @param bureau_agg Aggregated bureau data (or NULL to skip)
#' @param prev_app_agg Aggregated previous applications (or NULL to skip)
#' @param installments_agg Aggregated installments (or NULL to skip)
#' @return Application data with all supplementary features joined
join_aggregated_features <- function(app_data, 
                                      bureau_agg = NULL, 
                                      prev_app_agg = NULL, 
                                      installments_agg = NULL) {
  
  message("Joining aggregated features to application data...")
  
  if (!is.null(bureau_agg)) {
    app_data <- app_data |>
      left_join(bureau_agg, by = "SK_ID_CURR")
    message("  - Bureau features joined")
  }
  
  if (!is.null(prev_app_agg)) {
    app_data <- app_data |>
      left_join(prev_app_agg, by = "SK_ID_CURR")
    message("  - Previous application features joined")
  }
  
  if (!is.null(installments_agg)) {
    app_data <- app_data |>
      left_join(installments_agg, by = "SK_ID_CURR")
    message("  - Installments features joined")
  }
  
  # Create indicators for missing supplementary data
  app_data <- app_data |>
    mutate(
      HAS_BUREAU_HISTORY = as.integer(!is.na(BUREAU_CREDIT_COUNT)),
      HAS_PREV_APP_HISTORY = as.integer(!is.na(PREV_APP_COUNT)),
      HAS_INSTALLMENT_HISTORY = as.integer(!is.na(INSTAL_PAYMENT_COUNT))
    )
  
  message("Feature joining complete")
  
  app_data
}


# =============================================================================
# SECTION 5: TRAIN/TEST CONSISTENCY FUNCTIONS
# =============================================================================

#' Save Preprocessing Parameters
#' 
#' Saves all computed parameters (medians, thresholds) to a file for reuse.
#'
#' @param params List containing imputation_values and binning_thresholds
#' @param filepath Path to save the RDS file
save_preprocessing_params <- function(params, filepath = "preprocessing_params.rds") {
  saveRDS(params, filepath)
  message(paste("Preprocessing parameters saved to:", filepath))
}


#' Load Preprocessing Parameters
#' 
#' Loads previously saved preprocessing parameters.
#'
#' @param filepath Path to the RDS file
#' @return List of preprocessing parameters
load_preprocessing_params <- function(filepath = "preprocessing_params.rds") {
  if (!file.exists(filepath)) {
    stop(paste("Parameter file not found:", filepath))
  }
  params <- readRDS(filepath)
  message(paste("Preprocessing parameters loaded from:", filepath))
  params
}


#' Ensure Column Consistency Between Train and Test
#' 
#' Ensures test data has identical columns to training data (except TARGET).
#' Adds missing columns with NA, removes extra columns.
#'
#' @param test_data Test data frame
#' @param train_columns Character vector of training column names
#' @return Test data with consistent columns
ensure_column_consistency <- function(test_data, train_columns) {
  
  expected_cols <- setdiff(train_columns, "TARGET")
  
  test_cols <- names(test_data)
  missing_cols <- setdiff(expected_cols, test_cols)
  extra_cols <- setdiff(test_cols, c(expected_cols, "SK_ID_CURR"))
  
  if (length(missing_cols) > 0) {
    message(paste("Adding", length(missing_cols), "missing columns to test data"))
    for (col in missing_cols) {
      test_data[[col]] <- NA
    }
  }
  
  if (length(extra_cols) > 0) {
    message(paste("Removing", length(extra_cols), "extra columns from test data"))
    test_data <- test_data |>
      select(-all_of(extra_cols))
  }
  
  final_cols <- intersect(train_columns, names(test_data))
  test_data <- test_data |>
    select(all_of(final_cols))
  
  test_data
}


# =============================================================================
# SECTION 6: MAIN PROCESSING PIPELINES
# =============================================================================

#' Process Training Data (Complete Pipeline)
#' 
#' Runs the complete preprocessing pipeline on training data.
#' Returns processed data and all parameters needed for test processing.
#'
#' @param app_train_path Path to application_train.csv
#' @param bureau_path Path to bureau.csv (optional)
#' @param prev_app_path Path to previous_application.csv (optional)
#' @param installments_path Path to installments_payments.csv (optional)
#' @param save_params_path Path to save parameters (NULL to skip saving)
#' @return List with processed data, parameters, and column names
process_training_data <- function(app_train_path,
                                   bureau_path = NULL,
                                   prev_app_path = NULL,
                                   installments_path = NULL,
                                   save_params_path = "preprocessing_params.rds") {
  
  message(strrep("=", 60))
  message("PROCESSING TRAINING DATA")
  message(strrep("=", 60))
  
  message("\nLoading application training data...")
  app_train <- read_csv(app_train_path, show_col_types = FALSE)
  message(paste("Loaded", nrow(app_train), "rows,", ncol(app_train), "columns"))
  
  # Step 1: Clean data
  message("\n--- Step 1: Cleaning Data ---")
  clean_result <- clean_application_data(app_train, is_training = TRUE)
  app_train <- clean_result$data
  imputation_values <- clean_result$imputation_values
  
  # Step 2: Engineer features
  message("\n--- Step 2: Engineering Features ---")
  feature_result <- engineer_application_features(app_train, is_training = TRUE)
  app_train <- feature_result$data
  binning_thresholds <- feature_result$binning_thresholds
  
  # Step 3: Aggregate supplementary data
  message("\n--- Step 3: Aggregating Supplementary Data ---")
  
  bureau_agg <- NULL
  prev_app_agg <- NULL
  installments_agg <- NULL
  
  if (!is.null(bureau_path) && file.exists(bureau_path)) {
    bureau_agg <- aggregate_bureau_data(bureau_path)
  }
  
  if (!is.null(prev_app_path) && file.exists(prev_app_path)) {
    prev_app_agg <- aggregate_previous_applications(prev_app_path)
  }
  
  if (!is.null(installments_path) && file.exists(installments_path)) {
    installments_agg <- aggregate_installments_payments(installments_path)
  }
  
  # Step 4: Join features
  message("\n--- Step 4: Joining Features ---")
  app_train <- join_aggregated_features(
    app_train, 
    bureau_agg, 
    prev_app_agg, 
    installments_agg
  )
  
  train_columns <- names(app_train)
  
  preprocessing_params <- list(
    imputation_values = imputation_values,
    binning_thresholds = binning_thresholds,
    train_columns = train_columns
  )
  
  if (!is.null(save_params_path)) {
    save_preprocessing_params(preprocessing_params, save_params_path)
  }
  
  message("\n", strrep("=", 60))
  message("TRAINING DATA PROCESSING COMPLETE")
  message(paste("Final dimensions:", nrow(app_train), "rows,", ncol(app_train), "columns"))
  message(strrep("=", 60))
  
  list(
    data = app_train,
    params = preprocessing_params,
    bureau_agg = bureau_agg,
    prev_app_agg = prev_app_agg,
    installments_agg = installments_agg
  )
}


#' Process Test Data (Complete Pipeline)
#' 
#' Runs the preprocessing pipeline on test data using training parameters.
#' Ensures consistency with training data columns.
#'
#' @param app_test_path Path to application_test.csv
#' @param train_result Result from process_training_data() OR path to saved params
#' @param bureau_path Path to bureau.csv (optional, uses pre-aggregated if available)
#' @param prev_app_path Path to previous_application.csv (optional)
#' @param installments_path Path to installments_payments.csv (optional)
#' @return Processed test data frame
process_test_data <- function(app_test_path,
                               train_result = NULL,
                               params_path = "preprocessing_params.rds",
                               bureau_path = NULL,
                               prev_app_path = NULL,
                               installments_path = NULL) {
  
  message(strrep("=", 60))
  message("PROCESSING TEST DATA")
  message(strrep("=", 60))
  
  # Get parameters from train_result or load from file
  if (!is.null(train_result) && is.list(train_result)) {
    preprocessing_params <- train_result$params
    bureau_agg <- train_result$bureau_agg
    prev_app_agg <- train_result$prev_app_agg
    installments_agg <- train_result$installments_agg
  } else {
    preprocessing_params <- load_preprocessing_params(params_path)
    bureau_agg <- NULL
    prev_app_agg <- NULL
    installments_agg <- NULL
  }
  
  message("\nLoading application test data...")
  app_test <- read_csv(app_test_path, show_col_types = FALSE)
  message(paste("Loaded", nrow(app_test), "rows,", ncol(app_test), "columns"))
  
  # Step 1: Clean data using training parameters
  message("\n--- Step 1: Cleaning Data ---")
  clean_result <- clean_application_data(
    app_test, 
    imputation_values = preprocessing_params$imputation_values,
    is_training = FALSE
  )
  app_test <- clean_result$data
  
  # Step 2: Engineer features using training thresholds
  message("\n--- Step 2: Engineering Features ---")
  feature_result <- engineer_application_features(
    app_test,
    binning_thresholds = preprocessing_params$binning_thresholds,
    is_training = FALSE
  )
  app_test <- feature_result$data
  
  # Step 3: Aggregate supplementary data if not provided
  message("\n--- Step 3: Aggregating Supplementary Data ---")
  
  if (is.null(bureau_agg) && !is.null(bureau_path) && file.exists(bureau_path)) {
    bureau_agg <- aggregate_bureau_data(bureau_path)
  }
  
  if (is.null(prev_app_agg) && !is.null(prev_app_path) && file.exists(prev_app_path)) {
    prev_app_agg <- aggregate_previous_applications(prev_app_path)
  }
  
  if (is.null(installments_agg) && !is.null(installments_path) && file.exists(installments_path)) {
    installments_agg <- aggregate_installments_payments(installments_path)
  }
  
  # Step 4: Join features
  message("\n--- Step 4: Joining Features ---")
  app_test <- join_aggregated_features(
    app_test,
    bureau_agg,
    prev_app_agg,
    installments_agg
  )
  
  # Step 5: Ensure column consistency
  message("\n--- Step 5: Ensuring Column Consistency ---")
  app_test <- ensure_column_consistency(
    app_test,
    preprocessing_params$train_columns
  )
  
  message("\n", strrep("=", 60))
  message("TEST DATA PROCESSING COMPLETE")
  message(paste("Final dimensions:", nrow(app_test), "rows,", ncol(app_test), "columns"))
  message(strrep("=", 60))
  
  app_test
}

