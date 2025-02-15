---
title: "Churn Prediction Project Documentation"
author: "R4DataScienceNigeria Team"
format: 
  html:
    theme: cosmo
    toc: true
    toc-title: "Content"
    toc-depth: 2
---

## 1. Customer Retention Analysis

This documentation outlines the development of a robust Machine Learning model that accurately predicts customer churn for banks.

### 1.1 Introduction

In today's competitive financial sector, customer retention is crucial for banks. Increasing customer churn is impacting revenue and raising the cost of acquiring new customers. Despite various engagement strategies, banks lack a reliable system to predict and prevent customer attrition.

### 1.2 Aim and Objectives

The primary aim of this project is to develop a robust Machine Learning model that accurately predicts customer churn for banks. Leveraging historical customer data, including transaction patterns, product usage, account tenure, and demographic information, the model will identify key factors contributing to customer attrition.

-   **Aim**: To develop a predictive model that accurately identifies customers who are likely to churn.
-   **Objectives**:
    -   Collect and preprocess customer data.
    -   Explore and analyze the data to identify key features influencing churn.
    -   Build and evaluate multiple machine learning models.
    -   Deploy the best-performing model in a user-friendly dashboard.
-   Key Deliverable:
    -   Identify At-Risk Customers: Detect customers who are likely to churn in the near future with high accuracy.
    -   Understand Churn Drivers: Analyze the primary reasons for customer churn and provide actionable insights.
    -   Optimize Retention Strategies: Develop targeted retention strategies and personalized interventions to improve customer satisfaction and reduce churn rates.

## 2. Data Information

### 2.1 Data Source

-   **Source**: The data-set was obtained from a multinational bank with branches in Nigeria and across Africa. It was generated in 2023.
-   **Description**: The data-set contains 19 variables and 500,000 rows. It is part of a larger data-set of over 20 million records

### 2.2 Metadata

-   **Columns**:
    -   **`acct_id`**: A unique identifier for each customer account.

    -   **`years`**: The number of years a customer has been with the bank.

    -   **`churn`**: A binary indicator of whether the customer has churned (e.g., 0 for not churned, 1 for churned).

    -   **`risk_rating`**: A rating or score that reflects the financial risk associated with the customer.

    -   **`currency`**: The currency used in the customer’s account (e.g., NGN, USD, EUR).

    -   **`ave_bal`**: The average balance in the customer’s account over a specified period.

    -   **`scheme_type`**: The type of banking scheme or product the customer is using. Y = Yes; N = No.

    -   **`mobile*app*adoption`**: Indicates whether the customer uses the bank’s mobile app. Y = Yes; N = No.

    -   **`internet_banking_adoption`**: Indicates whether the customer uses internet banking. Y = Yes; N = No.

    -   **`ussd_banking_adoption`**: Indicates whether the customer uses USSD banking services. Y = Yes; N = No.

    -   **`digital_loan`**: Indicates whether the customer has taken a digital loan. Y = Yes; N = No.

    -   **`unsecured_loan`**: Indicates whether the customer has an unsecured loan. Y = Yes; N = No.

    -   **`termloan_status`**: Status of the customer’s term loan. Y = Yes; N = No.

    -   **`credit_card`**: Indicates whether the customer holds a credit card with the bank. Y = Yes; N = No.

    -   **`subsegment`**: Total volume of credit transactions over the last 12 months.

        **`last_12_months_credit_volume`**: Total volume of credit transactions over the last 12 months.

    -   **`last_12_months_debit_volume`**: Total volume of debit transactions over the last 12 months.

    -   **`last_12_months_debit_value`**: Total value of debit transactions over the last 12 months.

    -   **`last_12_months_credit_value`**: Total value of credit transactions over the last 12 months.

## 3. Research Questions

-   What are the key factors that influence customer churn?
-   How accurately can we predict customer churn using machine learning models?
-   Which machine learning model performs best for churn prediction?

## 4. Data Cleaning

-   Change Column Names to Lower Case

    `clean_names()`

    -   Convert all column names to lower case for consistency.

-   Rename Long Column Names

    ```{r, eval=FALSE}
    rename(
      years = "years_with_bank",
      risk = "risk_rating",
      scheme = "scheme_type",
      mobile_app = "mobile_app_adoption",
      internet_banking = "internet_banking_adoption",
      ussd_banking = "ussd_banking_adoption",
      termloan = "termloan_status",
      credit_vol = "last_12_months_credit_volume",
      debit_vol = "last_12_months_debit_volume",
      debit_val = "last_12_months_debit_value",
      credit_val = "last_12_months_credit_value"
    )
    ```

    -   Renames long column names to shorter, more manageable names.

-   Filter Out Specific Values

    ```{r, eval=FALSE}
    filter(!(ave %in% c("GBP", "JPY", "NGN", "SBA", "USD")))
    ```

    -   Removes rows where the `ave` column contains specific currency codes instead of average balance.

-   Convert Numeric Columns.

    ```{r, eval=FALSE}
    mutate(across(c(ave, subsegment, debit_vol, debit_val, credit_val), 
                  ~ parse_number(str_replace_all(., ",", "") %>%
                                   str_replace_all("-", "0"))))
    ```

    -   Converts columns to numeric by removing commas and replacing hyphens with zeros.

-   Convert Character Columns to Factors

    ```{r, eval=FALSE}
    mutate(across(where(is.character), as.factor))
    ```

    -   Converts all character columns to factors for better handling in modeling.

-   Recode Churn Column.

    ```{r,eval=FALSE}
    mutate(churn = factor(ifelse(churn == 1, "churned", "not churned")))
    ```

    -   Recodes the `churn` column to a factor with levels "churned" and "not churned".

-   Save Clean Data

    ```{r, eval=FALSE}
    saveRDS(new_churn, file = "Team A/clean_churn.rds")
    ```

    -   Saves the cleaned dataset to an RDS file for future use.

## 5. Data Preprocessing

-   Normalize numerical features.
-   Encode categorical features.
-   Split the data into training and testing sets.

## 6. Data Exploration

-   Summary statistics of the data.
-   Visualizations (e.g., histograms, bar charts, correlation matrix).
-   Insights from exploratory data analysis.

## 7. Modelling

-   Define and train multiple models (e.g., logistic regression, decision tree, random forest).
-   Evaluate models using metrics such as accuracy, precision, recall, ROC curve, feature importance and F1 score.
-   Select the best-performing model.
-   Summary of findings and recommendations

## 8. Dashboard Building

-   Use Quarto to create an interactive dashboard.
-   Include sections for model performance metrics, ROC curve, feature importance, and customer segmentation.
-   Add interactive filters and visualizations.

## 9. Deployment

-   Deploy the dashboard using a web hosting service (e.g., GitHub Pages, Netlify).
-   Ensure the dashboard is accessible and user-friendly.

## 10. Collaborators

-   **R4DataScienceNigeria Team**:
    -   Collaborator 1
    -   Collaborator 2
    -   Collaborator 3

## 11. Collaboration and Version Control

-   Initialize a git repository `git init`

-   Add files to the repo `git add`

-   Commit changes \`git commit -m "Initial commit"

-   Push to Github `git push`

