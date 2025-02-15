library(dplyr)
library(readr)
library(googledrive)
library(janitor)
library(stringr)
##Read in the data------------------------------------------------
churn_df <- read_table("Team A/Bank_customer_churn.txt")
## Data Cleaning---------------------------------------------------
#str(churn_df) # check data structure 
#glimpse(churn_df) # Inspecting data structure
# 1. change column names to lower case
new_churn <- churn_df |>
  clean_names() |> # change names to lower case
  rename( #rename long column names
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
    credit_val = "last_12_months_credit_value",
  ) |>
  filter(!(ave %in% c("GBP", "JPY", "NGN", "SBA", "USD"))) |>
  mutate(across(c(ave, subsegment, debit_vol, debit_val, credit_val), 
                ~ parse_number(str_replace_all(., ",", "") %>%
                                 str_replace_all("-", "0")))) |>
  mutate(across(where(is.character), as.factor)) |>
  mutate(churn = factor(ifelse(churn == 1, "churned", "not churned")))

# Save the data frame to an RDS file
saveRDS(new_churn, file = "Team A/clean_churn.rds")
# Read the data frame from the RDS file
#df <- readRDS("path/to/your/file.rds")

