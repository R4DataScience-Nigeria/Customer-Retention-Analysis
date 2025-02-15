library(dplyr)
library(readr)
library(googledrive)
library(janitor)
library(stringr)

# Authenticate Google account
#drive_auth()
# Read the file directly from Google Drive
#file <- drive_get("Bank_customer_churn.txt")
#file_id <- as_id(file)
# Read the file directly into R
#path <- drive_download(file_id, path = tempfile(), overwrite = TRUE)
#txt_data <- read_delim(drive_download(file_id, path = tempfile(), overwrite = TRUE), delim = "/t", col_names = TRUE)


##Read in the data------------------------------------------------
churn_df <- read_table("Team A/Bank_customer_churn.txt")

#View(Bank_customer_churn)
#dim(Bank_customer_churn)

## Data Cleaning---------------------------------------------------

str(churn_df) # check data structure 

glimpse(churn_df) # Inspecting data structure


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


