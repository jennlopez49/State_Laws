#### IMPORTING THE EDITED FINAL BILLS DATA (corrected some missing bill and years for VA in 2014)

bills_data <- read.csv("final_bills_data.csv")

### GETTING RID OF NAs -- first making sure there are only empty rows 
bills_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "NA_Count")

cleaned_bill_data <- bills_data %>% select(!Helper_Col) %>% na.omit()

### Randomly getting 400 entries to code by hand as training set --------------
set.seed(123)  # Set seed for reproducibility
training_set <- cleaned_bill_data %>% sample_n(400)

### write csv file to manually code 
write.csv(training_set, "bill_training_set.csv")
