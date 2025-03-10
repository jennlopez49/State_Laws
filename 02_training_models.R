########## Coding the Bills using a Dictionary  ---------------------------------
# Creating corpus 
corpus_all <- corpus(cleaned_bill_data, text_field = "Summary")

# Tokenize and remove stop words
tokens_all <- tokens(corpus_all, remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(stopwords("en"))

### importing dictionary 
dict <- read_excel("State_Laws_Dictionary.xlsx")
colnames(dict) <- c("P", "SP", "N", "AP", "A", "I")

### changing format to fit quanteda 
dict_long <- dict %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "keyword") %>%
  filter(!is.na(keyword))

# Quanteda Dict
q_dict <- dictionary(split(dict_long$keyword, dict_long$category))

# Create DFM and apply dictionary
dfm_all <- dfm(tokens_all)
dfm_dict <- dfm_lookup(dfm_all, dictionary = q_dict)

# Convert to data frame for classification
classification <- convert(dfm_dict, to = "data.frame")

# Assign categories based on highest match
cleaned_bill_data$class <- apply(classification[, -1], 1, function(row) {
  categories <- names(row)
  max_category <- categories[which.max(row)]
  if (max(row) == 0) {
    return("irrelevant")
  } else {
    return(max_category)
  }
})

################# Using Machine Learning ---------------------------------------
# import 
training_set <- read.csv("bill_training_set.csv")

## clean 
cleaned_bill_data <- cleaned_bill_data %>% filter(State != "PR")
cleaned_bill_data <- cleaned_bill_data %>%
  mutate(Vetoed = if_else(str_detect(Full_Text, "vetoed"), "Vetoed", "Passed"))

training_set_clean <- training_set %>% filter(State != "PR")
training_set_clean <- training_set_clean[,-c(1,2)]
training_set_clean$Class <- str_trim(training_set_clean$Class)

# Tokenize and clean both datasets
prepare_data <- function(data) {
  data %>%
    unnest_tokens(word, Summary) %>%
    anti_join(stop_words, by = "word") %>%
    count(Bill_Number, word) %>%
    cast_dtm(Bill_Number, word, n)
}


# Prepare training and full datasets
dtm_train <- prepare_data(training_set_clean)
dtm_test <- prepare_data(cleaned_bill_data)


# Create a recipe
recipe <- recipe(Class ~ ., data = training_set_clean) %>%
  step_select(-c(Bill_Number, Title, Vetoed, Full_Text, Status, Topic)) %>%
  step_tokenize(Summary) %>%
  step_stopwords(Summary) %>%
  step_tfidf(Summary) %>%
  step_novel(State) %>%
  step_dummy(all_nominal(), -all_outcomes())  # Exclude unnecessary non-numeric columns


# Create model specification
# Define the model with `tune()` for penalty
penalty_grid <- grid_regular(penalty(), levels = 10)
model_spec <- multinom_reg(penalty = tune()) %>%
  set_engine("glmnet")

# Create the workflow
workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(model_spec)

resampling <- vfold_cv(training_set_clean, v = 5)

# Tune the model on the training data
tuned_model <- tune_grid(
  workflow,
  resamples = resampling,
  grid = penalty_grid
)


# Check the tuning results
tuned_model_results <- tuned_model %>%
  collect_metrics()

# View the best penalty value
tuned_model_results
# Choose the best penalty value (e.g., based on RMSE, accuracy, etc.)
best_penalty <- tuned_model_results %>%
  filter(.metric == "accuracy") %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(penalty)

# Finalize the model with the best penalty
final_model <- model_spec %>%
  finalize_model(list(penalty = best_penalty))

# Create the workflow with the finalized model (no need to add the model again)
final_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(final_model)  # Only add the finalized model once

# Refit the model with the best penalty value
final_model_fit <- final_workflow %>%
  fit(data = training_set_clean)

## test set --- 
test_set_predictors <- anti_join(cleaned_bill_data, training_set_clean, by = "Bill_Number")

# Apply the recipe to the test set using bake()
test_set_prepared <- bake(trained_recipe, new_data = test_set_predictors)

# Check the structure of the test set after applying the recipe
colnames(test_set_prepared)

# Predict categories for new bills
predictions <- predict(final_model_fit, new_data = test_set_predictors)

# View the predictions
predictions
# Add predictions to dataset
bill_data <- cleaned_bill_data %>%
  bind_cols(predictions)

# # Save results
# write_csv(bill_data, "classified_bills_ml.csv")
# Split training set into training and testing
set.seed(123)
split <- initial_split(training_set_clean, strata = Class)
train <- training(split)
test <- testing(split)

# Fit and predict on test set
test_predictions <- predict(model_fit, test)
conf_mat <- conf_mat(test, truth = Class, estimate = .pred_class)

# View confusion matrix
conf_mat


################ TRYING ANOTHER WAY N-GRAMS --------------
#write.csv(test_set, "test_set.csv")
training_set_clean$Class <- as.factor(training_set_clean$Class)

# Modify your recipe for n-grams
recipe <- recipe(Class ~ ., data = training_set_clean) %>%
  step_select(-c(Bill_Number, Title, Vetoed, Full_Text, Status, Topic)) %>%
  step_tokenize(Summary, n = 2) %>% # Bigrams
  step_stopwords(Summary) %>%
  step_tfidf(Summary) %>%
  step_novel(State) %>%
  step_dummy(all_nominal(), -all_outcomes())

# Random Forest model
rf_model <- randomForest(Class ~ ., data = training_set_clean, ntree = 500)

# Fit the Random Forest model
print(rf_model)


