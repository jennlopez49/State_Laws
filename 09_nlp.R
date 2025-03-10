
# Specify the path to your downloaded Stanford CoreNLP directory
initCoreNLP("path/to/stanford-corenlp-full-2018-10-05")

# Assuming you have cleaned the text in `processed_summary` already
sentiment_scores_corenlp <- sapply(clean_text_set$processed_Summary, function(text) {
  sentiment <- annotateString(text)
  sentiment$sentiment
})

# Add sentiment scores to your dataset
clean_text_set$sentiment_corenlp <- sentiment_scores_corenlp

resos <- clean_text_set %>% filter(str_detect(Summary, "\\bresolution\\b"))
resos_IDs <- resos$Bill_Number

cleaned_bills_only <- clean_text_set %>% filter(!Bill_Number %in% resos_IDs)

cleaned_bills_only$Class_corenlp <- ifelse(cleaned_bills_only$sentiment_corenlp == "Positive", "P",
                                       ifelse(cleaned_bills_only$sentiment_corenlp == "Negative", "A", "N"))
resos$Class_corenlp <- ifelse(resos$sentiment_corenlp == "Positive", "SP",
                                           ifelse(resos$sentiment_corenlp == "Negative", "SA", "N"))
full_set <-cbind(cleaned_bills_only, resos)

labeled_full_set <- full_set %>% filter(Bill_Number %in% training_IDs)
labeled_full_set$Sent_Class <- labeled_full_set$Class


comparison_df_corenlp <- labeled_full_set %>%
  select(Bill_Number, Class_corenlp) %>%
  inner_join(training_set_clean %>% select(Bill_Number, Class), 
             by = "Bill_Number")
comparison_df_corenlp <- comparison_df_corenlp %>%
  mutate(Class_corenlp = as.factor(Class_corenlp),
         Class = as.factor(Class))

# Calculate accuracy
accuracy_corenlp <- mean(comparison_df_corenlp$Class_corenlp == comparison_df_corenlp$Class)
print(paste("Accuracy with CoreNLP:", round(accuracy_corenlp * 100, 2), "%"))
