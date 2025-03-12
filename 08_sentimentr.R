### Combining training sets ----------
training_set <- read.csv("bill_training_set.csv")
training_set <- training_set[,-c(1:2)]
extended_set <- read.csv("anti_sampled.csv")

extended_set<- extended_set[, colnames(training_set)] 

complete_training <- rbind(training_set, extended_set)
complete_training$Class <- str_trim(complete_training$Class)
training_set_clean <- complete_training[complete_training$Class != "I",]
## write csv file
write.csv(training_set_clean, "complete_training.csv")

cleaned_bill_data <- bills_data %>% select(!Helper_Col) %>% na.omit()
cleaned_bill_data <- cleaned_bill_data %>% filter(State != "PR")
cleaned_bill_data <- cleaned_bill_data %>%
  mutate(Vetoed = if_else(str_detect(Full_Text, "vetoed"), "Vetoed", "Passed"))

######## Prepping Labeled Data Set --------------
# Convert to lowercase
complete_training$processed_Summary <- tolower(complete_training$Summary)

# Remove numbers, punctuation, and whitespace
complete_training$processed_Summary <- removePunctuation(complete_training$processed_Summary)
complete_training$processed_Summary <- removeNumbers(complete_training$processed_Summary)
complete_training$processed_Summary <- stripWhitespace(complete_training$processed_Summary)

# Remove stopwords (common words like 'the', 'and', etc.)
complete_training$processed_Summary <- removeWords(complete_training$processed_Summary, stopwords("en"))

# Optional: Stem the words (reduces words to their root form)
#complete_training$processed_Summary <- lemmatize_words(complete_training$processed_Summary)

# Create a corpus from the text column

corpus <- Corpus(VectorSource(complete_training$processed_Summary))

# Create the Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(corpus)

# Convert DTM to a matrix
dtm_matrix <- as.matrix(dtm)

# Optional: Use tf-idf (Term Frequency - Inverse Document Frequency) for weighting terms
tfidf <- weightTfIdf(dtm)
### 
complete_dtm_df <- as.data.frame(dtm_matrix)

# Add the 'Class' column to the complete_dtm_df
complete_dtm_df$Class <- complete_training$Class  # Assuming Class is in complete_set

### clean unlabeled set -----------

# Preprocess the cleaned_bill_set (unlabeled)
cleaned_unlabeledcorpus <- Corpus(VectorSource(cleaned_bill_data$Summary))

# Apply necessary preprocessing steps (e.g., convert to lowercase, remove punctuation, etc.)
cleaned_unlabeledcorpus <- tm_map(cleaned_unlabeledcorpus, content_transformer(tolower))
cleaned_unlabeledcorpus <- tm_map(cleaned_unlabeledcorpus, removePunctuation)
cleaned_unlabeledcorpus <- tm_map(cleaned_unlabeledcorpus, removeNumbers)
cleaned_unlabeledcorpus <- tm_map(cleaned_unlabeledcorpus, removeWords, stopwords("en"))
cleaned_unlabeledcorpus <- tm_map(cleaned_unlabeledcorpus, stripWhitespace)
#cleaned_unlabeledcorpus <- tm_map(cleaned_unlabeledcorpus, content_transformer(function(x) lemmatize_words(x)))

# Create Document-Term Matrix (DTM) for the cleaned_bill_set
cleaned_unlabeleddtm <- DocumentTermMatrix(cleaned_unlabeledcorpus)

# Convert DTM to matrix
cleaned_dtm_matrix_un <- as.matrix(cleaned_unlabeleddtm)
cleaned_dtm_df_un <- as.data.frame(cleaned_dtm_matrix_un)

##### Train model on labeled set ------
class_weights <- c("A" = 1, "I" = 1, "N" = 1, "P" = 1, "SA" = 5, "SP" = 5)
nb_model <- naiveBayes(Class ~ ., data = complete_dtm_df, weights = class_weights, laplace = 1)

###### make predictions -----------
# Make predictions on the cleaned_bill_set

predictions <- predict(nb_model, cleaned_dtm_df_un)

# Add the predictions to the cleaned_bill_set data
cleaned_bill_data$Predicted_Class <- predictions

##### Accuracy Rate ---- 
# 
merged_set <- merge(cleaned_bill_data, complete_training[, c("Bill_Number", "Class")], by = "Bill_Number") 

# Compare the predicted labels with the true labels from complete_set
accuracy <- sum(merged_set$Predicted_Class == merged_set$Class) / nrow(merged_set)


##### Trying with sentiment analysis --------------------------------------------
## loading clean data set (no irrelevant bills)
clean_text_set <- read.csv("cleaned_text_set.csv")

# Step 1: Preprocess the cleaned_bill_data$Summary (entire summaries)
clean_text_set$processed_summary <- clean_text_set$Summary %>%
  tolower() %>%
  removePunctuation() %>%
  removeNumbers() %>%
  removeWords(stopwords("en")) %>%
  stripWhitespace() 

# Check the cleaned summaries (these are entire summaries of the bills)
head(clean_text_set$processed_summary)
# Remove rows with NA or empty summaries
clean_text_set <- clean_text_set %>%
  filter(!is.na(processed_summary) & processed_summary != "")


# Step 2: Sentiment analysis for entire sentences (use the processed summaries)
# Get sentiment scores for the entire summaries (not word-by-word)
sentiment_scores <- get_sentiment(clean_text_set$processed_summary, method = "syuzhet")

# Add sentiment scores back to your dataset
clean_text_set$sentiment <- sentiment_scores

# Check the results (view the sentiment scores for summaries)
head(clean_text_set[, c("processed_summary", "sentiment")])



### seeing distribution ---- 
# Plot sentiment distribution
ggplot(clean_text_set, aes(x = sentiment)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Sentiment Distribution of Bills",
       x = "Sentiment Score",
       y = "Frequency") +
  theme_minimal()

# skewed towards positive 

##### resolutions only ---------

resos <- clean_text_set %>% filter(str_detect(Summary, "\\bresolution\\b"))
resos_IDs <- resos$Bill_Number

cleaned_bills_only <- clean_text_set %>% filter(!Bill_Number %in% resos_IDs)

ggplot(resos, aes(x = sentiment)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "gray", alpha = 0.7) +
  labs(title = "Sentiment Distribution of Resolutions",
       x = "Sentiment Score",
       y = "Frequency") +
  theme_bw()

ggplot(cleaned_bills_only, aes(x = sentiment)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "gray", alpha = 0.7) +
  labs(title = "Sentiment Distribution of Bills",
       x = "Sentiment Score",
       y = "Frequency") +
  theme_bw()

# Adding classification --------
cleaned_bills_only$Class <- ifelse(cleaned_bills_only$sentiment > 0, "P",
                                         ifelse(cleaned_bills_only$sentiment < 0, "A", "N"))

resos$Class <- ifelse(resos$sentiment > 0, "SP",
                                   ifelse(resos$sentiment < 0, "SA", "N"))

# combining back together
full_set <- rbind(cleaned_bills_only, resos)

## checking accuracy with trained set 
training_IDs <- training_set_clean$Bill_Number
labeled_full_set <- full_set %>% filter(Bill_Number %in% training_IDs)
labeled_full_set$Sent_Class <- labeled_full_set$Class

# Merge labeled full set with training set based on Bill_Number
comparison_df <- labeled_full_set %>%
  select(Bill_Number, Sent_Class) %>%  # sentiment_class: assigned based on sentiment score
  inner_join(training_set_clean %>% select(Bill_Number, Class), 
             by = "Bill_Number")
comparison_df <- comparison_df %>% mutate(
  Sent_Class = as.factor(Sent_Class),
  Class = as.factor(Class),
)
# Calculate accuracy
accuracy <- mean(comparison_df$Sent_Class == comparison_df$Class)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

