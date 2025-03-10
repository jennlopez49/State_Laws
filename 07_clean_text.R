######## cleaning out irrelevant ------- 
dictionary <- read_xlsx("State_Laws_Dictionary.xlsx")
p <- dictionary$ProImmigrant
sp <- dictionary$Symbolically.Pro
n <- dictionary$Neutral
a <- dictionary$AntiImmigrant
i <- c(dictionary$Irrelevant.Commemorative.Misc, "high school graduation",
          "civics")
gen_terms <- c(
  "immigration", "migrant", "migrants", "noncitizen", "noncitizens", 
  "residency", "visa", "migration", "foreign national", "foreign nationals", 
  "legal resident", "legal residents", "citizenship process", "immigrants",
  "immigrant", "foreign born"
)


cleaned_bill_data <- cleaned_bill_data %>%
  mutate(
    has_anti = str_detect(tolower(Summary), paste(a, collapse = "|")),
    has_sa = str_detect(tolower(Summary), paste(sa, collapse = "|")),
    has_n = str_detect(tolower(Summary), paste(n, collapse = "|")),
    has_sp = str_detect(tolower(Summary), paste(sp, collapse = "|")),
    has_pro = str_detect(tolower(Summary), paste(p, collapse = "|")),
    has_irrelevant = str_detect(tolower(Summary), paste(i, collapse = "|")),
    has_gen = str_detect(tolower(Summary), paste(gen_terms, collapse = "|"))
  )

filtered_bills <- cleaned_bill_data %>%
  filter(!(has_irrelevant & !has_anti & !has_pro & !has_sa & !has_sp & !has_n) | has_gen)


removed_bills <- cleaned_bill_data %>%
  filter((has_irrelevant & !has_anti & !has_pro & !has_sa & !has_sp & !has_n) & !has_gen)
removed_bills$Class <- "I"
# Check the number of removed bills containing immigration-related terms
sum(removed_bills$has_gen)


# Check the number of matching bills ------- only catches 90 of the 210  ---- used this to adjust dictionary & workflow ABOVE ------
sum(matching_bills)

#### Pulling out new dictionary terms 

irrelevant_corpus <- Corpus(VectorSource(only_irr_train$Summary))

# Clean the text
irrelevant_corpus <- tm_map(irrelevant_corpus, content_transformer(tolower))
irrelevant_corpus <- tm_map(irrelevant_corpus, removePunctuation)
irrelevant_corpus <- tm_map(irrelevant_corpus, removeNumbers)
irrelevant_corpus <- tm_map(irrelevant_corpus, removeWords, stopwords("en"))
irrelevant_corpus <- tm_map(irrelevant_corpus, stripWhitespace)

# Create Document-Term Matrix (DTM)
irrelevant_dtm <- DocumentTermMatrix(irrelevant_corpus)
irrelevant_dtm <- irrelevant_dtm[row_sums > 0, ]


# Inspect the DTM (number of terms)
inspect(irrelevant_dtm[1:5, 1:5])

# Apply Latent Dirichlet Allocation (LDA)
lda_model <- LDA(irrelevant_dtm, k = 5, control = list(seed = 1234))

# Inspect the topics (top terms for each topic)
terms(lda_model, 10)  # Shows top 10 terms for each topic

### Double - checking 
terms <- c("\\bimmigration\\b", "\\bimmigrant\b",
           "\\bimmigrants\\b")
pattern <- paste(terms, collapse = "|")
filtered_with_immigration <- removed_bills %>%
  filter(str_detect(Summary, pattern))

write.csv(filtered_with_immigration, "irrelevant_immigration.csv")
         

######### Re-checking topics & frequent terms for new worklfow ------

irr_corpus <- Corpus(VectorSource(removed_bills$Summary))

# Clean the text
irr_corpus <- tm_map(irr_corpus, content_transformer(tolower))
irr_corpus <- tm_map(irr_corpus, removePunctuation)
irr_corpus <- tm_map(irr_corpus, removeNumbers)
irr_corpus <- tm_map(irr_corpus, removeWords, stopwords("en"))
irr_corpus <- tm_map(irr_corpus, stripWhitespace)

# Create Document-Term Matrix (DTM)
irr_dtm <- DocumentTermMatrix(irr_corpus)
irr_dtm <- irr_dtm[row_sums > 0, ]


# Inspect the DTM (number of terms)
inspect(irr_dtm[1:5, 1:5])

# Apply Latent Dirichlet Allocation (LDA)
lda_model_2 <- LDA(irr_dtm, k = 5, control = list(seed = 1234))

# Inspect the topics (top terms for each topic)
terms(lda_model_2, 10)  # Shows top 10 terms for each topic

### CHECKS OUT (filtered out resolutions that commend individuals & groups of people WITHOUT referencing immigration in a meaningful way)

######### Filtering out Irrelevant out of training & full data set --> ---------
training_set_clean <- complete_training[complete_training$Class != "I",]
removed_bill_ids <- removed_bills$Bill_Number
cleaned_text_set <- cleaned_bill_data %>% filter(!Bill_Number %in% removed_bill_ids)

write.csv(cleaned_text_set, "cleaned_text_set.csv")
