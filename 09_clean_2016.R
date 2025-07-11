### 2016 ---- subsetting by years to make it more manageable

cleaned_text_set <- read.csv("cleaned_text_set.csv")

sub_12_16 <- cleaned_text_set %>% filter(Year %in% 2012:2016)

complete_training <- read.csv("complete_training.csv")

sub_12_16 <- sub_12_16[,-c(13:20)]

complete_training <- complete_training %>%
  select(State, Bill_Number, Year, Title, Status, Topic, Summary, Full_Text,
         Vetoed, Class)

sub_12_16_coded <- sub_12_16 %>%
  left_join(complete_training %>% select(Bill_Number, Class), by = "Bill_Number")
write.csv(sub_12_16_coded, "coding_2012_2016.csv")

### 2016 - 2020 bills 

sub_16_20 <- cleaned_text_set %>% filter(Year %in% 2017:2020)
sub_16_20 <- sub_16_20[,-c(13:20)]
sub_16_20_coded <- sub_16_20 %>%
  left_join(complete_training %>% select(Bill_Number, Class), by = "Bill_Number")

write.csv(sub_16_20_coded, "coding_2016_2020.csv")


### re-importing to clean up indicator 

cleaned_text_full <- read.csv("coding_2012_2016_full.csv")

df_cleaned <- cleaned_text_full %>%
  distinct(Year, processed_Summary, .keep_all = TRUE)

df_associated_bills <- df_cleaned %>%
  filter(str_detect(Full_Text, regex("associated bill", ignore_case = TRUE)))

# View the resulting dataset
head(df_associated_bills)

df_no_duplicates <- df_associated_bills %>%
  distinct(Year, Topic, processed_Summary, .keep_all = TRUE)

# Step 4: If needed, drop rows where a Bill_ID is associated with a different processed_Summary (this can preserve the unique bill versions/entries)
df_unique_bills <- df_no_duplicates %>%
  group_by(Bill_Number) %>%
  filter(n_distinct(processed_Summary) == 1) %>%
  ungroup()

### printing out last duplicates -- 
write.csv(df_associated_bills, "df_associated_bills.csv")

### getting out irrelevant 

df_cleaned <- df_cleaned %>% mutate(Class_Cleaned = str_trim(Class_Cleaned)) %>%
  filter(!Class_Cleaned == "I")

df_cleaned<-  df_cleaned %>% mutate(Imm_Class_Code = case_when(
  Class_Cleaned == "A" ~ -1,
  Class_Cleaned == "SA" ~ -0.25,
  Class_Cleaned == "N" ~ 0,
  Class_Cleaned == "SP" ~ 0.25,
  Class_Cleaned == "P" ~ 1,
  TRUE ~ NA_real_)
)

df_state_summary <- df_cleaned %>%
  group_by(State) %>%  # Group by state
  summarize(Imm_Class_2016 = sum(Imm_Class_Code, na.rm = TRUE)) %>%  # Sum the classification points
  ungroup() 
df_cleaned %>%
  group_by(State) %>%
  summarize(total_bills = n(), 
            sum_classification_points = sum(Imm_Class_Code)) %>%
  arrange(desc(total_bills))


df_concrete_policies <- df_cleaned %>%
  filter(!(Class_Cleaned %in% c("SA", "SP")))  # Excludes SA and SP bills

table(df_concrete_policies$State,df_concrete_policies$Class_Cleaned)


# Summing the classification points for concrete policies
df_concrete_indicators <- df_concrete_policies %>%
  group_by(State) %>%
  summarise(Imm_Code_Pts = sum(Imm_Class_Code))  # Sum points for A, N, P

# View the resulting dataset
head(df_concrete_indicators)

## csv file 

full_indicators <- inner_join(
  df_concrete_indicators, 
  df_state_summary, 
  by = "State"
)

colnames(full_indicators) <- c("State", "Imm_Class_Concrete_2016", "Imm_Class_Full_2016")

write.csv(full_indicators, "full_indicators_2016.csv")

