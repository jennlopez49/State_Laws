##### CLEAN & PREP      ------------------------------------------------------
# required pkgs 
library(pdftools)
library(tidyverse)
library(stringr)
library(dplyr)
library(purrr)
library(quanteda)
library(readxl)
library(tidymodels)
library(tidytext)
library(tm)
library(recipes)
library(modeldata)
library(textrecipes)
library(glmnet)
library(tune)
library(e1071)
library(textclean)
library(caret)
library(SnowballC)
library(textstem)
library(topicmodels)
library(syuzhet)
library(stringdist)
library(tigris)
library(ggplot2)
library(tidycensus)
library(sf)
library(glue)
library(fuzzyjoin)
# cleaning -- automating -----------------

## clean function
process_pdf_file <- function(text_list) {
  # Step 1: Collapse all columns into a single string
  full_text <- paste(text_list, collapse = " ")
  
  # Step 2: Remove the initial unwanted line (everything before the first state name)
  full_text <- sub(".*?\\b(Alabama|Alaska|Arizona|Arkansas|California|Colorado|Connecticut|Delaware|District of Columbia|Florida|Georgia|Hawaii|Idaho|Illinois|Indiana|Iowa|Kansas|Kentucky|Louisiana|Maine|Maryland|Massachusetts|Michigan|Minnesota|Mississippi|Missouri|Montana|Nebraska|Nevada|New Hampshire|New Jersey|New Mexico|New York|North Carolina|North Dakota|Ohio|Oklahoma|Oregon|Pennsylvania|Rhode Island|South Carolina|South Dakota|Tennessee|Texas|Utah|Vermont|Virginia|Washington|West Virginia|Wisconsin|Wyoming)\\b", 
                   "\\1", full_text)
  
  # Step 3: Clean the text
  cleaned_text <- full_text %>%
    str_replace_all("\\n", " ") %>%  # Remove all newlines
    str_squish() %>%  # Remove extra spaces
    gsub("[\n\r\t]", " ", .) %>%  # Remove all newlines, tabs, etc.
    gsub("\\s+", " ", .) %>%  # Collapse multiple spaces into a single space
    gsub("\"", "", .)  # Remove rogue quotes
  
  # Step 4: Split into individual bills (keeping "Click for History" attached)
  bill_texts <- str_split(cleaned_text, "(?<=History: Click for History)\\s*", simplify = FALSE)[[1]]
  
  return(bill_texts)
}


# alt topic code   # Extract topic (between "Topics:" and "Summary:")
# topic_pattern <- "Topics:\\s*(.*?)\\s*Summary:"
# topic_match <- str_match(bill_text, topic_pattern)
# topic <- ifelse(!is.na(topic_match[,2]), topic_match[,2], NA)

# label function ------------
clean_and_extract_bill <- function(text) {
  state <- str_match(text, "([A-Z]{2})\\s")[,2]  # Extracts 2 uppercase letters for state abbreviation
  bill_number <- str_match(text, "\\b(?:[A-Z]{2,3} [A-Z]+ \\d{1,6})\\s+(\\d{4})\\b")[,1]  # Extracts bill number
  year <- str_match(bill_number, "(\\d{4})$")[,2]  # Extracts year
  title <- str_match(text, "\\d{4}\\s+(.*?)\\s*Status:")[,2]  # Extracts title
  status <- str_match(text, "Status:\\s(.*?)\\sDate of Last Action:")[,2]  # Extracts status
  topics <- str_match(text, "Topics:\\s(.*?)\\sSummary:")[,2]
  summary <- str_match(text, "Summary:\\s(.*?)\\sHistory:")[,2]  # Extracts summary
  
  
  # Ensure missing values return empty strings
  return(tibble(
    State = str_squish(state),
    Bill_Number = str_squish(bill_number),
    Year = str_squish(year),
    Title = str_squish(title),
    Status = str_squish(status),
    Topic = str_squish(topics),
    Summary = str_squish(summary),
    Full_Text = text
  ))
}


##### Automating through list of pdfs -----------------------------------------

generalpath <- "Data_Immigration_Bills_2012_2025/"
namesof.files <- c("BillsAdopted", "BillsEnacted")
listofyears <- c(2020:2025)
file_combinations <- expand.grid(name = namesof.files, year = listofyears)
## creating the paths & storing them
file_paths <- paste0(generalpath, file_combinations$name, "_", file_combinations$year, ".pdf")
# final_data <- list()
# Loop through each PDF file
final_data <- list()  # Initialize an empty list to store extracted data

for (i in seq_along(file_paths)) {
  file_path <- file_paths[i]
  tryCatch({
    # Step 1: Read the PDF file
    pdf_text <- pdf_text(file_path)
    
    # Step 2: Preprocess the text
    preprocessed_text <- process_pdf_file(pdf_text)
    
    # Step 3: Extract structured information
    extracted_data <- map_dfr(preprocessed_text, clean_and_extract_bill)
    
    # Step 4: Save the preprocessed text (optional)
    preprocessed_file <- gsub(".pdf", "_preprocessed.txt", file_path)
    write_lines(preprocessed_text, preprocessed_file)
    
    # Step 5: Append the extracted data to the final dataset
    final_data[[i]] <- extracted_data  # Store each dataset in a list
    
    # Message to track progress
    if (any(is.na(extracted_data))) {
      message("Processed file ", i, ": NAs detected in ", file_path)
    } else {
      message("Processed file ", i, ": No issues detected in ", file_path)
    }
    
  }, error = function(e) {
    # If an error occurs, print an error message
    message("Error processing file ", i, ": ", file_path, " - ", e$message)
  })
}

# Combine all extracted data into one final dataset
final_dataset <- bind_rows(final_data)

final_2025_bills <- final_dataset %>% filter(Year > 2020)

### OUTPUT ---- CSV FILE 

write.csv(final_2025_bills, "final_bills_data_2025.csv")

#### Custom Geo Functions 

get_place_geo <- function(state_fips, place_code) {
  tryCatch({
    pl <- tigris::places(state = state_fips, year = 2020) %>%
      sf::st_transform(4326)  # Transform to WGS84
    pl %>% filter(PLACEFP == place_code)
  }, error = function(e) NULL)
}

get_sub_geo <- function(state_fips, place_code) {
  tryCatch({
    county_subs <- tigris::county_subdivisions(state = state_fips, year = 2020) %>%
      sf::st_transform(4326)  # Transform to WGS84
    county_subs %>% filter(GEOID == paste0(state_fips, place_code))
  }, error = function(e) NULL)
}

get_county_geo <- function(state_fips, county_fips) {
  tryCatch({
    counties <- tigris::counties(state = state_fips, year = 2020) %>%
      sf::st_transform(4326)  # Transform to WGS84
    target_geoid <- paste0(state_fips, county_fips)
    
    match <- counties %>% dplyr::filter(GEOID == target_geoid)
    
    if (nrow(match) == 0) {
      message(paste("No match for county GEOID:", target_geoid))
      return(NULL)
    }
    
    match
  }, error = function(e) {
    message(paste("get_county_geo failed:", e$message))
    return(NULL)
  })
}

get_acs_filtered <- function(state, geo, place_codes = NULL, county_codes = NULL,
                             year_signed, last_year,
                             vars = c("B01003_001", "B03001_003", "B05002_013"),
                             acs_years = c(2010, 2014, 2016, 2020, 2023)) {
  
  valid_years <- acs_years[acs_years >= year_signed & acs_years <= last_year]
  
  if (length(valid_years) == 0) {
    message(glue::glue("⏩ Skipping {geo}, {state} – no ACS years fall within {year_signed}–{last_year}"))
    return(NULL)
  }
  
  acs_list <- list()
  
  for (yr in valid_years) {
    tryCatch({
      df <- get_acs(
        geography = geo,
        variables = vars,
        year = yr,
        state = state,
        survey = "acs5",
        output = "wide"
      ) %>%
        mutate(geo = geo, state_code = state, year = yr) %>%
        mutate(GEOID = as.character(GEOID))
      
      if (geo == "place" & !is.null(place_codes)) {
        # place_codes should be character vector of full GEOIDs (state + place code)
        place_codes <- as.character(place_codes)
        df_filtered <- df %>% filter(GEOID %in% place_codes)
        acs_list[[as.character(yr)]] <- df_filtered
        
      } else if (geo == "county" & !is.null(county_codes)) {
        # county_codes should be character vector of full GEOIDs (state + county code)
        county_codes <- as.character(county_codes)
        df_filtered <- df %>% filter(GEOID %in% county_codes)
        acs_list[[as.character(yr)]] <- df_filtered
        
      } else if (geo == "state") {
        # for state geo, usually just one row
        acs_list[[as.character(yr)]] <- df
      } else {
        message(glue::glue("⚠️ No matching codes provided for {geo}"))
        acs_list[[as.character(yr)]] <- NULL
      }
    }, error = function(e) {
      message(glue::glue("⚠️ Failed: {geo}, {state}, {yr} – {e$message}"))
      acs_list[[as.character(yr)]] <- NULL
    })
  }
  
  combined_df <- bind_rows(acs_list)
  return(combined_df)
}
