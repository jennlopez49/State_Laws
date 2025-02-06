##### CLEAN & PREP      ------------------------------------------------------
# required pkgs 
library(pdftools)
library(tidyverse)
library(stringr)
library(dplyr)
library(purrr)

# cleaning -- automating -----------------

## clean function
process_pdf_file <- function(text_list) {
  # Step 1: Collapse all columns into a single string
  full_text <- paste(text_list, collapse = " ")
  full_text <- sub(".*Bills:\\s*\\d{,3}\\s+", "\\1", full_text)
  # Step 2: Clean each bill entry
  cleaned_bill_texts <- map(full_text, ~ {
    text_cleaned <- .x %>%
      str_replace_all("\\n", " ") %>%  # Remove all newlines
      str_squish()  # Remove extra spaces
    cleaned_text <- gsub("[\n\r\t]", " ", text_cleaned)  # Remove all newlines, tabs, etc.
    cleaned_text <- gsub("\\s+", " ", cleaned_text)  # Collapse multiple spaces into a single space
    cleaned_text <- gsub("\"", "", cleaned_text)  # Remove rogue quotes
    return(cleaned_text)
  })
  
  # Step 3: Split into individual bills (keeping "Click for History" attached)
  bill_texts <- str_split(cleaned_bill_texts, "(?<=History: Click for History)\\s*", simplify = FALSE)[[1]]
  
  return(bill_texts)
}

# label function 
clean_and_extract_bill <- function(text) {
  state <- str_match(text, "([A-Z]{2})\\s")[,2]  # Extracts 2 uppercase letters for state abbreviation
  bill_number <- str_match(text, "\\b(?:[A-Z]{2,3} [A-Z]+ \\d{1,6})\\s+(\\d{4})\\b")[,1]  # Extracts bill number
  year <- str_match(bill_number, "(\\d{4})$")[,2]  # Extracts year
  title <- str_match(text, "\\d{4}\\s+(.*?)\\s*Status:")[,2]  # Extracts title
  status <- str_match(text, "Status:\\s(.*?)\\sDate of Last Action:")[,2]  # Extracts status
  summary <- str_match(text, "Summary:\\s(.*?)\\sHistory:")[,2]  # Extracts summary
  
  
  # Ensure missing values return empty strings
  return(tibble(
    State = str_squish(state),
    Bill_Number = str_squish(bill_number),
    Year = str_squish(year),
    Title = str_squish(title),
    Status = str_squish(status),
    Summary = str_squish(summary),
    Full_Text = text
  ))
}


######## Trying first with 2012 (Debugging) ------------------------------------
enacted_2012 <-pdf_text("Data_Immigration_Bills_2012_2020/BillsEnacted_2012.pdf")
adopted_2012 <-pdf_text("Data_Immigration_Bills_2012_2020/BillsAdopted_2012.pdf")


textenacted2012 <- paste(enacted_2012, collapse = " ")

billsenacted_2012 <- process_pdf_file(enacted_2012)
                                      #, pattern = "\n\n\n", simplify = TRUE)

rawenacted2012 <- clean_and_extract_bill(billsenacted_2012)
billadopted_2012 <- process_pdf_file(adopted_2012)
rawadopted2012 <- clean_and_extract_bill(billadopted_2012)



##### Automating through list of pdfs -----------------------------------------

generalpath <- "Data_Immigration_Bills_2012_2020/"
namesof.files <- c("BillsAdopted", "BillsEnacted")
listofyears <- c(2012:2020)
file_combinations <- expand.grid(name = namesof.files, year = listofyears)
## creating the paths & storing them
file_paths <- paste0(generalpath, file_combinations$name, "_", file_combinations$year, ".pdf")

final_data <- list()

# Loop through each PDF file
for (i in seq_along(file_paths)) {
  file_path <- file_paths[i]
  tryCatch({
    # Step 1: Read the PDF file
    pdf_text <- pdf_text(file_path)
    
    # Step 2: Preprocess the text
    preprocessed_text <- process_pdf_file(pdf_text)
    
    # Step 3: Extract structured information
    extracted_data <- map_dfr(preprocessed_text, clean_and_extract_bill)
    
    # Check for NAs in the extracted data
    if (any(is.na(extracted_data))) {
      message("Processed file ", i, ": NAs detected in ", file_path)
    } else {
      message("Processed file ", i, ": No issues detected in ", file_path)
    }
    
    # Step 4: Save the preprocessed text (optional)
    preprocessed_file <- gsub(".pdf", "_preprocessed.txt", file_path)
    write_lines(preprocessed_text, preprocessed_file)
    
    # Step 5: Append the extracted data to the final dataset
    final_data <- append(final_data, list(extracted_data))
    
  }, error = function(e) {
    # If an error occurs, print an error message
    message("Error processing file ", i, ": ", file_path, " - ", e$message)
  })
}

### Trying to use purr instead ------
cleaned_text_list <- map(file_paths, ~ {
  pdf_text <- pdf_text(.x)  # Read the PDF text
  process_pdf_file(pdf_text)  # Process the text
})

extracted_data <- map_dfr(cleaned_text_list, ~ map_dfr(.x, clean_and_extract_bill))
cleaned_bills_data <- extracted_data[rowSums(is.na(extracted_data)) < ncol(extracted_data), ]

na_counts_dplyr <- extracted_data %>%
  summarise_all(~ sum(is.na(.)))

### Writing CSV --------------

write.csv(extracted_data, "full_bills_data.csv")






