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
  #full_text <- sub(".*Bills:\\s*\\d{,3}\\s+", "\\1", full_text) ###### might be too aggressive 
  # Step 2: Clean each bill entry
  cleaned_text <- full_text %>%
    str_replace_all("\\n", " ") %>%  # Remove all newlines
    str_squish() %>%  # Remove extra spaces
    gsub("[\n\r\t]", " ", .) %>%  # Remove all newlines, tabs, etc.
    gsub("\\s+", " ", .) %>%  # Collapse multiple spaces into a single space
    gsub("\"", "", .) 
  
  # Step 3: Split into individual bills (keeping "Click for History" attached)
  bill_texts <- str_split(cleaned_text, "(?<=History: Click for History)\\s*", simplify = FALSE)[[1]]
  
  return(bill_texts)
}

# Looping through and creating txt files of the pdfs ---------------------------
##### Automating through list of pdfs -----------------------------------------

generalpath <- "Data_Immigration_Bills_2012_2020/"
namesof.files <- c("BillsAdopted", "BillsEnacted")
listofyears <- c(2012:2020)
file_combinations <- expand.grid(name = namesof.files, year = listofyears)
## creating the paths & storing them
file_paths <- paste0(generalpath, file_combinations$name, "_", file_combinations$year, ".pdf")
# final_data <- list()
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
    
    # # Check for NAs in the extracted data
    # if (any(is.na(extracted_data))) {
    #   message("Processed file ", i, ": NAs detected in ", file_path)
    # } else {
    #   message("Processed file ", i, ": No issues detected in ", file_path)
    # }
    
    # Step 4: Save the preprocessed text (optional)
    preprocessed_file <- gsub(".pdf", "_preprocessed.txt", file_path)
    write_lines(preprocessed_text, preprocessed_file)
    
    # # Step 5: Append the extracted data to the final dataset
    # final_data <- append(final_data, list(extracted_data))
    
  }, error = function(e) {
    # If an error occurs, print an error message
    message("Error processing file ", i, ": ", file_path, " - ", e$message)
  })
}


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
    Summary = str_squish(summary),
    Full_Text = text
  ))
}


######## Trying first with 2012 (Debugging) ------------------------------------
enacted_2012 <-pdf_text("Data_Immigration_Bills_2012_2020/BillsEnacted_2012.pdf")
adopted_2012 <-pdf_text("Data_Immigration_Bills_2012_2020/BillsAdopted_2012.pdf")


#textenacted2012 <- paste(enacted_2012, collapse = " ")

billsenacted_2012 <- process_pdf_file(enacted_2012)
                                      #, pattern = "\n\n\n", simplify = TRUE)

rawenacted2012 <- clean_and_extract_bill(billsenacted_2012)
billadopted_2012 <- process_pdf_file(adopted_2012)
rawadopted2012 <- clean_and_extract_bill(billadopted_2012)



##### Automating through list of pdfs -----------------------------------------

# generalpath <- "Data_Immigration_Bills_2012_2020/"
# namesof.files <- c("BillsAdopted", "BillsEnacted")
# listofyears <- c(2012:2020)
# file_combinations <- expand.grid(name = namesof.files, year = listofyears)
## creating the paths & storing them
file_paths_txt <- paste0(generalpath, file_combinations$name, "_", file_combinations$year,"_preprocessed.txt")

process_txt_file <- function(file_path) {
  # Step 1: Read the text from the .txt file
  file_text <- read_lines(file_path)  # Reads the file line by line
  
  # Step 2: Combine all lines into one large text block (optional)
  cleaned_text <- paste(file_text, collapse = " ")  # Collapse into a single string if necessary
  
  # Step 3: Process the cleaned text as needed
  #processed_text <- process_pdf_file(cleaned_text)  # You can use process_pdf_file function here
  
  return(cleaned_text)
}

# Apply this function to all files in your list
cleaned_text_list <- map(file_paths_txt, ~ {
  process_txt_file(.x)  # Read and process each .txt file
})



### Trying to use purr instead ------
# cleaned_text_list <- map(file_paths_txt, ~ {
#   pdf_text <- pdf_text(.x)  # Read the PDF text
#   process_pdf_file(pdf_text)  # Process the text
# })

extracted_data <- map_dfr(cleaned_text_list, ~ map_dfr(.x, clean_and_extract_bill))

cleaned_bills_data <- extracted_data[rowSums(is.na(extracted_data)) < ncol(extracted_data), ]

na_counts_dplyr <- extracted_data %>%
  summarise_all(~ sum(is.na(.)))

### Writing CSV --------------

write.csv(extracted_data, "full_bills_data.csv")






