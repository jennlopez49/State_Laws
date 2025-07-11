
######## Trying first with 2012 (Debugging) ------------------------------------
enacted_2012 <-pdf_text("Data_Immigration_Bills_2012_2025/BillsEnacted_2012.pdf")
adopted_2012 <-pdf_text("Data_Immigration_Bills_2012_2025/BillsAdopted_2012.pdf")


#textenacted2012 <- paste(enacted_2012, collapse = " ")

billsenacted_2012 <- process_pdf_file(enacted_2012)
                                      #, pattern = "\n\n\n", simplify = TRUE)

rawenacted2012 <- clean_and_extract_bill(billsenacted_2012)
billadopted_2012 <- process_pdf_file(adopted_2012)
rawadopted2012 <- clean_and_extract_bill(billadopted_2012)



##### Automating through list of pdfs -----------------------------------------

generalpath <- "Data_Immigration_Bills_2012_2025/"
namesof.files <- c("BillsAdopted", "BillsEnacted")
listofyears <- c(2020:2025) ## switched out to 2020-2025 
file_combinations <- expand.grid(name = namesof.files, year = listofyears)
## creating the paths & storing them
file_paths_txt <- paste0(generalpath, file_combinations$name, "_", file_combinations$year,"_preprocessed.txt")

process_txt_file <- function(file_path) {
  # Step 1: Read the text from the .txt file
  file_text <- read_lines(file_path)  # Reads the file line by line
  
  # Step 2: Combine all lines into one large text block (optional)
  cleaned_text <- paste(file_text, collapse = " ")  # Collapse into a single string if necessary
  
  # Step 3: Process the cleaned text as needed
  #processed_text <- process_pdf_file(cleaned_text)  # can use process_pdf_file function here
  
  return(cleaned_text)
}

# Apply this function to all files in list
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

### Re-running on 2020-2025 bills 






