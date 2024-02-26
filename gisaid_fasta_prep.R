gisaid_fasta_prep <- function(){
  ###IF NOT INSTALLED PLEASE INSTALL THE PRE-REQUISITE PACKAGES###
  #install.packages("readxl")
  #install.packages("readxl")
  #install.packages("readxl")
  #install.packages("stringdist")
  #install.packages("seqinr")
  
  #Step 1: Read Files
  #Load the libraries
  library(readxl)
  library(writexl)
  library(stringr)
  library(stringdist)
  library(seqinr)
  # Identify and read the .fasta and .xlsx files from the directory
  fasta_file_path <- list.files(path = getwd(), pattern = "\\.fasta$", full.names = TRUE)
  excel_file_path <- list.files(path = getwd(), pattern = "\\.xlsx$", full.names = TRUE)
  # Read the Excel file
  excel_data <- read_excel(excel_file_path)
  fasta_lines <- readLines(fasta_file_path)
  
  #Step 2: Extract Identifiers
  #Extract Identifiers from excel file
  excel_Identifiers <- as.character(excel_data[[1]])
  #Identify the column containing "Hasta" in its name
  name_column_index <- grep("Hasta", colnames(excel_data))
  if(length(name_column_index) == 0) {
    stop("No column with 'Hasta' found in the Excel file.")
  }
  # Extracting first names and converting to uppercase
  excel_data$FirstName <- toupper(sub(" .*", "", excel_data[[name_column_index]]))
  # Applying the function to the FirstName column and fasta Identifiers
  excel_data$NormalizedFirstName <- sapply(excel_data$FirstName, replace_turkish_chars)
  #Extract the fasta headers
  fasta_file_data <- read_fasta_file(fasta_file_path)
  
  #Step 3: Match Identifiers
  #Primary matching
  primary_matches <- intersect(excel_Identifiers, fasta_file_data$identifiers)
  # Initialize all_matched_* with primary matches
  all_matched_excel <- primary_matches
  all_matched_fasta <- primary_matches
  #Check if primary matches are insufficient
  if(length(primary_matches) < length(fasta_file_data$identifiers)) {
    #Secondary matching
    unmatched_excel_primary <- setdiff(excel_Identifiers, primary_matches)
    unmatched_fasta_primary <- setdiff(fasta_file_data$identifiers, primary_matches)
    secondary_matches_fasta <- intersect(unmatched_fasta_primary, excel_data$NormalizedFirstName)
    secondary_matches_excel <- excel_Identifiers[excel_data$NormalizedFirstName %in% secondary_matches_fasta]
    unmatched_excel_secondary <- setdiff(unmatched_excel_primary, secondary_matches_excel)
    unmatched_fasta_secondary <- setdiff(unmatched_fasta_primary, secondary_matches_fasta)
    # Add secondary matches to all_matched_*
    all_matched_excel <- unique(c(all_matched_excel, secondary_matches_excel))
    all_matched_fasta <- unique(c(all_matched_fasta, secondary_matches_fasta))
    #Initialize tertiary matches 
    tertiary_matches_excel <- c()
    tertiary_matches_fasta <- c()
    # Check if primary + secondary matches are insufficient
    if(length(all_matched_excel) < length(fasta_file_data$identifiers) || length(all_matched_excel) < length(excel_Identifiers)) {
      #Tertiary Matching
      #Present Unmatched Identifiers
      cat("Unmatched Excel Identifers:", unmatched_excel_secondary, "\n")
      cat("Unmatched FASTA identfiers:", unmatched_fasta_secondary, "\n")
      
      #User Confirmation 
      user_confirm <- readline(prompt = "Do you want to manually match remaining identifers? (yes/no): ")
      
      #Manual Matching
      if(tolower(user_confirm) == "yes") {
        repeat {
          # Option to view unmatched Identifiers
          view_unmatched <- readline(prompt = "Do you want to view unmatched Identifiers? (yes/no): ")
          if(tolower(view_unmatched) == "yes") {
            cat("Unmatched Excel Identifiers:", unmatched_excel_secondary, "\n")
            cat("Unmatched FASTA Identifiers:", unmatched_fasta_secondary, "\n")
          }
          
          # User inputs for matching pair
          excel_input <- readline(prompt = "Enter the Excel Identifier to match: ")
          fasta_input <- readline(prompt = "Enter the corresponding FASTA Identifier: ")
          
          # Validation
          if(excel_input %in% unmatched_excel_secondary && fasta_input %in% unmatched_fasta_secondary) {
            # Add to matched lists
            tertiary_matches_excel <- c(secondary_matches_excel, excel_input)
            tertiary_matches_fasta <- c(secondary_matches_fasta, fasta_input)
            
            # Remove from unmatched lists
            unmatched_excel_secondary <- setdiff(unmatched_excel_secondary, excel_input)
            unmatched_fasta_secondary <- setdiff(unmatched_fasta_secondary, fasta_input)
            
            # Log or store the manual match
            cat("Manually matched:", excel_input, "with", fasta_input, "\n")
            
          } else {
            cat("Invalid Identifiers. Please enter unmatched Identifiers.\n")
          }
          
          # Check if user wants to continue matching
          continue_matching <- readline(prompt = "Do you want to match another pair? (yes/no): ")
          if(tolower(continue_matching) != "yes") {
            break
          }
        }
      }
  # Add tertiary matches to all_matched_* (This part remains inside the tertiary matching block)
  all_matched_excel <- unique(c(all_matched_excel, tertiary_matches_excel))
  all_matched_fasta <- unique(c(all_matched_fasta, tertiary_matches_fasta))

} else {
  print("All identifiers matched in primary and secondary matching")
}
}
  #Step 4: Filter Data and Assign Virus names to the Sequences
  # Filter Excel Data
  filtered_excel_data <- excel_data[excel_Identifiers %in% all_matched_excel, ] 
  # Virus Name Assignment
  # Extract the second column which contains the virus names.
  potential_virus_names <- as.character(filtered_excel_data[[2]])
  # Check if the potential virus names adhere to the GISAID convention
  # Example: "hCoV-19/Country/Identifier/202X"
  valid_virus_name_pattern <- "^hCoV-19/.+/.+/202\\d$"
  valid_virus_names <- grepl(valid_virus_name_pattern, potential_virus_names)
  
  # If all virus names are valid, use them directly
  # If not, generate the virus names
  if(all(valid_virus_names)) {
    virus_names <- potential_virus_names
  } else {
    current_year <- format(Sys.Date(), "%Y")
    virus_names <- paste0("hCoV-19/Turkey/HSGM-", all_matched_excel, "/", current_year)
  }
  
  filtered_excel_data <- data.frame(
    filtered_excel_data[, 1, drop = FALSE],  # Keep the first column
    VirusName = virus_names,  # Add the generated virus names
    filtered_excel_data[, -1, drop = FALSE]  # Add the rest of the columns, shifting them to the right
  )
  # Create a mapping between Excel identifiers and virus names from filtered_excel_data
  name_mapping <- setNames(filtered_excel_data[[2]], filtered_excel_data[[1]])
  # Ensure the order of FASTA identifiers and sequences
  ordered_fasta_indices <- match(all_matched_fasta, fasta_file_data$identifiers)
  ordered_fasta_headers <- fasta_file_data$headers[ordered_fasta_indices]
  ordered_fasta_sequences <- fasta_file_data$sequences[ordered_fasta_indices]
  # Map the virus names to the ordered FASTA identifiers
  ordered_virus_names <- name_mapping[all_matched_fasta]
  # Construct the final dataframe ensuring the order and correctness of virus names
  final_fasta_df <- data.frame(
    Header = ordered_fasta_headers,
    Sequence = ordered_fasta_sequences,
    VirusName = ordered_virus_names,  # Ensuring virus names are in the correct order
    stringsAsFactors = FALSE
  )
  
  #Step 5: Writing the FASTA File
  # Define the directory and file paths
  new_dir_path <- file.path(getwd(), "Prepped")
  new_fasta_file_path <- file.path(new_dir_path, "prepped_sequences.fasta")
  
  # Check if the directory does not exist
  if (!dir.exists(new_dir_path)) {
    # Create the directory
    dir.create(new_dir_path)
  }
  
  # Writing the matched and renamed sequences to a new FASTA file
  # Writing the matched and renamed sequences to a new FASTA file
  sequences<-list(final_fasta_df$Sequence)
  sequences<-as.list(final_fasta_df$Sequence)
  names<-as.list(final_fasta_df$VirusName)
  write.fasta(sequences = sequences, names = names, file.out = new_fasta_file_path, open = "w", nbchar = 60, as.string = FALSE)
  # Define the Excel file path
  new_excel_file_path <- file.path(new_dir_path, "prepped_data.xlsx")
  # Write the filtered Excel data to the new file path
  writexl::write_xlsx(filtered_excel_data, path = new_excel_file_path)
  print("Step 1 Complete! - Your FASTA file and Excel file are ready for GISAID bulk upload, after following Step 2!")
  
}