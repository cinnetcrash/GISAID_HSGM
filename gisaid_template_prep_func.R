gisaid_template_prep <- function(){
  #Step 1: Load the libraries and upload relevant data and GISAID template
  library(readxl)
  library(writexl)
  library(seqinr)
  all_files <- list.files(pattern = "\\.(xlsx|fasta)$")
  # Identify the Excel and FASTA files
  excel_file <- all_files[grep("\\.xlsx$", all_files)]
  fasta_file_name <- all_files[grep("\\.fasta$", all_files)]
  
  # Check if exactly one Excel and one FASTA file are present
  if(length(excel_file) != 1 || length(fasta_file_name) != 1) {
    stop("Ensure one Excel file and one FASTA file are in the working directory.")
  }
  # Read the existing Excel data
  excel_data <- read_excel(excel_file)
  # Read the FASTA file
  fasta_file <- read.fasta(fasta_file_name)
  # Read the GISAID template
  # Note: The GISAID template path should be specified unless it's also in the working directory
  gisaid_template <- read_excel("/home/newusername/Masaüstü/GISAID_HSGM/UploadingFiles/gisaid_hcov-19_batch_upload_template_and_instructions/20230515_EpiCoV_BulkUpload_Template.xls", sheet = "Submissions")
  original_header <- original_headers <- colnames(gisaid_template)
  gisaid_template <- read_excel("/home/newusername/Masaüstü/GISAID_HSGM/UploadingFiles/gisaid_hcov-19_batch_upload_template_and_instructions/20230515_EpiCoV_BulkUpload_Template.xls", sheet = "Submissions", skip = 1)
  
  #Step 2: Data Preparation
  #Step 2.1: Excel Data 
  # Function to check if a value is formatted like a date
  # Identify the collection date column
  date_column_name <- grep("Tarih", names(excel_data), ignore.case = TRUE, value = TRUE)
  if(length(date_column_name) == 0) {
    stop("Couldn't find a column that includes the string 'Tarih'. Please ensure the Excel file has a relevant column.")
  }
  # Attempt to transform the date to "YYYY-MM-DD" format
  excel_data[[date_column_name]] <- as.Date(excel_data[[date_column_name]], format = "%Y-%m-%d")
  missing_dates <- is.na(excel_data[[date_column_name]])
  # Check for dates without day information
  dates_without_day <- grepl("^\\d{4}-\\d{2}$", excel_data[[date_column_name]])
  missing_dates[dates_without_day] <- TRUE
  # Filter out samples without a valid collection date
  samples_with_date <- excel_data[!missing_dates, ]
  # Identify and report samples removed
  removed_samples <- setdiff(excel_data$VirusName, samples_with_date$VirusName)
  if(length(removed_samples) > 0) {
    cat("Removed samples due to missing or invalid collection dates:\n", paste(removed_samples, collapse = ", "), "\n")
  }
  # Update excel_data to only include samples with a valid collection date
  excel_data <- samples_with_date
  
  #Step 2.2: FASTA File
  # Extract virus names from the filtered Excel data
  valid_virus_names <- excel_data$VirusName
  # Filter the FASTA sequences based on valid virus names
  fasta_file <- fasta_file[names(fasta_file) %in% valid_virus_names]
  
  #Step 3. Populate the GISAID Template
  if(length(excel_data) > 0) {
    #Step.3.1 Identify the location for the GISAID template
    city_column_name <- grep("Şehir", names(excel_data), ignore.case = TRUE, value = TRUE)
    if(length(city_column_name) == 0) {
      stop("Couldn't find the 'Şehir' column. Please ensure the Excel file has a column named 'Şehir' or similar.")
    }
    #Extract the city or province names from this column
    cities <- excel_data[[city_column_name]]
    #Step 3.2: Remove the first row from the GISAID template and format it accordingly
    gisaid_template <- gisaid_template[-1, ]
    filler_headers<-colnames(gisaid_template)
    num_samples <- length(valid_virus_names)
    gisaid_template <- data.frame(matrix(ncol = ncol(gisaid_template), nrow = num_samples))
    colnames(gisaid_template) <- filler_headers
    
    # 3.3 Populate the gisaid_template with our data
    gisaid_template$Submitter <- "syalcin"
    gisaid_template$`FASTA filename` <- fasta_file_name
    gisaid_template$`Virus name` <- valid_virus_names
    gisaid_template$Type <- "betacoronavirus"
    gisaid_template$`Passage details/history` <- "Original"
    gisaid_template$`Collection date` <- excel_data[[date_column_name]]
    gisaid_template$Host <- "human"
    gisaid_template$Gender <- "unknown"
    gisaid_template$`Patient age` <- "unknown"
    gisaid_template$`Patient status` <- "unknown"
    gisaid_template$`Specimen source`<- "Oropharyngeal swab"
    gisaid_template$`Submitting lab` <- "Ministry of Health Turkey, Public Health Directorate, National Virology Reference Laboratory"
    gisaid_template$Address...26 <- "Sağlık Mahallesi Adnan Saygun Cad. No:55 06430 / Sıhhiye/ Çankaya/ Ankara"
    # 3.4 Ask the user for the location and populate the dataframe accordingly
    location_input <- readline(prompt = "Please specify the location (Ankara, Adana or Erzurum): ")
    if (location_input == "Ankara") {
      gisaid_template$Location <- paste("Europe / Turkey /", cities)
      gisaid_template$`Originating lab` <- "Ministry of Health Türkiye"
      gisaid_template$Address...23 <- "Adnan Saygun Caddesi Sağlık Mahallesi 06430 Ankara Türkiye"
      gisaid_template$Authors <- "Süleyman Yalçın, Yasemin Coşgun, Gültekin Ünal,Ekrem Sağtaş, Sedat Kaygusuz"
      seq_tech_choices <- c("Ilumina MiSeq / Paragon CleanPlex Kit", "Ilumina NextSeq550 / Illumina CovidSeq Kit")
      cat("Choose the sequencing technology:\n")
      for(i in 1:length(seq_tech_choices)) {
        cat(paste(i, ":", seq_tech_choices[i], "\n"))
      }
      seq_tech_input <- as.integer(readline(prompt = "Enter your choice (1 or 2): "))
      if(seq_tech_input %in% 1:2) {
        gisaid_template$`Sequencing technology` <- seq_tech_choices[seq_tech_input]
      } else {
        stop("Invalid choice for sequencing technology")
      }
    }else if (location_input == "Adana") {
      gisaid_template$Location <- "Europe / Turkey / Adana"
      gisaid_template$`Originating lab` <- "Ministry of Health Türkiye, Adana Public Health Laboratory"
      gisaid_template$Address...23 <- "Reşatbey mh. No:51 PK:01120 Seyhan/ADANA"
      gisaid_template$Authors <- "Deniz Pekmezci, Gökhan Karacaoğlan, Okan Bakşi, Süleyman Yalcin, Yasemin Coşgun, Gültekin Ünal, Ekrem Sağtaş, Sedat Kaygusuz "
      gisaid_template$`Sequencing technology` <- "Ilumina MiSeq / Paragon CleanPlex Kit"
    }else if (location_input == "Erzurum") {
      gisaid_template$Location <- "Europe / Turkey / Erzurum"
      gisaid_template$`Originating lab` <- "Ministry of Health Türkiye, Erzurum Public Health Laboratory"
      gisaid_template$Address...23 <- "Murat Paşa Mahallesi, No:23 PK:25100 Yakutiye/Erzurum"
      gisaid_template$Authors <- "NA, Süleyman Yalcin, Yasemin Coşgun, Gültekin Ünal, Ekrem Sağtaş, Sedat Kaygusuz "
      gisaid_template$`Sequencing technology` <- "Ilumina MiSeq / Paragon CleanPlex Kit"
    } else {
      stop("Invalid location input")
    }
  }else {
    stop ("No sequences left after filtering")
  }
  
  #3.5  Add a Row at the Top as the column names and clean it:
  filler_df <- as.data.frame(t(filler_headers))
  colnames(filler_df) <- colnames(gisaid_template)
  gisaid_template <- rbind(filler_df, gisaid_template)
  gisaid_template[1,] <- gsub("\\...\\d+", "", gisaid_template[1,])
  
  #3.6 Revalue the column names
  colnames(gisaid_template) <- original_headers
  tarih<-as.character(excel_data[[date_column_name]])
  gisaid_template[2:nrow(gisaid_template), 6]<-tarih
  
  #Save the updated GISAID template
  if(!dir.exists("ready")) {
    dir.create("ready")
  }
  write_xlsx(gisaid_template, "ready/Updated_GISAID_Template.xls")
  sequences <- lapply(fasta_file, function(x) toupper(as.character(x)))
  names  <- as.list(sapply(fasta_file, function(x) attr(x, "name")))
  write.fasta(sequences = sequences, names = names, file.out = paste0("ready/", fasta_file_name), open = "w", nbchar = 60, as.string = FALSE)
 print("Step 2 Complete! Your GISAID file is ready for upload! Both your fasta file and GISAID file are in the directory ready within the directory prepped!") 
}