# GISAID_HSGM
These codes are to generate an automated code for uploading SARS-CoV-2 genomic sequences to GISAID via a FASTQ file and an EXCEL file for Türkiye

USER MANUAL

A) Setting Up Your Environment in RStudio

1- Launching RStudio: Open RStudio to begin your session.

2- Preparing Functions: To source the required functions in R, click the "Source" button at the top-right side     of the RStudio interface. This will source the currently open script, including the functions read_fasta_file" and "replace_turkish_chars."
Ensure the script file containing these functions is open in the RStudio editor before sourcing.

3- Organizing Files:
Create a folder to contain the multi-FASTA file with the sequences you intend to upload and the related Excel file. This step is crucial for maintaining an efficient workflow.

4- Setting the Working Directory:
Navigate through the RStudio menu: Session -> Set Working Directory -> Choose Directory... Select the folder you organized in step 3 as your working directory. Setting the working directory is essential for the R scripts to access the necessary files.

5- Adjusting the GISAID Template Preparation Script:
For those utilizing a GISAID template, modify lines 21 and 23 of the gisaid_template_prep_func code to match the file path where your GISAID template file is located. Ensuring the file path is accurate for the script to function properly is critical.

B) Processing Your Data

1- Running the First Script:
Execute the "gisaid_fasta_prep.R" script by clicking the "Source" button while the script is open. This will source the function. Afterward, in the R command line, run the newly sourced function. Upon running the function, it will generate a new folder titled "prepped" containing the processed data ready for the next steps.

2- Accessing the 'Prepped' Folder:
Navigate to the prepped folder within RStudio by setting it as your working directory using the method outlined in step A4.

3- Running the GISAID Template Preparation Function:
Run the "gisaid_template_prep_func" script inside the prepped folder to finalize the data preparation for GISAID submission.

4- Final Steps:
After running the script, a ready file will appear in the prepped folder, signifying that your FASTA and Excel files are prepared for GISAID upload.
