read_fasta_file <- function(fasta_file_path) {
  fasta_lines <- readLines(fasta_file_path)
  
  # Check if the FASTA file is in the expected format (header followed by single-line sequence)
  is_standard_format <- all(startsWith(fasta_lines[seq(1, length(fasta_lines), by=2)], ">"))
  
  if (is_standard_format) {
    fasta_headers <- fasta_lines[seq(1, length(fasta_lines), by=2)]
    fasta_sequences <- fasta_lines[seq(2, length(fasta_lines), by=2)]
  } else {
    # Your provided code for handling non-standard FASTA format
    fasta_headers <- c()
    fasta_sequences <- c()
    concatenated_sequence <- ""
    
    for (i in 1:length(fasta_lines)) {
      line <- fasta_lines[i]
      
      if (startsWith(line, ">")) {
        if (concatenated_sequence != "") {
          fasta_sequences <- c(fasta_sequences, concatenated_sequence)
          concatenated_sequence <- ""
        }
        fasta_headers <- c(fasta_headers, line)
      } else {
        concatenated_sequence <- paste0(concatenated_sequence, gsub("\\s+", "", line))
      }
    }
    fasta_sequences <- c(fasta_sequences, concatenated_sequence)
    
    if (length(fasta_headers) != length(fasta_sequences)) {
      stop("Mismatch between number of headers and sequences in the FASTA file.")
    }
  }
  
  # Extract Identifiers from fasta file headers
  fasta_Identifiers <- str_extract(fasta_headers, "(?<=^>)[^.|]+")
  fasta_Identifiers <- sapply(strsplit(fasta_Identifiers, "_"), function(x) tail(x, 1))
  
  return(list(headers = fasta_headers, sequences = fasta_sequences, identifiers = fasta_Identifiers))
}
