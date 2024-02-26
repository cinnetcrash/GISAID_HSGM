# Function to replace Turkish characters with non-diacritic equivalents
replace_turkish_chars <- function(string) {
  string <- gsub("Ü", "U", string, ignore.case = TRUE)
  string <- gsub("Ş", "S", string, ignore.case = TRUE)
  string <- gsub("İ", "I", string, ignore.case = TRUE)
  string <- gsub("Ö", "O", string, ignore.case = TRUE)
  string <- gsub("Ç", "C", string, ignore.case = TRUE)
  string <- gsub("Ğ", "G", string, ignore.case = TRUE)
  return(string)
}
