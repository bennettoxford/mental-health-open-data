library(stringr)

str_extract_snomed <- function(string) {
  pattern <- "\\b[1-9][0-9]{5,17}\\b"

  snomed_codes <- str_extract_all(string, pattern) 
  snomed_codes
}

str_extract_icd <- function(string) {
  pattern <- "\\b[A-Z][0-9]{1,3}\\b" 

  icd_codes <- str_extract_all(string, pattern)  
  icd_codes
}
