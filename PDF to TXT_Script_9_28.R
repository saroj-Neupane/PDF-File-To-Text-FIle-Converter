library(shiny)
library("pdftools")
library("tesseract")
pages = c(1,2,3)
dpi=250

main <- function() {
# Ask user for PDF Folder path
folder <- readline(prompt = "Enter the PDF folder path: ")
pdf_files <- list.files(folder, pattern = "\\.pdf$", full.names = TRUE)

#Define required folders
text_output_folder <- file.path(folder, 'Text Output Files')
dir.create(text_output_folder, recursive = FALSE, showWarnings = FALSE)
n=0
for (file in pdf_files) {
  n=n+1
  tryCatch( 
  {
  pdf_text <- pdf_text(file)
  
  if (nchar(pdf_text[1]) == 0) {
    if (pdf_info(file)$pages <= length(pages)) {
      pages = pdf_info(file)$pages
      cat("\n", basename(file),"contains only ", pages, "pages\n")
    }
    pdf_text <- pdf_ocr_text(file, dpi = dpi, language = "eng", pages= pages)
  }
  
  text_output_file <- file.path(text_output_folder, paste0(basename(file), '.txt') )
  
  writeLines(pdf_text, text_output_file)
},
error = function(e) {
  message("Error occurred while processing ", basename(file), ": ", e)
}

)
  cat(sprintf("\r Processed %d out of %d PDFs",n,length(pdf_files)))
  
}
}

main()
