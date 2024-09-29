library(shiny)
library(pdftools)
library(tesseract)
library(zip)

# Define the UI
ui <- fluidPage(
  titlePanel("PDF to Text OCR Converter"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("pdfs", "Choose PDF Files", multiple = TRUE, accept = ".pdf"),
      numericInput("dpi", "DPI for OCR (Default is 250):", value = 250),
      textInput("pages", "Pages to OCR (comma-separated):", value = "1,2,3"),
      actionButton("convert", "Convert PDFs"),
      downloadButton("downloadData", "Download Text Files"),
      verbatimTextOutput("status")  # Displays real-time processing status
    ),
    
    mainPanel(
      tableOutput("pdf_list"),          # Displays list of selected PDFs
      verbatimTextOutput("log_output")  # Displays detailed log of the process
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  rv <- reactiveValues(logs = "", output_dir = NULL)  # Store logs and output directory
  
  # Display list of selected PDFs
  output$pdf_list <- renderTable({
    req(input$pdfs)
    input$pdfs$name
  })
  
  # Append log messages and update UI
  log_message <- function(message) {
    rv$logs <- paste(rv$logs, message, sep = "\n")
    output$log_output <- renderText(rv$logs)
  }
  
  # Process PDFs when the "Convert" button is clicked
  observeEvent(input$convert, {
    req(input$pdfs)
    
    pages <- as.numeric(unlist(strsplit(input$pages, ",")))
    dpi <- input$dpi
    pdf_files <- input$pdfs$datapath
    total_pdfs <- length(pdf_files)
    
    # Create a directory for output inside a temporary directory
    rv$output_dir <- file.path(tempdir(), "Text_Output_Files")
    dir.create(rv$output_dir, showWarnings = FALSE)
    
    log_message(paste("Processing", total_pdfs, "PDFs..."))
    
    # Show progress bar
    withProgress(message = "Processing PDFs", value = 0, {
      for (i in seq_along(pdf_files)) {
        file_path <- pdf_files[i]
        file_name <- input$pdfs$name[i]
        log_message(paste("Processing:", file_name))
        
        tryCatch({
          pdf_text <- pdf_text(file_path)
          if (nchar(pdf_text[1]) == 0) {
            log_message(paste("Performing OCR on:", file_name))
            pdf_text <- pdf_ocr_text(file_path, dpi = dpi, language = "eng", pages = pages)
          }
          
          # Save the text output to a file inside the created directory
          text_output_file <- file.path(rv$output_dir, paste0(file_name, ".txt"))
          writeLines(pdf_text, text_output_file)
          log_message(paste("Saved text to:", text_output_file))
          
        }, error = function(e) {
          log_message(paste("Error processing", file_name, ":", e$message))
        })
        
        # Update progress bar and status
        incProgress(1 / total_pdfs, detail = paste(i, "/", total_pdfs, "files processed"))
        output$status <- renderText(paste(i, "/", total_pdfs, "files processed"))
      }
    })
    
    log_message(paste("All files processed. Output stored in:", rv$output_dir))
  })
  
  # Provide a download link for the processed text files
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("text_output_files_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Zip the output directory
      zip::zip(zipfile = file, files = list.files(rv$output_dir, full.names = TRUE))
    },
    contentType = "application/zip"
  )
}

# Run the app
shinyApp(ui, server)
