library(shiny)
library(magick)
library(tesseract)
library(DT)
library(dplyr)
library(stringr)
library(tidyr)

#Image Pre-procesing
preprocess_image <- function(path) {
  image_read(path) |>
    image_orient() |>
    image_convert(colorspace = "gray") |>
    image_resize("3000x") |>
    image_enhance() |>
    image_contrast(sharpen = 1)
}


#OCR for tables
ocr_table <- function(img) {
  
  engine <- tesseract(
    "eng",
    options = list(
      tessedit_pageseg_mode = 6,   # uniform block of text (best for tables)
      preserve_interword_spaces = 1
    )
  )
  
  ocr(img, engine = engine)
}


#Convert OCR text into a table
text_to_table <- function(text, min_spaces = 2) {
  
  lines <- strsplit(text, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  
  rows <- lapply(lines, function(line) {
    str_split(line, paste0("\\s{", min_spaces, ",}"))[[1]]
  })
  
  max_cols <- max(lengths(rows))
  
  rows <- lapply(rows, function(r) {
    length(r) <- max_cols
    r
  })
  
  as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
}

# UI --------------------
ui <- fluidPage(
  
  titlePanel("Table OCR → Digital Data"),
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput(
        "image",
        "Upload Table Image",
        accept = c("image/png", "image/jpeg")
      ),
      actionButton("process", "Extract Table"),
      br(), br(),
      downloadButton("download", "Download CSV")
    ),
    
    mainPanel(
      h4("Image Preview"),
      imageOutput("preview", height = "300px"),
      
      h4("Extracted Table"),
      DTOutput("table")
    )
  )
)

# Server --------------------------------
server <- function(input, output, session) {
  
  # Image preview
  output$preview <- renderImage({
    req(input$image)
    list(
      src = input$image$datapath,
      contentType = input$image$type,
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  # Table extraction
  extracted_table <- eventReactive(input$process, {
    req(input$image)
    
    img <- preprocess_image(input$image$datapath)
    text <- ocr_table(img)
    
    text_to_table(text)
  })
  
  # Render table
  output$table <- renderDT({
    req(extracted_table())
    DT::datatable(extracted_table(), editable = TRUE)
  })
  
  # Download CSV
  output$download <- downloadHandler(
    filename = function() "extracted_table.csv",
    content = function(file) {
      write.csv(extracted_table(), file, row.names = FALSE)
    }
  )
}

# Run App ----------------------------------
shinyApp(ui, server)
