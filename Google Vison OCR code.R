# A. Enable Google Vision API

# > Go to Google Cloud Console

# > Create or select a project

# > Enable Vision API

# > Create an API key

Sys.setenv(GOOGLE_VISION_KEY = "YOUR_API_KEY_HERE")

install.packages(c("httr", "jsonlite", "base64enc"))

install.packages(c(
  "shiny",
  "tesseract",
  "magick",
  "DT",
  "dplyr"
))

library(shiny)
library(tesseract)
library(magick)
library(DT)
library(dplyr)

library(httr)
library(jsonlite)
library(base64enc)

# OCR System
google_vision_ocr <- function(image_path) {
  
  api_key <- Sys.getenv("GOOGLE_VISION_KEY")
  if (api_key == "") stop("Google Vision API key not set")
  
  img_base64 <- base64enc::base64encode(image_path)
  
  body <- list(
    requests = list(
      list(
        image = list(content = img_base64),
        features = list(
          list(type = "DOCUMENT_TEXT_DETECTION")
        )
      )
    )
  )
  
  res <- POST(
    url = paste0(
      "https://vision.googleapis.com/v1/images:annotate?key=",
      api_key
    ),
    body = toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )
  
  parsed <- content(res, as = "parsed", simplifyVector = TRUE)
  
  parsed$responses[[1]]$fullTextAnnotation$text
}

# ---- UI ----
ui <- fluidPage(
  
  titlePanel("Image-to-Data Shiny App"),
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput(
        "image",
        "Upload Photograph",
        accept = c("image/png", "image/jpeg", "image/pdf")
      ),
      
      radioButtons(
        "ocr_engine",
        "OCR Engine",
        choices = c(
          "Standard (Printed Text)" = "tesseract",
          "Handwriting (Google Vision)" = "google"
        ),
        selected = "tesseract"
      ),
      
      actionButton("process", "Extract Data")
    ),
    
    mainPanel(
      h4("Uploaded Image"),
      imageOutput("preview", height = "300px"),
      
      h4("Extracted Text"),
      verbatimTextOutput("text"),
      
      h4("Extracted Data Table"),
      DTOutput("table")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  image_obj <- reactive({
    req(input$image)
    image_read(input$image$datapath)
  })
  
  output$preview <- renderImage({
    req(image_obj())
    list(
      src = input$image$datapath,
      contentType = input$image$type,
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  extracted_text <- eventReactive(input$process, {
    req(input$image)
    
    if (input$ocr_engine == "google") {
      
      # --- Google Vision OCR (best for handwriting) ---
      google_vision_ocr(input$image$datapath)
      
    } else {
      
      # --- Tesseract OCR (printed text) ---
      img <- image_obj() |>
        image_orient() |>
        image_convert(colorspace = "gray") |>
        image_resize("2000x") |>
        image_enhance() |>
        image_contrast(sharpen = 1)
      
      ocr(img, engine = tesseract("eng"))
    }
  })
  
  output$text <- renderText({
    req(extracted_text())
    extracted_text()
  })
  
  output$table <- renderDT({
    req(extracted_text())
    
    lines <- strsplit(extracted_text(), "\n")[[1]]
    
    tibble(
      line_number = seq_along(lines),
      text = lines
    ) |>
      filter(text != "") |>
      datatable()
  })
}

# ---- RUN APP ----
shinyApp(ui, server)
