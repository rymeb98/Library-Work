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
  
  # Read uploaded image
  image_obj <- reactive({
    req(input$image)
    image_read(input$image$datapath)
  })
  
  # Show preview
  output$preview <- renderImage({
    req(image_obj())
    list(
      src = input$image$datapath,
      contentType = input$image$type,
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  # Process image + OCR (button-controlled)
  extracted_text <- eventReactive(input$process, {
    req(image_obj())
    
    img <- image_obj() |>
      image_orient() |>
      image_convert(colorspace = "gray") |>
      image_resize("2000x") |>
      image_enhance() |>
      image_blur(1) |>
      image_contrast(sharpen = 1)
    
    ocr(img, engine = tesseract("eng"))
  })
  
  # Display extracted text
  output$text <- renderText({
    req(extracted_text())
    extracted_text()
  })
  
  # Convert text into table
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
