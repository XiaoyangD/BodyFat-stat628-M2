library(shiny)
library(ggplot2)
library(shinyjs)
library(base64enc)

# Create a function for the body fat calculation
calculate_body_fat <- function(age, weight, height, neck, waist) {
  coef_age = 0.01197798
  coef_weight = 0.04021063
  coef_height = -0.30521452
  coef_neck = -0.49662496
  coef_waist = 0.80913985 
  intercept = -8.88390845
  
  body_fat = coef_age * age + coef_weight * weight +
    coef_height * height + coef_neck * neck +
    coef_waist * waist + intercept
  
  return(max(0, body_fat))
}

# Read the background image file and encode it to base64
background_image <- readBin("image.png", "raw", file.info("image.png")$size)
background_image_base64 <- base64enc::dataURI(background_image, mime = "image/jpeg")

# Using ChatGPT helped me insert pictures into the interface
# Using img() function
welcome_ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        paste0(
          "
          .center-title {
            text-align: center;
          }
          .center-button {
            text-align: center;
          }
          .large-button {
            font-size: 20px;
          }
          html, body {
            height: 100%;
            margin: 0;  
          }
          .background-image {  
            position: absolute;  
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            z-index: -1;  
          }
          "
        )
      )
    )
  ),
  div(class = "background-image",
      img(src = background_image_base64, alt = "Background Image",
          style = "width: 100%; height: 100%;"),  
  ),
  div(class = "center-title",
      titlePanel("Welcome to Body Fat Calculator")
  ),
  div(class = "center-button",
      actionButton("start", "Start", class = "large-button")
  )
)


# Create a UI for the main application page without a background image
app_ui <- fluidPage(
  titlePanel("Body Fat Calculator"),
  tabsetPanel(
    tabPanel("Understanding Body Fat Percentages",
             h4("Understanding Body Fat Percentages:"),
             p("Body fat percentages can vary widely, but generally:"),
             tags$ul(
               tags$li("Essential fat: 2-5%"),
               tags$li("Athletes: 6-13%"),
               tags$li("Fitness: 14-24%"),
               tags$li("Average: 25-31%"),
               tags$li("Obese: 32% and higher")
             )
    ),
    tabPanel("Calculator", 
             sidebarLayout(
               sidebarPanel(
                 numericInput("age", "Age:", 30, min = 18, max = 100),
                 numericInput("weight", "Weight (lbs):", 150, min = 90, max = 400),
                 numericInput("height", "Height (inches):", 68, min = 48, max = 84),
                 numericInput("neck", "Neck Circumference (cm):", 35, min = 30.48, max = 50.8),
                 numericInput("waist", "Waist Circumference (cm):", 80, min = 60.96, max = 152.4),
                 actionButton("calculate", "Calculate"),
                 actionButton("saveRecord", "Save Record")
               ),
               mainPanel(
                 h3("Estimated Body Fat:"),
                 verbatimTextOutput("result", placeholder = TRUE),
                 tags$hr(),
                 plotOutput("fatProjection"),
                 h4("Limitations:"),
                 p("This tool provides a rough estimate and should not substitute for professional medical advice.")
               )
             )
    ),
    tabPanel("History", 
             verbatimTextOutput("historyText"),
             actionButton("clearHistory", "Clear History")
    ),
    tabPanel("Contact Us",
             h4("Contact Information:"),
             p("For any queries or feedback, please contact:"),
             p("Email:"),
             p("Osama Kheshaifaty at kheshaifaty@wisc.edu "),
             p("CHUYI LIN at clin395@wisc.edu;"),
             p("XIAOYANG DONG at xdong88@wisc.edu")
    )
  )
)

# Server Component
server <- function(input, output, session) {
  observe({
    if (!is.null(input$start) && input$start > 0) {
      shinyjs::disable("start")
      shinyjs::show("app_page")
      shinyjs::hide("welcome_page")
    }
  })
  
  
  observeEvent(input$calculate, {
    warning_msg <- ""
    if (!is.numeric(input$age) || input$age < 18 || input$age > 100) {
      warning_msg <- paste(warning_msg, "Age should be between 18 and 100.\n")
    }
    if (!is.numeric(input$weight) || input$weight < 90 || input$weight > 400) {
      warning_msg <- paste(warning_msg, "Weight should be between 90 and 400 lbs.\n")
    }
    if (!is.numeric(input$height) || input$height < 48 || input$height > 84) {
      warning_msg <- paste(warning_msg, "Height should be between 48 and 84 inches.\n")
    }
    if (!is.numeric(input$neck) || input$neck < 30.48 || input$neck > 50.8) {
      warning_msg <- paste(warning_msg, "Neck circumference should be between 30.48 and 50.8 cm.\n")
    }
    if (!is.numeric(input$waist) || input$waist < 60.96 || input$waist > 152.4) {
      warning_msg <- paste(warning_msg, "Waist circumference should be between 60.96 and 152.4 cm.\n")
    }
    if (!is.numeric(input$age) || !is.numeric(input$weight) || !is.numeric(input$height) || !is.numeric(input$neck) || !is.numeric(input$waist)) {
      warning_msg <- paste(warning_msg, "Please enter numeric values for all input fields.")
    }
    if (anyNA(input)) {
      warning_msg <- paste(warning_msg, "Please fill in all input fields.")
    }
    
    if (warning_msg != "") {
      output$result <- renderText({
        warning_msg
      })
    } else {
      body_fat <- calculate_body_fat(input$age, input$weight, input$height, input$neck, input$waist)
      output$result <- renderText({
        paste("Your estimated body fat percentage is:", round(body_fat, 2), "%")
      })
    }
    
    output$fatProjection <- renderPlot({
      result <- tryCatch({
        new_weights <- seq(input$weight - 30, input$weight + 30, by = 1)
        projected_fat <- sapply(new_weights, function(w) calculate_body_fat(input$age, w, input$height, input$neck, input$waist))
        projection_df <- data.frame(Weight = new_weights, Projected_Body_Fat = projected_fat)
        
        p <- ggplot(projection_df, aes(x = Weight, y = Projected_Body_Fat)) +
          geom_line() +
          geom_point(aes(x = input$weight, y = body_fat), color = "red") +
          annotate("text", x = input$weight, y = body_fat, label = "You're here", vjust = -1, hjust = -0.5) +
          xlab("Weight (lbs)") +
          ylab("Projected Body Fat (%)") +
          ggtitle("Projected Body Fat vs. Weight Change") +
          theme_minimal()
      }, error = function(e) {
        showNotification("Unable to generate the plot. Please check your input values.", type = "warning")
        return(NULL)
      })
      
      return(result)
    })
  })
  
  savedRecords <- reactiveVal(list())  
  
  observeEvent(input$saveRecord, {
    # Create a new record with the current input and result
    newRecord <- list(
      Age = input$age,
      Weight = input$weight,
      Height = input$height,
      Neck = input$neck,
      Waist = input$waist,
      BodyFat = calculate_body_fat(input$age, input$weight, input$height, input$neck, input$waist)
    )
    # Add the new record to the history
    currentRecords <- savedRecords()
    currentRecords <- c(currentRecords, list(newRecord))
    savedRecords(currentRecords)
  })
  
  output$historyText <- renderText({
    history <- savedRecords()
    if (length(history) == 0) {
      return("No history records available.")
    } else {
      
      formatted_history <- sapply(history, function(record) {
        paste("Age:", record$Age, "lbs:", record$Weight, "Height:", record$Height, "Neck:", record$Neck, "Waist:", record$Waist, "Body Fat:", record$BodyFat, "%")
      })
      return(paste(formatted_history, collapse = "\n"))
    }
  })
  
  observeEvent(input$clearHistory, {
    # Clean History
    savedRecords(NULL)
  })
}

# Create total Shiny app
shinyApp(
  ui = fluidPage(
    useShinyjs(),
    div(id = "welcome_page", welcome_ui),
    div(id = "app_page", app_ui, style = "display: none;")
  ),
  server = server
)
