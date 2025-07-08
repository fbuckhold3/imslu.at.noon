# server.R - Conference Attendance Tracking App

library(shinyjs)

server <- function(input, output, session) {
  
  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================
  
  values <- reactiveValues(
    current_step = "access",
    participant = NULL,
    error_message = NULL,
    time_check = NULL,
    last_cleaned_code = ""
  )
  
  # ============================================================================
  # TIME WINDOW CHECK FUNCTION
  # ============================================================================
  
  is_conference_time <- function() {
    # Override for testing
    if (exists("TESTING_MODE") && TESTING_MODE) {
      return(list(
        allowed = TRUE,
        message = "Testing mode - time restrictions bypassed"
      ))
    }
    
    # Your actual time window logic goes here
    # For now, this is a placeholder - replace with your actual time checking logic
    current_time <- Sys.time()
    chicago_time <- lubridate::with_tz(current_time, CONFERENCE_TIMEZONE)
    
    # Example time window (replace with your actual logic):
    # Conference runs Monday-Friday, 12:00 PM - 1:00 PM Chicago time
    hour <- lubridate::hour(chicago_time)
    weekday <- lubridate::wday(chicago_time, week_start = 1) # Monday = 1
    
    if (weekday >= 1 && weekday <= 5 && hour >= 12 && hour < 13) {
      return(list(
        allowed = TRUE,
        message = "Conference submission window is open."
      ))
    } else {
      return(list(
        allowed = FALSE,
        message = paste0("Conference submission is closed. ",
                         "Submissions are accepted Monday-Friday, 12:00 PM - 1:00 PM Central Time. ",
                         "Please return during the designated time window.")
      ))
    }
  }
  
  # ============================================================================
  # MOBILE COMPATIBILITY & SECURITY FIXES
  # ============================================================================
  
  # Force HTTPS redirect for mobile security
  observe({
    if (Sys.getenv("SHINY_PORT") != "" && 
        !is.null(session$clientData$url_protocol) &&
        !grepl("^https://", session$clientData$url_protocol)) {
      
      # Redirect to HTTPS version
      url_https <- paste0("https://", session$clientData$url_hostname, 
                          session$clientData$url_pathname)
      
      runjs(paste0("window.location.replace('", url_https, "');"))
    }
  })
  
  # Add security headers for mobile compatibility
  session$onSessionEnded(function() {
    session$sendCustomMessage(type = "add_headers", message = list(
      "Strict-Transport-Security" = "max-age=31536000; includeSubDomains",
      "X-Frame-Options" = "SAMEORIGIN",
      "X-Content-Type-Options" = "nosniff"
    ))
  })
  
  # ============================================================================
  # TIME WINDOW OBSERVERS
  # ============================================================================
  
  # Check time window on app load and periodically
  observe({
    values$time_check <- is_conference_time()
    
    # If outside time window, show restriction message
    if (!values$time_check$allowed) {
      values$current_step <- "time_restricted"
    }
  })
  
  # Periodic time check (every 30 seconds)
  observe({
    invalidateLater(30000, session)  # 30 seconds
    values$time_check <- is_conference_time()
    
    # If time window opens, allow access
    if (values$time_check$allowed && values$current_step == "time_restricted") {
      values$current_step <- "access"
    }
    # If time window closes, restrict access
    if (!values$time_check$allowed && values$current_step != "time_restricted") {
      values$current_step <- "time_restricted"
    }
  })
  
  # ============================================================================
  # STEP VISIBILITY CONTROLS
  # ============================================================================
  
  output$show_time_restriction <- reactive({
    values$current_step == "time_restricted"
  })
  outputOptions(output, "show_time_restriction", suspendWhenHidden = FALSE)
  
  output$show_access_step <- reactive({
    values$current_step == "access"
  })
  outputOptions(output, "show_access_step", suspendWhenHidden = FALSE)
  
  output$show_question_step <- reactive({
    values$current_step == "question"
  })
  outputOptions(output, "show_question_step", suspendWhenHidden = FALSE)
  
  output$show_success_step <- reactive({
    values$current_step == "success"
  })
  outputOptions(output, "show_success_step", suspendWhenHidden = FALSE)
  
  output$show_error <- reactive({
    !is.null(values$error_message)
  })
  outputOptions(output, "show_error", suspendWhenHidden = FALSE)
  
  # ============================================================================
  # TIME RESTRICTION OUTPUTS
  # ============================================================================
  
  output$time_restriction_message <- renderText({
    if (!is.null(values$time_check)) {
      return(values$time_check$message)
    }
    return("")
  })
  
  output$current_time_display <- renderText({
    # Update every 30 seconds
    invalidateLater(30000, session)
    
    # Get current time and convert to Chicago timezone
    current_time <- Sys.time()
    chicago_time <- lubridate::with_tz(current_time, CONFERENCE_TIMEZONE)
    
    # Format in Chicago timezone
    formatted_time <- format(chicago_time, "%I:%M %p %Z on %A, %B %d, %Y")
    
    # Debug output
    cat("Display time - UTC:", format(current_time, "%Y-%m-%d %H:%M:%S %Z"), "\n")
    cat("Display time - Chicago:", format(chicago_time, "%Y-%m-%d %H:%M:%S %Z"), "\n")
    cat("Formatted for display:", formatted_time, "\n")
    
    return(formatted_time)
  })
  
  # ============================================================================
  # MOBILE-OPTIMIZED ACCESS CODE HANDLING
  # ============================================================================
  
  # Clean input for mobile compatibility (prevents infinite loop)
  observeEvent(input$access_code, {
    if (!is.null(input$access_code) && 
        input$access_code != values$last_cleaned_code &&
        nchar(input$access_code) > 0) {
      
      # Clean the input by removing whitespace only (preserve case for case-sensitive codes)
      cleaned_code <- trimws(input$access_code)
      
      # Only update if the cleaned code is different from current input
      if (cleaned_code != input$access_code) {
        values$last_cleaned_code <- cleaned_code
        updateTextInput(session, "access_code", value = cleaned_code)
      }
    }
  })
  
  # Clear error when user starts typing
  observeEvent(input$access_code, {
    if (!is.null(input$access_code) && nchar(input$access_code) > 0) {
      values$error_message <- NULL
    }
  })
  
  # Access code submission with mobile error handling
  observeEvent(input$submit_access, {
    req(input$access_code)
    
    # First check if we're in the time window
    time_check <- is_conference_time()
    if (!time_check$allowed) {
      values$error_message <- "Conference submission window is currently closed. Please try again during the designated time."
      return()
    }
    
    values$error_message <- NULL
    
    # Add comprehensive error handling for mobile connectivity issues
    tryCatch({
      # Find participant by access code
      if (!is.null(resident_data) && nrow(resident_data) > 0) {
        participant <- resident_data[resident_data$access_code == input$access_code, ]
        
        if (nrow(participant) > 0) {
          values$participant <- participant[1, ]  # Take first match
          values$current_step <- "question"
          
          # Clear the access code input and reset tracking
          updateTextInput(session, "access_code", value = "")
          values$last_cleaned_code <- ""
          
          # Populate rotation choices when step becomes visible
          updateSelectizeInput(
            session, 
            "q_rotation",
            choices = c(
              "Red" = "1",
              "Green" = "2", 
              "White" = "3",
              "Yellow" = "4",
              "Diamond" = "5",
              "Gold" = "6",
              "MICU" = "7",
              "Bronze" = "8",
              "Cardiology" = "9",
              "Bridge / Acute Care" = "10",
              "Consults - SLUH" = "11",
              "Elective / Clinics CSM" = "12",
              "VA A" = "13",
              "VA B" = "14",
              "VA C" = "15",
              "VA D" = "16",
              "VA Clinics or Consults" = "17"
            ),
            selected = character(0)
          )
          
        } else {
          values$error_message <- "Invalid access code. Please check your code and try again."
        }
      } else {
        values$error_message <- "Unable to validate access code. Please try again later."
      }
    }, error = function(e) {
      values$error_message <- "Connection error. Please check your internet connection and try again."
      cat("Error in access code validation:", e$message, "\n")
    })
  })
  
  # ============================================================================
  # PARTICIPANT INFO DISPLAY
  # ============================================================================
  
  output$participant_name <- renderText({
    if (!is.null(values$participant)) {
      # Use 'name' field or construct from first_name/last_name
      if ("name" %in% names(values$participant) && !is.na(values$participant$name)) {
        return(values$participant$name)
      } else if ("first_name" %in% names(values$participant) && "last_name" %in% names(values$participant)) {
        return(paste(values$participant$first_name, values$participant$last_name))
      } else {
        return("Conference Participant")
      }
    }
    return("")
  })
  
  # ============================================================================
  # RESPONSE SUBMISSION WITH MOBILE ERROR HANDLING
  # ============================================================================
  
  observeEvent(input$submit_response, {
    req(values$participant)
    req(input$q_rotation)
    req(input$q_answer)
    
    # Double-check time window before submission
    time_check <- is_conference_time()
    if (!time_check$allowed) {
      values$error_message <- "Conference submission window has closed. Your response could not be submitted."
      return()
    }
    
    values$error_message <- NULL
    
    # Add comprehensive error handling for mobile connectivity
    tryCatch({
      # Submit to REDCap
      success <- submit_question_response(
        record_id = values$participant$record_id,
        rotation = input$q_rotation,
        answer = input$q_answer
      )
      
      if (success) {
        values$current_step <- "success"
        # Reset form values
        updateSelectizeInput(session, "q_rotation", selected = character(0))
        updateRadioButtons(session, "q_answer", selected = character(0))
      } else {
        values$error_message <- "Failed to submit response. Please try again."
      }
    }, error = function(e) {
      values$error_message <- "Network error during submission. Please check your connection and try again."
      cat("Error in response submission:", e$message, "\n")
    })
  })
  
  # ============================================================================
  # ERROR HANDLING
  # ============================================================================
  
  output$error_message <- renderText({
    values$error_message
  })
  
  # ============================================================================
  # START OVER FUNCTIONALITY
  # ============================================================================
  
  observeEvent(input$start_over, {
    # Reset all values
    values$current_step <- "access"
    values$participant <- NULL
    values$error_message <- NULL
    values$last_cleaned_code <- ""
    
    # Reset all inputs
    updateTextInput(session, "access_code", value = "")
    updateSelectizeInput(session, "q_rotation", selected = character(0))
    updateRadioButtons(session, "q_answer", selected = character(0))
  })
}