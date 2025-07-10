# server.R - Conference Attendance Tracking App

server <- function(input, output, session) {
  
  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================
  
  values <- reactiveValues(
    current_step = "access",
    participant = NULL,
    error_message = NULL,
    time_check = NULL
  )
  
  # Function to clean access code input (add this to your global.R or at top of server)
  clean_access_code <- function(code) {
    if (is.null(code) || is.na(code) || nchar(code) == 0) {
      return("")
    }
    
    # Step 1: Basic trimming
    cleaned <- trimws(code)
    
    # Step 2: Remove common invisible characters that mobile keyboards insert
    # Remove non-breaking spaces, zero-width spaces, etc.
    cleaned <- gsub("[\u00A0\u2000-\u200F\u2028-\u202F\u205F-\u206F\uFEFF]", "", cleaned)
    
    # Step 3: Normalize Unicode (handles different types of similar characters)
    # This handles cases where mobile keyboards insert different Unicode versions
    if (requireNamespace("stringi", quietly = TRUE)) {
      cleaned <- stringi::stri_trans_nfc(cleaned)  # Normalize to composed form
    }
    
    # Step 4: Remove any remaining non-alphanumeric characters
    # Only keep letters and numbers (adjust if your codes have other valid characters)
    cleaned <- gsub("[^A-Za-z0-9]", "", cleaned)
    
    return(cleaned)
  }
  
  # Function to find participant with flexible matching
  find_participant <- function(input_code, data) {
    if (is.null(data) || nrow(data) == 0 || is.null(input_code) || nchar(input_code) == 0) {
      return(NULL)
    }
    
    # Clean the input code
    cleaned_input <- clean_access_code(input_code)
    
    # Try exact match first (case-sensitive)
    exact_match <- data[data$access_code == cleaned_input, ]
    if (nrow(exact_match) > 0) {
      return(exact_match[1, ])
    }
    
    # Try case-insensitive match
    case_insensitive_match <- data[toupper(data$access_code) == toupper(cleaned_input), ]
    if (nrow(case_insensitive_match) > 0) {
      return(case_insensitive_match[1, ])
    }
    
    # If still no match, try cleaning all stored codes and comparing
    for (i in seq_len(nrow(data))) {
      if (toupper(clean_access_code(data$access_code[i])) == toupper(cleaned_input)) {
        return(data[i, ])
      }
    }
    
    return(NULL)
  }
  
  # ============================================================================
  # MOBILE COMPATIBILITY - HTTPS REDIRECT ONLY
  # ============================================================================
  
  # Simple HTTPS redirect for mobile security (non-disruptive)
  observe({
    if (!is.null(session$clientData$url_protocol) && 
        session$clientData$url_protocol == "http:" &&
        !grepl("localhost|127.0.0.1", session$clientData$url_hostname)) {
      
      url_https <- paste0("https://", session$clientData$url_hostname, 
                          session$clientData$url_pathname)
      runjs(paste0("window.location.replace('", url_https, "');"))
    }
  })
  
  # ============================================================================
  # TIME WINDOW CHECK
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
  # ACCESS CODE HANDLING WITH MOBILE IMPROVEMENTS
  # ============================================================================
  
  # Simple input cleaning for mobile (just trim whitespace, preserve case)
  observeEvent(input$access_code, {
    if (!is.null(input$access_code) && nchar(input$access_code) > 0) {
      # Clear errors when typing
      values$error_message <- NULL
      
      # Clean the input
      cleaned <- clean_access_code(input$access_code)
      
      # Only update if cleaning changed something and result is not empty
      if (cleaned != input$access_code && nchar(cleaned) > 0) {
        updateTextInput(session, "access_code", value = cleaned)
      }
      
      # Debug output for troubleshooting
      if (cleaned != input$access_code) {
        cat("Access code cleaned:\n")
        cat("  Original: '", input$access_code, "' (", nchar(input$access_code), " chars)\n")
        cat("  Cleaned:  '", cleaned, "' (", nchar(cleaned), " chars)\n")
        cat("  Raw bytes original:", paste(charToRaw(input$access_code), collapse = " "), "\n")
        cat("  Raw bytes cleaned: ", paste(charToRaw(cleaned), collapse = " "), "\n")
      }
    }
  })
  
  observeEvent(input$submit_access, {
    req(input$access_code)
    
    # First check if we're in the time window
    time_check <- is_conference_time()
    if (!time_check$allowed) {
      values$error_message <- "Conference submission window is currently closed. Please try again during the designated time."
      return()
    }
    
    values$error_message <- NULL
    
    # Find participant using robust matching
    participant <- find_participant(input$access_code, resident_data)
    
    if (!is.null(participant)) {
      values$participant <- participant
      values$current_step <- "question"
      
      # Clear the access code input
      updateTextInput(session, "access_code", value = "")
      
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
      
      # Log successful access for debugging
      cat("Access granted for code:", input$access_code, "\n")
      
    } else {
      values$error_message <- "Invalid access code. Please check your code and try again."
      
      # Debug logging for failed attempts
      cat("Access denied for code:", input$access_code, "\n")
      cat("  Cleaned version:", clean_access_code(input$access_code), "\n")
      cat("  Available codes:", paste(head(resident_data$access_code, 5), collapse = ", "), "...\n")
    }
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
  # RESPONSE SUBMISSION
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
    
    # Reset all inputs
    updateTextInput(session, "access_code", value = "")
    updateSelectizeInput(session, "q_rotation", selected = character(0))
    updateRadioButtons(session, "q_answer", selected = character(0))
  })
}