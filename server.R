# server.R - Conference Attendance Tracking App

server <- function(input, output, session) {
  
  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================
  
  values <- reactiveValues(
    current_step = "access",
    participant = NULL,
    error_message = NULL,
    time_check = NULL,
    current_conference = NULL
  )
  
  # ============================================================================
  # MOBILE INPUT CLEANING FUNCTIONS
  # ============================================================================
  
  # Function to clean access code input
  clean_access_code <- function(code) {
    if (is.null(code) || is.na(code) || nchar(code) == 0) {
      return("")
    }
    
    # Step 1: Convert to character and basic trimming
    cleaned <- as.character(code)
    cleaned <- trimws(cleaned)
    
    # Step 2: Remove iOS-specific invisible characters
    cleaned <- gsub("[\u00A0\u2000-\u200F\u2028-\u202F\u205F-\u206F\uFEFF\u200B-\u200D\u2060\uFEFF]", "", cleaned)
    
    # Step 3: Remove iOS auto-correction artifacts
    cleaned <- gsub("[\u2018\u2019\u201C\u201D\u2013\u2014]", "", cleaned)
    
    # Step 4: Normalize Unicode
    if (requireNamespace("stringi", quietly = TRUE)) {
      cleaned <- stringi::stri_trans_nfc(cleaned)
      cleaned <- stringi::stri_trans_general(cleaned, "Any-Latin; Latin-ASCII")
    }
    
    # Step 5: Remove any remaining non-alphanumeric characters (PRESERVING CASE)
    cleaned <- gsub("[^A-Za-z0-9]", "", cleaned)
    
    return(cleaned)
  }
  
  # Function to find participant with flexible matching
  # Replace your find_participant function with this corrected version:
  
  find_participant <- function(input_code, data) {
    if (is.null(data) || nrow(data) == 0 || is.null(input_code) || nchar(input_code) == 0) {
      cat("find_participant: Invalid input data\n")
      return(NULL)
    }
    
    # Clean the input code
    cleaned_input <- clean_access_code(input_code)
    cat("find_participant: Searching for cleaned code '", cleaned_input, "'\n")
    
    # Clean all stored codes for comparison (UNCOMMENTED)
    data$access_code_cleaned <- sapply(data$access_code, clean_access_code)
    
    # Try exact match with cleaned codes
    exact_match <- data[data$access_code_cleaned == cleaned_input, ]
    if (nrow(exact_match) > 0) {
      cat("find_participant: Found exact match\n")
      return(exact_match[1, ])
    }
    
    # Try case-insensitive match as backup
    case_insensitive_match <- data[toupper(data$access_code_cleaned) == toupper(cleaned_input), ]
    if (nrow(case_insensitive_match) > 0) {
      cat("find_participant: Found case-insensitive match\n")
      return(case_insensitive_match[1, ])
    }
    
    # Debug: Show available codes for troubleshooting
    cat("find_participant: No match found. Available codes (first 10):\n")
    available_codes <- head(data$access_code_cleaned, 10)
    for(i in seq_along(available_codes)) {
      cat("  ", i, ": '", available_codes[i], "'\n")
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
  
  # Enhanced periodic time check that updates rotation choices if conference changes
  observe({
    invalidateLater(30000, session)  # Check every 30 seconds
    
    previous_check <- values$time_check
    values$time_check <- is_conference_time()
    
    # If we're in the question step and the conference type changed
    if (values$current_step == "question" && !is.null(previous_check)) {
      
      current_type <- values$time_check$conference_type
      previous_type <- previous_check$conference_type
      
      # If conference type changed, update rotation choices
      if (current_type != previous_type) {
        if (values$time_check$allowed) {
          # Update to new conference choices
          new_choices <- get_current_rotation_choices()
          updateSelectizeInput(
            session, 
            "q_rotation",
            choices = new_choices,
            selected = character(0)  # Clear selection since conference changed
          )
          
          values$current_conference <- get_current_conference_name()
          cat("Conference window changed - updated rotation choices\n")
          
        } else {
          # Conference window closed
          values$current_step <- "time_restricted"
        }
      }
    }
    
    # Handle normal time window transitions
    if (values$time_check$allowed && values$current_step == "time_restricted") {
      values$current_step <- "access"
    }
    
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
  
  observeEvent(input$access_code, {
    if (!is.null(input$access_code) && nchar(input$access_code) > 0) {
      # Clear errors when typing
      values$error_message <- NULL
      
      # Clean the input
      cleaned <- clean_access_code(input$access_code)
      
      # Enhanced debugging
      cat("=== ACCESS CODE INPUT DEBUG ===\n")
      cat("Original input: '", input$access_code, "'\n")
      cat("Length: ", nchar(input$access_code), "\n")
      cat("Cleaned: '", cleaned, "'\n")
      cat("Raw bytes: ", paste(utf8ToInt(input$access_code), collapse = ", "), "\n")
      
      # Update input field if cleaning changed something
      if (cleaned != input$access_code && nchar(cleaned) > 0) {
        updateTextInput(session, "access_code", value = cleaned)
        cat("Updated input field with cleaned value\n")
      }
    }
  }, ignoreInit = TRUE)
  
  # Enhanced access code submission handler
  # Replace your existing observeEvent(input$submit_access, ...) with this:
  observeEvent(input$submit_access, {
    req(input$access_code)
    
    cat("=== ACCESS CODE SUBMISSION ===\n")
    
    # First check if we're in any time window
    time_check <- is_conference_time()
    if (!time_check$allowed) {
      values$error_message <- time_check$message
      return()
    }
    
    values$error_message <- NULL
    
    # Enhanced debugging for the search
    cat("Attempting to find participant for code: '", input$access_code, "'\n")
    
    # Find participant using robust matching
    participant <- find_participant(input$access_code, resident_data)
    
    if (!is.null(participant)) {
      cat("✅ Access granted!\n")
      values$participant <- participant
      values$current_step <- "question"
      
      # Clear the access code input
      updateTextInput(session, "access_code", value = "")
      
      # Get rotation choices for current conference window
      current_choices <- get_current_rotation_choices()
      current_conference <- get_current_conference_name()
      
      # Update rotation choices based on current conference
      updateSelectizeInput(
        session, 
        "q_rotation",
        choices = current_choices,
        selected = character(0)
      )
      
      # Store current conference info for display
      values$current_conference <- current_conference
      
    } else {
      cat("❌ Access denied\n")
      values$error_message <- paste("Invalid access code. Please check your code and try again.",
                                    "If you continue having issues, try typing the code manually instead of copying/pasting.")
      
      # Enhanced debug logging for failed attempts
      cleaned_code <- clean_access_code(input$access_code)
      cat("Access denied for code:", input$access_code, "\n")
      cat("  Cleaned version:", cleaned_code, "\n")
      
      # Show first few available codes for debugging
      if (!is.null(resident_data) && nrow(resident_data) > 0) {
        available_codes <- head(sapply(resident_data$access_code, clean_access_code), 5)
        cat("  Available codes:", paste(available_codes, collapse = ", "), "...\n")
      }
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
  
  # Add output for current conference name display
  output$current_conference_name <- renderText({
    if (!is.null(values$current_conference)) {
      return(values$current_conference)
    }
    return(get_current_conference_name())
  })
  
  # ============================================================================
  # RESPONSE SUBMISSION
  # ============================================================================
  
  # Enhanced response submission with conference type logging
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
    
    # Submit to REDCap with conference context
    success <- submit_question_response(
      record_id = values$participant$record_id,
      rotation = input$q_rotation,
      answer = input$q_answer,
      conference_type = time_check$conference_type  # Pass conference type for logging
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
    values$current_conference <- NULL
    
    # Reset all inputs
    updateTextInput(session, "access_code", value = "")
    updateSelectizeInput(session, "q_rotation", selected = character(0))
    updateRadioButtons(session, "q_answer", selected = character(0))
  })
}