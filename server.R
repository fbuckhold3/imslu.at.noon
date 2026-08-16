# server.R - Conference Attendance Tracking App

server <- function(input, output, session) {
  
  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================
  
  values <- reactiveValues(
    current_step = "access",
    participant = NULL,
    error_message = NULL,
    my_history = NULL   # this resident's "questions" rows, fetched on login
  )

  # Rows saved earlier in this login (not yet reflected in values$my_history,
  # which is only refetched on login) — merged in for display so the
  # overview updates immediately after a submit.
  local_new_rows <- reactiveVal(NULL)

  display_history <- reactive({
    base  <- values$my_history
    extra <- local_new_rows()
    if (is.null(base) && is.null(extra)) return(NULL)
    if (is.null(base))  return(extra)
    if (is.null(extra)) return(base)
    cols <- union(names(base), names(extra))
    for (cn in setdiff(cols, names(base)))  base[[cn]]  <- NA
    for (cn in setdiff(cols, names(extra))) extra[[cn]] <- NA
    rbind(base[cols], extra[cols])
  })
  
  # ============================================================================
  # MOBILE INPUT CLEANING FUNCTIONS
  # ============================================================================
  
  # Fixed clean_access_code function - the main issue:
  clean_access_code <- function(code) {
    if (is.null(code) || is.na(code) || nchar(code) == 0) {
      return("")
    }
    
    # Step 1: Convert to character and AGGRESSIVE trimming
    cleaned <- as.character(code)
    cleaned <- trimws(cleaned, which = "both")  # Explicit both sides
    cleaned <- gsub("^\\s+|\\s+$", "", cleaned)  # Backup regex trim
    
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
    
    # Debug output to see what's happening
    if (code != cleaned) {
      cat("CLEANING DEBUG: '", code, "' -> '", cleaned, "'\n")
    }
    
    return(cleaned)
  }
  
  # Updated find_participant function:
  find_participant <- function(input_code, data) {
    if (is.null(data) || nrow(data) == 0 || is.null(input_code) || nchar(input_code) == 0) {
      cat("find_participant: Invalid input data\n")
      return(NULL)
    }
    
    # Clean the input code
    cleaned_input <- clean_access_code(input_code)
    cat("find_participant: Searching for cleaned code '", cleaned_input, "'\n")
    
    # Clean all stored codes for comparison
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
  # STEP VISIBILITY CONTROLS
  # ============================================================================

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
  observeEvent(input$submit_access, {
    req(input$access_code)

    cat("=== ACCESS CODE SUBMISSION ===\n")
    values$error_message <- NULL

    # Enhanced debugging for the search
    cat("Attempting to find participant for code: '", input$access_code, "'\n")

    # Find participant using robust matching
    participant <- find_participant(input$access_code, resident_data)

    if (!is.null(participant)) {
      cat("✅ Access granted!\n")
      values$participant <- participant
      values$current_step <- "question"

      # Shared-kiosk app — a different resident may have just used this same
      # browser session, so always refetch (never reuse) the previous
      # participant's history.
      values$my_history <- get_resident_questions_history(participant$record_id, conf_token, url)
      local_new_rows(NULL)

      # Clear the access code input
      updateTextInput(session, "access_code", value = "")

      # Reset date/conference-type/rotation/answer selections for a fresh entry
      updateDateInput(session, "q_date", value = Sys.Date())
      updateSelectInput(session, "q_conference_type", selected = character(0))
      updateSelectizeInput(session, "q_rotation", choices = c(), selected = character(0))
      updateRadioButtons(session, "q_answer", selected = character(0))

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

  # ============================================================================
  # ATTENDANCE OVERVIEW (percentage + calendar heatmap)
  # ============================================================================

  output$attendance_overview <- renderUI({
    req(values$participant)
    today <- Sys.Date()
    july1 <- academic_year_start(today)
    all_days <- seq(july1, today, by = "day")
    weekday_seq <- all_days[!weekdays(all_days) %in% c("Saturday", "Sunday")]

    df <- display_history()
    attended_dates  <- as.Date(character(0))
    afternoon_count <- 0L
    if (!is.null(df) && nrow(df) > 0) {
      d <- df
      d$.date <- suppressWarnings(as.Date(as.character(d$q_date)))
      d <- d[!is.na(d$.date) & d$.date >= july1 & d$.date <= today, , drop = FALSE]
      qual <- d[as.character(d$q_conference_type) %in% percentage_conference_types, , drop = FALSE]
      attended_dates  <- unique(qual$.date)
      afternoon_count <- sum(as.character(d$q_conference_type) == "3", na.rm = TRUE)
    }
    n_weekdays <- length(weekday_seq)
    pct <- if (n_weekdays > 0) round(length(attended_dates) / n_weekdays * 100) else 0

    div(class = "ssm-card mb-4",
      div(class = "step-content p-4",
        div(class = "d-flex flex-wrap gap-4 align-items-end mb-3",
          div(
            div(style = "font-size:0.78rem; color:#6c757d;",
                paste0("Noon Conference + Grand Rounds — since ", format(july1, "%b %d"))),
            div(style = "font-size:1.8rem; font-weight:700; color:var(--roundsui-accent); line-height:1.2;",
              paste0(pct, "%"),
              tags$span(style = "font-size:0.85rem; font-weight:400; color:#6c757d; margin-left:6px;",
                        paste0("(", length(attended_dates), " of ", n_weekdays, " weekdays)")))
          ),
          div(
            div(style = "font-size:0.78rem; color:#6c757d;",
                paste0("Afternoon School — since ", format(july1, "%b %d"))),
            div(style = "font-size:1.8rem; font-weight:700; color:var(--roundsui-accent); line-height:1.2;",
                afternoon_count)
          )
        ),
        build_attendance_heatmap(attended_dates, today)
      )
    )
  })

  # ============================================================================
  # CONFERENCE TYPE -> ROTATION CHOICES
  # ============================================================================

  observeEvent(input$q_conference_type, {
    req(input$q_conference_type)
    updateSelectizeInput(
      session,
      "q_rotation",
      choices = rotation_choices_for_conference(input$q_conference_type),
      selected = character(0)
    )
  }, ignoreInit = TRUE)

  # ============================================================================
  # DATE -> QUIZ VISIBILITY (server-computed so it can't go stale like a
  # baked-in JS date string would on a long-running deployment)
  # ============================================================================

  output$is_today <- reactive({
    req(input$q_date)
    identical(input$q_date, Sys.Date())
  })
  outputOptions(output, "is_today", suspendWhenHidden = FALSE)

  # ============================================================================
  # RESPONSE SUBMISSION
  # ============================================================================

  # Shared handler for "Submit Response", "I'm not sure", and the past-date
  # "Log Attendance" button — the only difference is whether an answer is
  # required/passed (past-date entries never have a quiz answer).
  do_submit_response <- function(answer) {
    req(values$participant)
    req(input$q_conference_type)
    req(input$q_date)

    needs_rotation <- length(rotation_choices_for_conference(input$q_conference_type)) > 0
    if (needs_rotation) req(input$q_rotation)

    values$error_message <- NULL

    result <- submit_question_response(
      record_id = values$participant$record_id,
      conference_type = input$q_conference_type,
      rotation = if (needs_rotation) input$q_rotation else NULL,
      answer = answer,
      date = input$q_date
    )

    if (isTRUE(result$success)) {
      values$current_step <- "success"

      # Reflect the new entry immediately in the attendance overview
      new_row <- data.frame(
        record_id = as.character(values$participant$record_id),
        redcap_repeat_instrument = "questions",
        redcap_repeat_instance = as.character(result$instance),
        q_date = result$date,
        q_conference_type = as.character(input$q_conference_type),
        q_rotation = if (needs_rotation) as.character(input$q_rotation) else "",
        q_entry_timestamp = result$entry_timestamp,
        stringsAsFactors = FALSE
      )
      local_new_rows(rbind(local_new_rows(), new_row))

      # Reset form values
      updateDateInput(session, "q_date", value = Sys.Date())
      updateSelectInput(session, "q_conference_type", selected = character(0))
      updateSelectizeInput(session, "q_rotation", choices = c(), selected = character(0))
      updateRadioButtons(session, "q_answer", selected = character(0))
    } else {
      values$error_message <- "Failed to submit response. Please try again."
    }
  }

  observeEvent(input$submit_response, {
    req(input$q_answer)
    do_submit_response(input$q_answer)
  })

  observeEvent(input$mark_attending_only, {
    do_submit_response(NULL)
  })

  observeEvent(input$submit_past, {
    do_submit_response(NULL)
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
    values$my_history <- NULL
    local_new_rows(NULL)

    # Reset all inputs
    updateTextInput(session, "access_code", value = "")
    updateDateInput(session, "q_date", value = Sys.Date())
    updateSelectInput(session, "q_conference_type", selected = character(0))
    updateSelectizeInput(session, "q_rotation", choices = c(), selected = character(0))
    updateRadioButtons(session, "q_answer", selected = character(0))
  })
}