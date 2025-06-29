# server.R - Conference Attendance Tracking App

server <- function(input, output, session) {
  
  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================
  
  values <- reactiveValues(
    current_step = "access",
    participant = NULL,
    selected_rotation = NULL,
    error_message = NULL,
    filtered_rotations = NULL
  )
  
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
  
  output$show_selected_rotation <- reactive({
    !is.null(input$q_rotation) && input$q_rotation != ""
  })
  outputOptions(output, "show_selected_rotation", suspendWhenHidden = FALSE)
  
  output$show_question <- reactive({
    !is.null(input$q_rotation) && input$q_rotation != ""
  })
  outputOptions(output, "show_question", suspendWhenHidden = FALSE)
  
  output$show_error <- reactive({
    !is.null(values$error_message)
  })
  outputOptions(output, "show_error", suspendWhenHidden = FALSE)
  
  # ============================================================================
  # ACCESS CODE HANDLING
  # ============================================================================
  
  observeEvent(input$submit_access, {
    req(input$access_code)
    
    values$error_message <- NULL
    
    # Find participant by access code
    if (!is.null(resident_data) && nrow(resident_data) > 0) {
      participant <- resident_data[resident_data$access_code == input$access_code, ]
      
      if (nrow(participant) > 0) {
        values$participant <- participant[1, ]  # Take first match
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
        
      } else {
        values$error_message <- "Invalid access code. Please check your code and try again."
      }
    } else {
      values$error_message <- "Unable to validate access code. Please try again later."
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
  
  # ============================================================================
  # CLEAR ERROR MESSAGES
  # ============================================================================
  
  # Clear error when user starts typing
  observeEvent(input$access_code, {
    if (nchar(input$access_code) > 0) {
      values$error_message <- NULL
    }
  })
  
}