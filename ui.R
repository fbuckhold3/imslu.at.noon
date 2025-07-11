# ui.R - Conference Attendance Tracking App

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    base_font = font_google("Inter"),
    heading_font = font_google("Inter", wght = 600)
  ),
  
  # Mobile-optimized head section
  tags$head(
    useShinyjs(),
    
    # CSS and Font Awesome
    tags$link(rel = "stylesheet", type = "text/css", href = "www/conference-app.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    
    # Mobile viewport and app settings
    tags$meta(name = "viewport", 
              content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    tags$meta(name = "mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "default"),
    tags$meta(name = "theme-color", content = "#1e40af"),
    
    # Page title
    tags$title("SSM Health Conference Attendance"),
    
    # Mobile-specific styles
    tags$style(HTML("
      /* Prevent iOS zoom on input focus */
      input[type='text'], 
      input[type='email'], 
      input[type='password'], 
      select, 
      textarea {
        font-size: 16px !important;
      }
      
      /* Better mobile button styling */
      .btn-lg {
        padding: 12px 24px;
        font-size: 18px;
        min-height: 50px;
      }
      
      /* Mobile-specific adjustments */
      @media screen and (max-width: 768px) {
        .container {
          padding-left: 15px;
          padding-right: 15px;
        }
        
        .form-control {
          padding: 12px 16px;
          font-size: 16px;
        }
        
        .form-control-lg {
          padding: 15px 20px;
          font-size: 18px;
        }
        
        .ssm-card {
          margin-bottom: 20px;
        }
        
        .step-content {
          padding: 20px 15px !important;
        }
        
        .access-form {
          max-width: 100% !important;
        }
        
        /* Improve radio button spacing on mobile */
        .radio {
          margin-bottom: 15px;
        }
        
        .radio label {
          padding-left: 25px;
          font-size: 16px;
        }
        
        /* Better error message styling */
        .alert {
          margin-top: 20px;
          padding: 15px;
          border-radius: 8px;
        }
      }
      
      /* Fix for mobile keyboard displacement */
      .container-fluid {
        overflow-x: hidden;
      }
      
      /* Improve touch targets */
      .btn {
        min-height: 44px;
      }
      
      /* Better selectize styling for mobile */
      .selectize-input {
        min-height: 44px;
        padding: 12px 16px;
        font-size: 16px;
      }
      
      .selectize-dropdown {
        font-size: 16px;
      }
    "))
  ),
  
  # Header
  tags$header(
    class = "conference-header",
    div(
      class = "container-fluid",
      div(
        class = "d-flex justify-content-between align-items-center py-3",
        h1(
          class = "navbar-brand mb-0",
          style = "font-size: clamp(1.2rem, 4vw, 1.75rem);",
          "SSM Health Conference Attendance"
        ),
        div(
          class = "header-info text-end",
          tags$small(
            class = "text-muted",
            style = "font-size: clamp(0.8rem, 2.5vw, 0.9rem);",
            format(lubridate::with_tz(Sys.time(), "America/Chicago"), "%B %d, %Y")
          )
        )
      )
    )
  ),
  
  # Main Content
  div(
    class = "container mt-4",
    
    # Time Restriction Message
    conditionalPanel(
      condition = "output.show_time_restriction",
      div(
        class = "ssm-card mb-4",
        div(
          class = "time-restriction-content text-center py-5",
          div(
            class = "restriction-icon mb-4",
            tags$i(class = "fas fa-clock", style = "font-size: clamp(3rem, 8vw, 4rem); color: var(--ssm-warning-orange);")
          ),
          h3(
            class = "text-warning mb-3",
            style = "font-size: clamp(1.2rem, 5vw, 1.5rem);",
            "Conference Submission Window Closed"
          ),
          div(
            class = "restriction-message mb-4",
            style = "font-size: clamp(0.9rem, 3vw, 1rem);",
            textOutput("time_restriction_message")
          ),
          div(
            class = "current-time-display",
            style = "font-size: clamp(0.9rem, 3vw, 1rem);",
            tags$strong("Current Time: "),
            textOutput("current_time_display", inline = TRUE)
          )
        )
      )
    ),
    
    # Access Code Step
    conditionalPanel(
      condition = "output.show_access_step",
      div(
        class = "ssm-card",
        div(
          class = "step-header",
          h3(
            style = "font-size: clamp(1.2rem, 5vw, 1.5rem);",
            "Enter Your Access Code"
          )
        ),
        div(
          class = "step-content p-4",
          div(
            class = "access-form mx-auto",
            style = "max-width: 400px;",
            div(
              class = "form-group mb-4",
              tags$label(
                "Access Code:",
                class = "form-label required",
                style = "font-size: clamp(1rem, 3.5vw, 1.1rem);",
                `for` = "access_code"
              ),
              textInput("access_code", 
                        "Enter Access Code:", 
                        value = "",
                        placeholder = "Enter your code here",
                        # Enhanced mobile attributes
                        `autocomplete` = "off",
                        `autocorrect` = "off", 
                        `autocapitalize` = "characters",  # Force uppercase on mobile
                        `spellcheck` = "false",
                        `data-lpignore` = "true",  # Prevent password managers
                        `inputmode` = "text"       # Optimize mobile keyboard
              ),
              div(
                class = "form-help mt-2",
                style = "font-size: clamp(0.8rem, 2.5vw, 0.9rem);",
                "Enter your unique IMSLU resident ID, or if you are a rotator (Neurology, Anesthesia, etc), enter 'imrot'."
              )
            ),
            actionButton(
              "submit_access",
              "Continue",
              class = "btn btn-primary btn-lg w-100 fw-bold",
              style = "font-size: clamp(1rem, 4vw, 1.2rem);"
            )
          )
        )
      )
    ),
    
    # Question Form Step
    conditionalPanel(
      condition = "output.show_question_step",
      
      # Participant Info Card
      div(
        class = "ssm-card mb-4",
        div(
          class = "participant-card",
          div(
            class = "d-flex align-items-center justify-content-between mb-3 flex-wrap",
            div(
              class = "d-flex align-items-center mb-2 mb-md-0",
              div(
                class = "participant-icon me-3",
                tags$i(class = "fas fa-user-md", style = "font-size: clamp(2rem, 6vw, 2.5rem); color: var(--ssm-primary-blue);")
              ),
              div(
                h4(
                  class = "participant-name mb-1",
                  style = "font-size: clamp(1.1rem, 4vw, 1.3rem);",
                  textOutput("participant_name", inline = TRUE)
                ),
                p(
                  class = "text-muted mb-0",
                  style = "font-size: clamp(0.8rem, 2.5vw, 0.9rem);",
                  "Conference Participant"
                )
              )
            ),
            div(
              class = "conference-date text-end",
              div(
                class = "fw-bold text-primary",
                style = "font-size: clamp(0.9rem, 3vw, 1rem);",
                format(lubridate::with_tz(Sys.time(), "America/Chicago"), "%B %d, %Y")
              ),
              div(
                class = "text-muted small",
                style = "font-size: clamp(0.7rem, 2vw, 0.8rem);",
                "Conference Date"
              )
            )
          )
        )
      ),
      
      # Conference Type Display
      div(
        class = "ssm-card mb-3",
        div(
          class = "conference-type-display text-center py-3",
          div(
            class = "d-flex align-items-center justify-content-center",
            div(
              class = "conference-icon me-3",
              tags$i(class = "fas fa-calendar-check", style = "font-size: 1.5rem; color: var(--ssm-primary-blue);")
            ),
            div(
              h5(
                class = "mb-1",
                style = "font-size: clamp(1rem, 3.5vw, 1.2rem);",
                textOutput("current_conference_name", inline = TRUE)
              ),
              p(
                class = "text-muted mb-0 small",
                style = "font-size: clamp(0.8rem, 2.5vw, 0.9rem);",
                "Current Conference Session"
              )
            )
          )
        )
      ),
      
      # Question Form Card
      div(
        class = "ssm-card",
        div(
          class = "step-header",
          h3(
            style = "font-size: clamp(1.2rem, 5vw, 1.5rem);",
            "Conference Question"
          )
        ),
        div(
          class = "step-content p-4",
          
          # Rotation Selection
          div(
            class = "form-group mb-4",
            tags$label(
              "Current Rotation:",
              class = "form-label required",
              style = "font-size: clamp(1rem, 3.5vw, 1.1rem);"
            ),
            selectizeInput(
              "q_rotation",
              NULL,
              choices = NULL,  # Start with empty choices
              selected = NULL,
              options = list(
                placeholder = "Search or select your rotation...",
                searchField = 'text',
                maxOptions = 50,
                create = FALSE
              )
            ),
            div(
              class = "form-help mt-2",
              style = "font-size: clamp(0.8rem, 2.5vw, 0.9rem);",
              "Please take the time to accurately display the rotation you are on. If you have questions, please ask the Chiefs or Dr. B."
            )
          ),
          
          # Question Section - Only shows when rotation is selected
          conditionalPanel(
            condition = "input.q_rotation != null && input.q_rotation != ''",
            div(
              class = "question-section",
              hr(),
              div(
                class = "question-text mb-4",
                style = "font-size: clamp(1rem, 3.5vw, 1.1rem);",
                "Please select your answer to today's conference question:"
              ),
              div(
                class = "answer-options",
                radioButtons(
                  "q_answer",
                  NULL,
                  choices = c(
                    "A" = "1",
                    "B" = "2", 
                    "C" = "3",
                    "D" = "4",
                    "E" = "5"
                  ),
                  selected = character(0),
                  inline = FALSE
                )
              ),
              div(
                class = "mt-4 d-grid",
                actionButton(
                  "submit_response",
                  "Submit Response",
                  class = "btn btn-success btn-lg fw-bold",
                  style = "font-size: clamp(1rem, 4vw, 1.2rem);"
                )
              )
            )
          )
        )
      )
    ),
    
    # Success Step
    conditionalPanel(
      condition = "output.show_success_step",
      div(
        class = "ssm-card text-center",
        div(
          class = "success-content py-5",
          div(
            class = "success-icon mb-4",
            tags$i(class = "fas fa-check-circle", style = "font-size: clamp(3rem, 8vw, 4rem); color: var(--ssm-success-green);")
          ),
          h3(
            class = "text-success mb-3",
            style = "font-size: clamp(1.2rem, 5vw, 1.5rem);",
            "Response Submitted Successfully!"
          ),
          p(
            class = "text-muted mb-4",
            style = "font-size: clamp(0.9rem, 3vw, 1rem);",
            "Thank you for participating in today's conference."
          ),
          actionButton(
            "start_over",
            "Submit Another Response",
            class = "btn btn-outline-primary",
            style = "font-size: clamp(0.9rem, 3vw, 1rem);"
          )
        )
      )
    ),
    
    # Error Messages
    conditionalPanel(
      condition = "output.show_error",
      div(
        class = "alert alert-danger mt-3",
        div(
          class = "d-flex align-items-start",
          div(
            class = "alert-icon me-2",
            style = "font-size: 1.2rem;",
            "⚠️"
          ),
          div(
            h6(
              class = "alert-heading mb-1",
              style = "font-size: clamp(1rem, 3.5vw, 1.1rem);",
              "Error"
            ),
            div(
              style = "font-size: clamp(0.9rem, 3vw, 1rem);",
              textOutput("error_message")
            )
          )
        )
      )
    )
  ),
  
  # Footer
  tags$footer(
    class = "conference-footer mt-5 py-4",
    div(
      class = "container text-center",
      p(
        class = "mb-0 text-muted",
        style = "font-size: clamp(0.8rem, 2.5vw, 0.9rem);",
        "SSM Health Educational Conference System"
      )
    )
  )
)