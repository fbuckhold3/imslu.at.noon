ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    base_font = font_google("Inter"),
    heading_font = font_google("Inter", wght = 600)
  ),
  
  # Include custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/conference-app.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$title("SSM Health Conference Attendance")
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
          "SSM Health Conference Attendance"
        ),
        div(
          class = "header-info text-end",
          tags$small(
            class = "text-muted",
            format(Sys.Date(), "%B %d, %Y")
          )
        )
      )
    )
  ),
  
  # Main Content
  div(
    class = "container mt-4",
    
    # Access Code Step
    conditionalPanel(
      condition = "output.show_access_step",
      div(
        class = "ssm-card",
        div(
          class = "step-header",
          h3("Enter Your Access Code")
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
                class = "form-label required"
              ),
              textInput(
                "access_code",
                NULL,
                placeholder = "Enter your access code"
              ),
              # Add custom CSS class via tags
              tags$script(HTML("
                $('#access_code').addClass('form-control-lg text-center');
              ")),
              div(
                class = "form-help",
                "Enter your unique IMSLU resident ID, or if you are a rotator (Neurology, Anesthesia, etc), enter 'imrot'."
              )
            ),
            actionButton(
              "submit_access",
              "Continue",
              class = "btn btn-primary btn-lg w-100 fw-bold"
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
            class = "d-flex align-items-center justify-content-between mb-3",
            div(
              class = "d-flex align-items-center",
              div(
                class = "participant-icon me-3",
                tags$i(class = "fas fa-user-md", style = "font-size: 2.5rem; color: var(--ssm-primary-blue);")
              ),
              div(
                h4(
                  class = "participant-name mb-1",
                  textOutput("participant_name", inline = TRUE)
                ),
                p(
                  class = "text-muted mb-0",
                  "Conference Participant"
                )
              )
            ),
            div(
              class = "conference-date text-end",
              div(
                class = "fw-bold text-primary",
                format(Sys.Date(), "%B %d, %Y")
              ),
              div(
                class = "text-muted small",
                "Conference Date"
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
          h3("Conference Question")
        ),
        div(
          class = "step-content p-4",
          
          # Rotation Selection
          div(
            class = "form-group mb-4",
            tags$label(
              "Current Rotation:",
              class = "form-label required"
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
              class = "form-help",
              "Please take the time to accurately display the rotation you are on. If you have questions, please ask the Chiefs or Dr. B."
            )
          ),
          
          # Question Section
          conditionalPanel(
            condition = "input.q_rotation != null && input.q_rotation != ''",
            div(
              class = "question-section",
              hr(),
              div(
                class = "question-text mb-4",
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
                  class = "btn btn-success btn-lg fw-bold"
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
            tags$i(class = "fas fa-check-circle", style = "font-size: 4rem; color: var(--ssm-success-green);")
          ),
          h3(
            class = "text-success mb-3",
            "Response Submitted Successfully!"
          ),
          p(
            class = "text-muted mb-4",
            "Thank you for participating in today's conference."
          ),
          actionButton(
            "start_over",
            "Submit Another Response",
            class = "btn btn-outline-primary"
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
            "⚠️"
          ),
          div(
            h6(
              class = "alert-heading mb-1",
              "Error"
            ),
            textOutput("error_message")
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
        "SSM Health Educational Conference System"
      )
    )
  )
)