# global.R - Conference Attendance Tracking App

# ============================================================================
# LIBRARIES
# ============================================================================
library(shiny)
library(dplyr)
library(config)
library(bslib)
library(httr)
library(jsonlite)
library(DT)
library(lubridate)
library(shinyjs)
library(stringi)

# ============================================================================
# CONFERENCE CONFIGURATION
# ============================================================================

CONFERENCE_TIMEZONE <- "America/Chicago"

# Resident-picked conference type — no time-window gating, log any time.
conference_type_choices <- c(
  "SLUH Noon Conference" = "1",
  "VA Noon Conference"   = "2",
  "Afternoon School"     = "3",
  "SLUH Grand Rounds"    = "4"
)

# Team/rotation question is hidden entirely for Afternoon School.
rotation_choices_for_conference <- function(conf_code) {
  if (conf_code %in% c("1", "4")) sluh_rotation_choices
  else if (identical(conf_code, "2")) va_rotation_choices
  else c()
}

# ============================================================================
# ATTENDANCE OVERVIEW (percentage + calendar heatmap)
# ============================================================================
# Mirrors imslu.ind.dash/R/modules/mod_attendance.R's .att_* helpers —
# same July-1 rollover rule and heatmap design, ported here without the
# module-namespace prefix since this app isn't modularized.

# Conference types that count toward the July-1 attendance percentage.
# Afternoon School is tracked separately as a simple counter, not a rate.
percentage_conference_types <- c("1", "2", "4")

# Most recent July 1 on or before `today` — same academic-year convention
# used elsewhere in this ecosystem (gmed's level-at-time calculations).
academic_year_start <- function(today) {
  yr <- as.numeric(format(today, "%Y"))
  mo <- as.numeric(format(today, "%m"))
  if (mo >= 7) as.Date(paste0(yr, "-07-01")) else as.Date(paste0(yr - 1, "-07-01"))
}

# Small calendar-heatmap: rows = Mon-Fri, columns = the last `n_weeks`
# weeks (oldest -> newest, ending this week). Attended weekdays get a
# filled checkmark cell so the status isn't color-only; upcoming weekdays
# are outlined/blank; everything else is a muted "no entry" cell.
build_attendance_heatmap <- function(attended_dates, today, n_weeks = 9) {
  iso_wday <- as.integer(format(today, "%u"))  # Mon=1..Sun=7
  monday_this_week <- today - (iso_wday - 1)
  week_starts <- monday_this_week - 7 * seq(n_weeks - 1, 0)
  day_labels <- c("Mon", "Tue", "Wed", "Thu", "Fri")
  attended_chr <- format(attended_dates, "%Y-%m-%d")

  month_cells <- lapply(seq_along(week_starts), function(i) {
    ws <- week_starts[i]
    show_label <- i == 1 || format(ws, "%m") != format(week_starts[i - 1], "%m")
    tags$td(style = "font-size:0.65rem; color:#6c757d; text-align:center; padding-bottom:2px;",
            if (show_label) format(ws, "%b") else "")
  })

  day_rows <- lapply(seq_along(day_labels), function(wd_i) {
    row_cells <- lapply(seq_along(week_starts), function(i) {
      d <- week_starts[i] + (wd_i - 1)
      if (d > today) {
        cell <- div(style = "width:18px; height:18px; border-radius:3px; background:#ffffff; border:1px dashed #dee2e6;",
                     title = paste(format(d, "%a, %b %d"), "— upcoming"))
      } else if (format(d, "%Y-%m-%d") %in% attended_chr) {
        cell <- div(style = "width:18px; height:18px; border-radius:3px; background:#198754; display:flex; align-items:center; justify-content:center;",
                     title = paste(format(d, "%a, %b %d"), "— attended"),
                     tags$span(style = "color:#fff; font-size:10px; line-height:1;", "✓"))
      } else {
        cell <- div(style = "width:18px; height:18px; border-radius:3px; background:#e9ecef;",
                     title = paste(format(d, "%a, %b %d"), "— no entry"))
      }
      tags$td(style = "padding:2px;", cell)
    })
    tags$tr(
      tags$td(style = "font-size:0.7rem; color:#6c757d; padding-right:6px; text-align:right; white-space:nowrap;", day_labels[wd_i]),
      row_cells
    )
  })

  legend <- div(style = "display:flex; flex-wrap:wrap; gap:16px; margin-top:10px; font-size:0.78rem; color:#6c757d;",
    div(style = "display:flex; align-items:center; gap:5px;",
      div(style = "width:14px; height:14px; border-radius:3px; background:#198754; display:flex; align-items:center; justify-content:center;",
          tags$span(style = "color:#fff; font-size:9px;", "✓")),
      "Attended (Noon Conference / Grand Rounds)"),
    div(style = "display:flex; align-items:center; gap:5px;",
      div(style = "width:14px; height:14px; border-radius:3px; background:#e9ecef;"), "No entry"),
    div(style = "display:flex; align-items:center; gap:5px;",
      div(style = "width:14px; height:14px; border-radius:3px; background:#fff; border:1px dashed #dee2e6;"), "Upcoming")
  )

  tagList(
    div(style = "overflow-x:auto;",
      tags$table(style = "border-collapse:collapse;",
        tags$tbody(tags$tr(tags$td(""), month_cells), day_rows))
    ),
    legend
  )
}

# ============================================================================
# ROTATION OPTIONS BY CONFERENCE
# ============================================================================

# SLUH Conference rotations (11:55-12:10)
sluh_rotation_choices <- c(
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
  "Elective / Clinics CSM" = "12"
)

# VA Conference rotations (12:25-12:40)
va_rotation_choices <- c(
  "VA A" = "13",
  "VA B" = "14",
  "VA C" = "15",
  "VA D" = "16",
  "VA Clinics or Consults" = "17"
)

# Combined choices for reference (if needed elsewhere)
all_rotation_choices <- c(sluh_rotation_choices, va_rotation_choices)

# ============================================================================
# ENVIRONMENT SETUP
# ============================================================================

# Identify whether we are hosted
is_hosted <- Sys.getenv("CONF_TOKEN") != ""

# Load tokens from environment or config
if (is_hosted) {
  conf_token <- Sys.getenv("CONF_TOKEN")
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
} else {
  # Try to load from config file if it exists, otherwise use environment
  if (file.exists("config.yml")) {
    conf <- config::get(file = "config.yml")
    conf_token <- conf$conf_token
  } else {
    # Fallback to environment variable if no config file
    conf_token <- Sys.getenv("CONF_TOKEN")
    if (conf_token == "") {
      stop("No REDCap token found. Please set CONF_TOKEN environment variable or create config.yml")
    }
  }
}

# RedCap URL
url <- "https://redcapsurvey.slu.edu/api/"

# ============================================================================
# ANSWER CHOICES - Update these to match your REDCap field values
# ============================================================================
answer_choices <- c(
  "A" = "1",  # Update these values to match your REDCap q_answer field
  "B" = "2",  # Check your REDCap project for the actual coded values
  "C" = "3",  # These might be "1, A" = "1" or similar
  "D" = "4",
  "E" = "5"
)

# ============================================================================
# DATA FUNCTIONS
# ============================================================================

get_resident_data <- function() {
  tryCatch({
    cat("Pulling resident database...\n")
    
    formData <- list(
      "token" = conf_token,
      content = 'record',
      action = 'export',
      format = 'json',
      type = 'flat',
      csvDelimiter = '',
      forms = 'resident_data',
      rawOrLabel = 'label',
      rawOrLabelHeaders = 'raw',
      exportCheckboxLabel = 'false',
      exportSurveyFields = 'false',
      exportDataAccessGroups = 'false',
      returnFormat = 'json'
    )
    
    response <- httr::POST(url, body = formData, encode = "form")
    
    if (httr::status_code(response) != 200) {
      stop("REDCap API call failed with status: ", httr::status_code(response))
    }
    
    response_text <- httr::content(response, "text", encoding = "UTF-8")
    resident_data <- jsonlite::fromJSON(response_text)
    
    cat("Resident data loaded. Total rows:", nrow(resident_data), "\n")
    return(resident_data)
    
  }, error = function(e) {
    cat("Error in API pull:", e$message, "\n")
    return(NULL)
  })
}

# Full "questions" history for one resident — used to build their
# attendance overview (percentage + heatmap). Separate from
# get_next_question_instance() below, which stays a lean instance-only
# query since it's called on every submit and needs to stay fast.
get_resident_questions_history <- function(record_id, token, url) {
  tryCatch({
    response <- httr::POST(
      url = url,
      body = list(
        token = token,
        content = "record",
        action = "export",
        format = "json",
        type = "flat",
        records = as.character(record_id),
        fieldNames = "record_id,redcap_repeat_instrument,redcap_repeat_instance,q_date,q_conference_type,q_rotation,q_entry_timestamp",
        rawOrLabel = "raw",
        rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false",
        exportSurveyFields = "false",
        exportDataAccessGroups = "false",
        returnFormat = "json"
      ),
      encode = "form"
    )

    if (httr::status_code(response) != 200) {
      cat("❌ get_resident_questions_history failed with status:", httr::status_code(response), "\n")
      return(NULL)
    }

    all_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    if (!is.data.frame(all_data) || nrow(all_data) == 0) return(NULL)

    history <- all_data[!is.na(all_data$redcap_repeat_instrument) &
                           all_data$redcap_repeat_instrument == "questions", ]
    if (nrow(history) == 0) return(NULL)

    history
  }, error = function(e) {
    cat("❌ Error in get_resident_questions_history:", e$message, "\n")
    return(NULL)
  })
}

# conference_type : one of conference_type_choices' values ("1"-"4")
# rotation        : team/rotation code, or NULL/"" for Afternoon School
# answer          : quiz answer code "1"-"5", or NULL/"" if the resident used
#                   "I'm not sure — just mark me as attending"
# date            : Date to log the entry under (defaults to today; a past
#                   date is a resident-initiated backfill)
# Returns list(success, instance, entry_timestamp) on success, list(success=FALSE) on failure.
submit_question_response <- function(record_id, conference_type, rotation = NULL, answer = NULL, date = Sys.Date()) {
  tryCatch({
    current_date <- format(date, "%Y-%m-%d")
    entry_timestamp <- format(lubridate::with_tz(Sys.time(), CONFERENCE_TIMEZONE), "%Y-%m-%d %H:%M:%S")

    cat("=== SUBMITTING QUESTION RESPONSE ===\n")
    cat("Record ID:", record_id, "\n")
    cat("Conference Type:", conference_type, "\n")
    cat("Rotation:", rotation, "\n")
    cat("Answer:", answer, "\n")
    cat("Date:", current_date, "\n")
    cat("Entry Timestamp:", entry_timestamp, "\n")

    # Get the next available instance number for this record's questions instrument
    next_instance <- get_next_question_instance(record_id, conf_token, url)
    cat("Next instance number:", next_instance, "\n")

    # Create the data to submit in data.frame format
    redcap_data <- data.frame(
      record_id = as.character(record_id),
      redcap_repeat_instrument = "questions",
      redcap_repeat_instance = as.character(next_instance),
      q_date = current_date,
      q_conference_type = as.character(conference_type),
      q_rotation = if (is.null(rotation)) "" else as.character(rotation),
      q_answer = if (is.null(answer)) "" else as.character(answer),
      q_entry_timestamp = entry_timestamp,
      questions_complete = "2",
      stringsAsFactors = FALSE
    )

    cat("REDCap submission data:\n")
    print(redcap_data)
    
    # Submit to REDCap
    result <- httr::POST(
      url = url,
      body = list(
        token = conf_token,
        content = "record",
        format = "json",
        type = "flat",
        data = jsonlite::toJSON(redcap_data, auto_unbox = TRUE)
      ),
      encode = "form"
    )
    
    response_text <- httr::content(result, "text")
    cat("REDCap response status:", httr::status_code(result), "\n")
    cat("REDCap response:", response_text, "\n")
    
    if (httr::status_code(result) != 200) {
      stop("Failed to submit response to REDCap. Status: ", httr::status_code(result), " Response: ", response_text)
    }
    
    cat("✅ Successfully submitted", conference_type, "conference response for record:", record_id, "\n")
    return(list(success = TRUE, instance = next_instance, entry_timestamp = entry_timestamp,
                date = current_date))

  }, error = function(e) {
    cat("❌ Error submitting response:", e$message, "\n")
    return(list(success = FALSE))
  })
}

# Function to get the next available instance number for questions instrument
get_next_question_instance <- function(record_id, token, url) {
  tryCatch({
    cat("=== GETTING NEXT INSTANCE FOR RECORD ID:", record_id, "===\n")
    
    # Query for specific record with only needed fields
    response <- httr::POST(
      url = url,
      body = list(
        token = token,
        content = "record",
        action = "export",
        format = "json",
        type = "flat",
        records = as.character(record_id),
        fieldNames = "record_id,redcap_repeat_instrument,redcap_repeat_instance",
        rawOrLabel = "raw",
        rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false",
        exportSurveyFields = "false",
        exportDataAccessGroups = "false",
        returnFormat = "json"
      ),
      encode = "form"
    )
    
    cat("REDCap query response status:", httr::status_code(response), "\n")
    
    if (httr::status_code(response) == 200) {
      response_text <- httr::content(response, "text", encoding = "UTF-8")
      all_data <- jsonlite::fromJSON(response_text)
      
      if (is.data.frame(all_data) && nrow(all_data) > 0) {
        # Filter for questions instrument instances
        question_records <- all_data[
          !is.na(all_data$redcap_repeat_instrument) & 
            all_data$redcap_repeat_instrument == "questions", 
        ]
        
        cat("Question records found:", nrow(question_records), "\n")
        
        if (nrow(question_records) > 0) {
          # Get all instance numbers
          instances <- as.numeric(question_records$redcap_repeat_instance)
          instances <- instances[!is.na(instances)]
          
          if (length(instances) > 0) {
            instances <- sort(instances)
            max_instance <- max(instances)
            next_instance <- max_instance + 1
            
            cat("✅ Found existing instances:", paste(instances, collapse = ", "), "\n")
            cat("✅ Max instance:", max_instance, "Next instance:", next_instance, "\n")
            
            return(next_instance)
          }
        }
      }
    } else {
      cat("❌ REDCap query failed with status:", httr::status_code(response), "\n")
      error_text <- httr::content(response, "text", encoding = "UTF-8")
      cat("Error details:", error_text, "\n")
    }
    
    # Fallback to instance 1 if no existing instances found
    cat("⚠️ No existing question instances found, starting with instance 1\n")
    return(1)
    
  }, error = function(e) {
    cat("❌ Error in get_next_question_instance:", e$message, "\n")
    return(1)
  })
}

# ============================================================================
# DATA LOADING
# ============================================================================

# Load resident data on startup
resident_data <- get_resident_data()

if (!is.null(resident_data)) {
  cat("Resident data loaded successfully. Total records:", nrow(resident_data), "\n")
} else {
  cat("Failed to load resident data\n")
  resident_data <- data.frame() # Create empty dataframe as fallback
}