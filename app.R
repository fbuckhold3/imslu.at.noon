# app.R - Main application file for Conference Attendance Tracking

# Source the application components
source("global.R")
source("ui.R") 
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)