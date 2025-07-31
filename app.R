# app.R

# Load necessary libraries
library(shiny)
library(bslib)
library(refineR)
library(readxl)
library(moments)
library(shinyjs)
library(shinyWidgets)
library(shinyFiles)

# Source UI and Server logic from separate files
source("ui.R")
source("server.R")
source("utils.R") # If you have shared utility functions

# Run the app
shinyApp(ui = ui, server = server)