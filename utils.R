# R/utils.R

# Function to generate a safe filename for plots
generate_safe_filename <- function(plot_title, base_path, extension = "png") {
  # Replace non-alphanumeric characters with underscores
  safe_title <- gsub("[^a-zA-Z0-9_-]", "_", plot_title)
  # Add date and time for uniqueness
  datestamp <- format(Sys.Date(), "%Y%m%d")
  timestamp <- format(Sys.time(), "%H%M%S") # Added seconds for more uniqueness
  file.path(base_path, paste0(safe_title, "_", datestamp, "-", timestamp, ".", extension))
}

# You could add other utility functions here, e.g., data cleaning helpers,
# custom validation functions, etc.