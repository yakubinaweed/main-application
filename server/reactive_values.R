# R/server/reactive_values.R

# Define reactive values for storing data and selected directory
data_reactive <- reactiveVal()
selected_dir_reactive <- reactiveVal()

# Reactive value for displaying dynamic messages to the user
# Stores a list with 'type' (e.g., "error", "success", "") and 'text'
message_rv <- reactiveVal(list(type = "", text = ""))