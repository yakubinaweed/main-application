# R/serversecond/data_prep.R

# Function to filter data based on gender and age
filter_data <- function(data, gender_choice, age_min, age_max, col_gender, col_age) {
  
  # Ensure column names exist in the data
  if (!col_gender %in% names(data) || !col_age %in% names(data)) {
    stop("Gender or age column not found in data.")
  }

  # Filter by age range
  filtered_data <- data %>%
    filter(!!sym(col_age) >= age_min & !!sym(col_age) <= age_max)
  
  # Filter by gender if "Both" is not selected
  if (gender_choice != "Both") {
    filtered_data <- filtered_data %>%
      filter(str_detect(!!sym(col_gender), regex(gender_choice, ignore_case = TRUE)))
  }
  
  return(filtered_data)
}