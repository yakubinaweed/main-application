# R/utils.R
library(dplyr)
library(stringr)
library(ggplot2)

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

# Function to generate a safe filename for plots
generate_safe_filename <- function(plot_title, base_path, extension = "png") {
  # Replace non-alphanumeric characters with underscores
  safe_title <- gsub("[^a-zA-Z0-9_-]", "_", plot_title)
  # Add date and time for uniqueness
  datestamp <- format(Sys.Date(), "%Y%m%d")
  timestamp <- format(Sys.time(), "%H%M%S") # Added seconds for more uniqueness
  file.path(base_path, paste0(safe_title, "_", datestamp, "-", timestamp, ".", extension))
}

# Function to generate a plot from refineR output
plot_refiner_output <- function(df, value_col_name, unit, input_low, input_high) {
  
  # Create a base plot with a histogram
  plot_title <- "RefineR Reference Interval Estimation"
  xlab_label <- sprintf("%s [%s]", value_col_name, unit)
  
  # Check if refiner_output is valid
  if (is.null(df) || nrow(df) == 0) {
    p <- ggplot() + labs(title = plot_title) + 
      annotate("text", x = 0.5, y = 0.5, label = "No valid data to plot.", size = 6)
    return(p)
  }
  
  # This part of the code is adapted from the plot.refineR method but using ggplot
  df_to_plot <- data.frame(
    Value = df[[value_col_name]]
  )
  
  p <- ggplot(df_to_plot, aes(x = Value)) +
    geom_histogram(bins = 30, aes(y = ..density..), fill = "#e9ecef", color = "#69b3a2") +
    geom_density(color = "#4c87c7", size = 1.2) +
    theme_minimal() +
    labs(title = plot_title, x = xlab_label, y = "Density")
  
  # Add user-defined limits if available
  if (!is.na(input_low)) {
    p <- p + geom_vline(xintercept = input_low, linetype = "dotted", color = "red", size = 1) +
      annotate("text", x = input_low, y = Inf, label = paste("User Low:", input_low), vjust = 3, hjust = -0.1, color = "darkred", size = 4)
  }
  if (!is.na(input_high)) {
    p <- p + geom_vline(xintercept = input_high, linetype = "dotted", color = "blue", size = 1) +
      annotate("text", x = input_high, y = Inf, label = paste("User High:", input_high), vjust = 3, hjust = 1.1, color = "darkblue", size = 4)
  }
  
  return(p)
}

# Master function to orchestrate the entire analysis flow
run_analysis <- function(isolated_inputs, data_reactive, output, session, selected_dir_reactive, message_rv) {
  
  # Filter data based on gender and age
  filtered_data <- filter_data(data_reactive(),
                               isolated_inputs$gender_choice,
                               isolated_inputs$age_range[1],
                               isolated_inputs$age_range[2],
                               isolated_inputs$col_gender,
                               isolated_inputs$col_age)

  # Check if data is available after filtering
  if (nrow(filtered_data) == 0) {
    stop("No data remains after filtering. Adjust filters or check data.")
  }

  # Run RefineR analysis
  # Assumes 'run_refiner' and 'extract_intervals' functions are available
  refiner_result <- refineR::RefineR(filtered_data[[isolated_inputs$col_value]], modelSelection = TRUE, nBootstraps = 1)
  reference_intervals <- refineR::getPercentileCI(refiner_result, percentiles = c(2.5, 97.5))

  # Render results
  output$result_text <- renderPrint({
    if (!is.null(reference_intervals)) {
      cat("Reference Interval:\n")
      print(reference_intervals)
    } else {
      cat("No reference interval could be determined.\n")
    }
    if (!is.null(refiner_result$details)) {
      cat("\nRefineR Details:\n")
      print(refiner_result$details)
    }
  })

  # Generate plot
  output$result_plot <- renderPlot({
    plot_refiner_output(
      df = filtered_data,
      value_col_name = isolated_inputs$col_value,
      unit = isolated_inputs$unit_input,
      input_low = isolated_inputs$ref_low,
      input_high = isolated_inputs$ref_high
    )
  })

  # Auto-save plot if enabled and directory selected
  if (input$enable_directory && !is.null(selected_dir_reactive())) {
    save_plot_to_directory(
      plot_data = list(
        df = filtered_data,
        value_col_name = isolated_inputs$col_value,
        unit = isolated_inputs$unit_input,
        input_low = isolated_inputs$ref_low,
        input_high = isolated_inputs$ref_high
      ),
      output_dir = selected_dir_reactive(),
      file_prefix = "RefineR_Plot"
    )
    message_rv(list(text = paste0("Plot saved to ", selected_dir_reactive()), type = "success"))
  }
}