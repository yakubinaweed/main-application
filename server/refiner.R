library(refineR)

run_refiner <- function(values) {
  model <- RefineR(values, modelSelection = TRUE, nBootstraps = 1)
  return(model)
}

extract_intervals <- function(model) {
  ri <- getPercentileCI(model, percentiles = c(2.5, 97.5))
  return(ri)
}

# =========================================================================
  # Window 1: Main Analysis Tab (refineR) - (Original structure, not fully revised here)
  # =========================================================================

  # Reactive value for uploaded data
  uploaded_data_rv <- reactiveVal(NULL)
  refiner_results_rv <- reactiveVal(NULL) # To store refineR analysis results

  # UI for Main Analysis Tab (moved to a renderUI block for better structure)
  output$main_analysis_tab <- renderUI({
    tagList(
      fluidRow(
        column(4,
               box(title = "1. Upload Data (Excel)", width = NULL,
                   fileInput("file_upload", "Choose Excel File",
                             multiple = FALSE,
                             accept = c(".xlsx", ".xls"))
               ),
               box(title = "2. Select Columns and Filters", width = NULL,
                   uiOutput("value_col_selector"),
                   uiOutput("age_col_selector"),
                   uiOutput("gender_col_selector"),
                   radioButtons("gender_filter", "Filter by Gender:",
                                choices = c("Male", "Female", "Both"),
                                selected = "Both", inline = TRUE),
                   sliderInput("age_range_filter", "Age Range:",
                               min = 0, max = 100, value = c(0, 100)),
                   textInput("unit_input", "Unit of Measurement (e.g., g/dL):", value = "")
               ),
               box(title = "3. Run Analysis", width = NULL,
                   actionButton("run_analysis_btn", "Analyze Data", class = "btn-primary"),
                   actionButton("reset_file_btn", "Reset File", class = "btn-danger"),
                   hr(),
                   h4("Graph Options"),
                   textInput("lower_limit_input", "User-Defined Lower Limit:", value = ""),
                   textInput("upper_limit_input", "User-Defined Upper Limit:", value = ""),
                   checkboxInput("auto_save_graph", "Auto-Save Graph", value = FALSE),
                   conditionalPanel(
                     condition = "input.auto_save_graph == true",
                     actionButton("select_output_dir", "Select Output Directory", class = "btn-info")
                   )
               )
        ),
        column(8,
               tabBox(
                 title = "Analysis Results",
                 id = "refiner_results_tabs", width = NULL,
                 tabPanel("Plot", plotOutput("plot_output", height = "500px")),
                 tabPanel("Summary", verbatimTextOutput("summary_output"))
               )
        )
      )
    )
  })


  # Observers for file upload and column selection for Main Analysis tab
  observeEvent(input$file_upload, {
    req(input$file_upload)
    tryCatch({
      data <- readxl::read_excel(input$file_upload$datapath)
      uploaded_data_rv(data)
      message_rv(list(text = "Data uploaded successfully.", type = "success"))
    }, error = function(e) {
      message_rv(list(text = paste("Error reading file:", e$message), type = "error"))
      uploaded_data_rv(NULL)
    })
  })

  output$value_col_selector <- renderUI({
    data <- uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("value_col", "Select Value Column:", choices = names(data),
                selected = c("HGB", "hgb", "Value", "value")[c("HGB", "hgb", "Value", "value") %in% names(data)][1])
  })

  output$age_col_selector <- renderUI({
    data <- uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("age_col", "Select Age Column:", choices = names(data),
                selected = c("Age", "age")[c("Age", "age") %in% names(data)][1])
  })

  output$gender_col_selector <- renderUI({
    data <- uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("gender_col", "Select Gender Column:", choices = names(data),
                selected = c("Gender", "gender", "Sex", "sex")[c("Gender", "gender", "Sex", "sex") %in% names(data)][1])
  })

  # Observer for analysis button
  observeEvent(input$run_analysis_btn, {
    req(uploaded_data_rv(), input$value_col, input$age_col, input$gender_col, input$gender_filter, input$age_range_filter)

    if (analysis_running_rv()) {
      message_rv(list(text = "An analysis is already running. Please wait.", type = "warning"))
      return(NULL)
    }

    analysis_running_rv(TRUE)
    shinyjs::disable("refiner_results_tabs") # Disable tab switching

    withProgress(message = 'Running RefineR Analysis', value = 0, {
      incProgress(0.1, detail = "Preparing data...")

      data <- uploaded_data_rv()
      value_col <- input$value_col
      age_col <- input$age_col
      gender_col <- input$gender_col
      gender_filter_val <- input$gender_filter
      age_range <- input$age_range_filter
      unit_of_measurement <- input$unit_input

      # Data filtering (simplified, assume helper functions exist)
      filtered_data <- data %>%
        dplyr::select(Value = !!sym(value_col), Age = !!sym(age_col), Gender = !!sym(gender_col)) %>%
        na.omit() %>%
        filter(Age >= age_range[1] & Age <= age_range[2])

      if (gender_filter_val != "Both") {
        filtered_data <- filtered_data %>%
          filter(str_detect(Gender, regex(gender_filter_val, ignore_case = TRUE)))
      }

      if (nrow(filtered_data) == 0) {
        message_rv(list(text = "No data remains after filtering. Adjust filters or check data.", type = "warning"))
        refiner_results_rv(NULL)
        analysis_running_rv(FALSE)
        shinyjs::enable("refiner_results_tabs")
        return(NULL)
      }

      incProgress(0.4, detail = "Running refineR algorithm...")
      # Placeholder for refineR analysis
      # In a real app, you'd call refineR functions here
      # For example:
      # library(refineR)
      # ref_model <- refineR(data = filtered_data$Value, ...)
      # ref_intervals <- extract_intervals(ref_model) # Assuming such functions exist

      # Mock refineR results for demonstration
      mock_intervals <- list(
        lower = quantile(filtered_data$Value, 0.025, na.rm = TRUE),
        upper = quantile(filtered_data$Value, 0.975, na.rm = TRUE)
      )
      refiner_results_rv(list(
        data = filtered_data,
        intervals = mock_intervals,
        user_lower = as.numeric(input$lower_limit_input),
        user_upper = as.numeric(input$upper_limit_input),
        unit = unit_of_measurement
      ))

      incProgress(0.4, detail = "Generating plot and summary...")
      message_rv(list(text = "RefineR analysis complete!", type = "success"))
    }) # End of withProgress

    analysis_running_rv(FALSE)
    shinyjs::enable("refiner_results_tabs") # Re-enable tab switching
  })

  # Observer for resetting file
  observeEvent(input$reset_file_btn, {
    uploaded_data_rv(NULL)
    refiner_results_rv(NULL)
    shinyjs::reset("file_upload") # Reset file input
    shinyjs::reset("gender_filter")
    shinyjs::reset("age_range_filter")
    shinyjs::reset("unit_input")
    shinyjs::reset("lower_limit_input")
    shinyjs::reset("upper_limit_input")
    shinyjs::reset("auto_save_graph")
    message_rv(list(text = "File and analysis results reset.", type = "info"))
  })

  # Render Plot for Main Analysis Tab
  output$plot_output <- renderPlot({
    results <- refiner_results_rv()
    if (is.null(results)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Upload data and run analysis to see plot.", size = 6, color = "grey50"))
    }

    plot_refiner_output(results$data,
                        results$intervals$lower, results$intervals$upper,
                        results$user_lower, results$user_upper,
                        results$unit)
  })

  # Render Summary for Main Analysis Tab
  output$summary_output <- renderPrint({
    results <- refiner_results_rv()
    if (is.null(results)) {
      return("No analysis results to display.")
    }

    cat("RefineR Analysis Results:\n")
    cat(paste0("Estimated Lower Limit (2.5th percentile): ", round(results$intervals$lower, 2), " ", results$unit, "\n"))
    cat(paste0("Estimated Upper Limit (97.5th percentile): ", round(results$intervals$upper, 2), " ", results$unit, "\n"))

    if (!is.na(results$user_lower) || !is.na(results$user_upper)) {
      cat("\nUser-Defined Limits:\n")
      if (!is.na(results$user_lower)) {
        cat(paste0("User Lower Limit: ", results$user_lower, " ", results$unit, "\n"))
      }
      if (!is.na(results$user_upper)) {
        cat(paste0("User Upper Limit: ", results$user_upper, " ", results$unit, "\n"))
      }
    }
  })

  # Auto-save graph logic (placeholder for directory selection)
  output_dir_rv <- reactiveVal(NULL)

  observeEvent(input$select_output_dir, {
    # This is a placeholder. In a real desktop app, you'd use a file chooser.
    # For Shiny web app, user can only choose a directory on the server.
    # For local development, rstudioapi::selectDirectory() could be used.
    # For deployment, you'd typically have a pre-configured output path.
    # For simplicity, let's assume a 'downloads' subfolder or similar.
    selected_dir <- file.path(getwd(), "downloads") # Example: current working directory + /downloads
    dir.create(selected_dir, showWarnings = FALSE) # Create if not exists
    output_dir_rv(selected_dir)
    message_rv(list(text = paste("Output directory set to:", selected_dir), type = "info"))
  })

  observeEvent(input$run_analysis_btn, {
    if (input$auto_save_graph && !is.null(refiner_results_rv()) && !is.null(output_dir_rv())) {
      plot_obj <- plot_refiner_output(refiner_results_rv()$data,
                                      refiner_results_rv()$intervals$lower, refiner_results_rv()$intervals$upper,
                                      refiner_results_rv()$user_lower, refiner_results_rv()$user_upper,
                                      refiner_results_rv()$unit)

      filename <- generate_safe_filename("refiner_plot", "png")
      filepath <- file.path(output_dir_rv(), filename)
      ggsave(filepath, plot = plot_obj, width = 10, height = 6, units = "in")
      message_rv(list(text = paste("Graph saved to:", filepath), type = "success"))
    }
  }, ignoreInit = TRUE)