# analysis_observers.R

call_analysis_observer <- function(input, output, session, data_reactive, selected_dir_reactive, analysis_running, message_rv) {

  # Observer for the Analyze button
  observeEvent(input$analyze_btn, {
    # Check if analysis is already running
    if (analysis_running()) {
      message_rv(list(text = "Analysis is already running. Please wait or reset.", type = "warning"))
      return()
    }

    # Input validation
    req(data_reactive()) # Ensure data is loaded
    if (is.null(input$col_value) || input$col_value == "" ||
        is.null(input$col_age) || input$col_age == "" ||
        is.null(input$col_gender) || input$col_gender == "") {
      message_rv(list(text = "Please select all required columns (Value, Age, Gender).", type = "warning"))
      return()
    }

    # Set analysis running flag to TRUE
    analysis_running(TRUE)
    message_rv(list(text = "Analysis started...", type = "info"))

    # Send status to client-side JavaScript for tab disabling
    session$sendCustomMessage('analysisStatus', TRUE)

    # Use isolate to prevent re-running if only inputs change during computation
    isolated_inputs <- isolate({
      list(
        gender_choice = input$gender_choice,
        age_range = input$age_range,
        col_value = input$col_value,
        col_age = input$col_age,
        col_gender = input$col_gender,
        nbootstrap_speed = input$nbootstrap_speed,
        unit_input = input$unit_input,
        ref_low = input$ref_low,
        ref_high = input$ref_high
      )
    })

    tryCatch({
      # Filter data based on gender and age
      filtered_data <- filter_data(data_reactive(),
                                   isolated_inputs$gender_choice,
                                   isolated_inputs$age_range[1],
                                   isolated_inputs$age_range[2],
                                   isolated_inputs$col_gender,
                                   isolated_inputs$col_age)

      # Run RefineR analysis
      refiner_result <- run_refiner_analysis(filtered_data,
                                             isolated_inputs$col_value,
                                             isolated_inputs$nbootstrap_speed)

      # Extract reference intervals
      reference_intervals <- extract_intervals(refiner_result)

      # Z-transform data if reference intervals are available
      z_transformed_data <- NULL
      if (!is.null(reference_intervals) && !is.na(reference_intervals$Lower) && !is.na(reference_intervals$Upper)) {
        z_transformed_data <- z_transform_data(
          data_reactive(), # Use original data for z-transform based on col_value
          isolated_inputs$col_value,
          reference_intervals$Lower,
          reference_intervals$Upper
        )
      }

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
        if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
          plot_refiner_output(
            df = filtered_data,
            value_col_name = isolated_inputs$col_value,
            age_col_name = isolated_inputs$col_age,
            gender_col_name = isolated_inputs$col_gender,
            refiner_output = refiner_result,
            unit = isolated_inputs$unit_input,
            input_low = isolated_inputs$ref_low,
            input_high = isolated_inputs$ref_high,
            z_data = z_transformed_data # Pass z-transformed data for plotting
          )
        } else {
          plot.new() # Clear plot area if no data
          text(0.5, 0.5, "No data to plot after filtering.", cex = 1.5)
        }
      })

      # Auto-save plot if enabled and directory selected
      if (input$enable_directory && !is.null(selected_dir_reactive())) {
        save_plot_to_directory(
          plot_data = list(
            df = filtered_data,
            value_col_name = isolated_inputs$col_value,
            age_col_name = isolated_inputs$col_age,
            gender_col_name = isolated_inputs$col_gender,
            refiner_output = refiner_result,
            unit = isolated_inputs$unit_input,
            input_low = isolated_inputs$ref_low,
            input_high = isolated_inputs$ref_high,
            z_data = z_transformed_data
          ),
          output_dir = selected_dir_reactive(),
          file_prefix = "RefineR_Plot"
        )
        message_rv(list(text = paste0("Plot saved to ", selected_dir_reactive()), type = "success"))
      }

      message_rv(list(text = "Analysis complete!", type = "success"))

    }, error = function(e) {
      message_rv(list(text = paste("Analysis Error:", e$message), type = "danger"))
      # Clear previous results on error
      output$result_text <- renderPrint({ cat("") })
      output$result_plot <- renderPlot(plot.new())
    }, finally = {
      # Set analysis running flag to FALSE regardless of success or failure
      analysis_running(FALSE)
      session$sendCustomMessage('analysisStatus', FALSE) # Update client-side status
    })
  })
}