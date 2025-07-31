# analysis_observers.R
library(refineR) # Explicitly load refineR here to ensure functions are in scope

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
      
      # Determine nbootstrap_value based on user input
      nbootstrap_value <- switch(isolated_inputs$nbootstrap_speed,
                                 "Fast" = 1,
                                 "Medium" = 50,
                                 "Slow" = 200,
                                 1) # Default to 1 if not matched

      # Run RefineR analysis with the determined nbootstrap_value
      # Correctly call 'findRI' and 'getRI' functions
      refiner_model <- refineR::findRI(Data = filtered_data[[isolated_inputs$col_value]], NBootstrap = nbootstrap_value)
      reference_intervals <- refineR::getRI(refiner_model, RIperc = c(0.025, 0.975))

      # Render results
      output$result_text <- renderPrint({
        if (!is.null(reference_intervals)) {
          cat("Reference Interval:\n")
          print(reference_intervals)
        } else {
          cat("No reference interval could be determined.\n")
        }
        if (!is.null(refiner_model$details)) {
          cat("\nRefineR Details:\n")
          print(refiner_model$details)
        }
      })

      # Generate plot
      output$result_plot <- renderPlot({
        if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
          # Use a general plotting function that accepts the `refiner_model` output
          # You may need to create a `plot_refiner_output` function to handle this
          # For now, using the base `plot` method
          plot(refiner_model, showCI = TRUE, RIperc = c(0.025, 0.975), showPathol = FALSE,
               title = paste("Estimated Reference Intervals"),
               xlab = sprintf("%s [%s]", isolated_inputs$col_value, isolated_inputs$unit_input))

          # Add user-defined lines if they exist
          if (!is.na(isolated_inputs$ref_low)) { abline(v = isolated_inputs$ref_low, col = "red", lty = 2) }
          if (!is.na(isolated_inputs$ref_high)) { abline(v = isolated_inputs$ref_high, col = "blue", lty = 2) }
          
        } else {
          plot.new() # Clear plot area if no data
          text(0.5, 0.5, "No data to plot after filtering.", cex = 1.5)
        }
      })

      # Auto-save plot if enabled and directory selected
      if (input$enable_directory && !is.null(selected_dir_reactive())) {
        # This part assumes a save function that can handle the refiner_model object
        # You may need to create a custom save function or use the base R plotting to save
        filename <- generate_safe_filename("RefineR_Plot", selected_dir_reactive(), "png")
        png(filename, width = 800, height = 600)
        plot(refiner_model, showCI = TRUE, RIperc = c(0.025, 0.975), showPathol = FALSE,
             title = paste("Estimated Reference Intervals"),
             xlab = sprintf("%s [%s]", isolated_inputs$col_value, isolated_inputs$unit_input))
        if (!is.na(isolated_inputs$ref_low)) { abline(v = isolated_inputs$ref_low, col = "red", lty = 2) }
        if (!is.na(isolated_inputs$ref_high)) { abline(v = isolated_inputs$ref_high, col = "blue", lty = 2) }
        dev.off()
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