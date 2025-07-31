# R/server/output_renderers.R

# --- Centralized Message Display ---
render_app_message <- function(output, message_rv) {
  output$app_message <- renderUI({
    msg <- message_rv()
    if (msg$text == "") {
      return(NULL) # Don't display anything if no message
    }

    # Determine class based on message type for styling
    class_name <- switch(msg$type,
                         "error" = "alert alert-danger",
                         "success" = "alert alert-success",
                         "warning" = "alert alert-warning",
                         "info" = "alert alert-info",
                         "alert alert-secondary") # Default

    div(class = class_name, msg$text)
  })
}

# Helper to clear messages
clear_messages <- function(message_rv) {
  message_rv(list(type = "", text = ""))
}

# Helper to display analysis-related errors consistently
display_analysis_error <- function(output, message_rv, error_text) {
  message_rv(list(type = "error", text = error_text))
  output$result_text <- renderPrint({
    cat(error_text)
  })
  output$result_plot <- renderPlot(plot.new()) # Clear the plot
}
# --- End Centralized Message Display ---


# Function to render the text output of the refineR result
render_results_text <- function(output, result) {
  output$result_text <- renderPrint({
    print(result, RIperc = c(0.025, 0.975))
  })
  print("Text results rendered.")
}

# Function to render the plot output
render_results_plot <- function(output, result, input, plot_title, col_value, units) {
  output$result_plot <- renderPlot({
    plot(result, showCI = TRUE, RIperc = c(0.025, 0.975), showPathol = FALSE,
         title = plot_title,
         xlab = sprintf("%s [%s]", col_value, units),
         ylab = "Frequency")

    # Get plot limits to position text
    usr <- par("usr")
    y_max <- usr[4]
    y_label_pos <- y_max * 0.95

    # Add vertical lines and text for reference limits if provided
    if (!is.na(input$ref_low)) {
      abline(v = input$ref_low, col = "red", lty = 2, lwd = 2)
      text(x = input$ref_low, y = y_label_pos,
           labels = round(input$ref_low, 2),
           col = "red", cex = 1.1, pos = 4)
    }

    if (!is.na(input$ref_high)) {
      abline(v = input$ref_high, col = "blue", lty = 2, lwd = 2)
      text(x = input$ref_high, y = y_label_pos,
           labels = round(input$ref_high, 2),
           col = "blue", cex = 1.1, pos = 2)
    }
  })
  print("Plot rendered to UI.")
}

# Function to save the plot to a file
save_plot_to_file <- function(result, input, selected_dir_reactive, plot_title, col_value, units) {
  selected_directory <- selected_dir_reactive()
  if (is.null(selected_directory)) {
    warning("Auto-save enabled but no directory selected. Plot not saved.")
    return()
  }

  filename <- generate_safe_filename(plot_title, selected_directory)

  png(filename = filename, width = 800, height = 600)
  plot(result, showCI = TRUE, RIperc = c(0.025, 0.975), showPathol = FALSE,
       title = plot_title, xlab = sprintf("%s [%s]", col_value, units),
       ylab = "Frequency")

  usr <- par("usr")
  y_max <- usr[4]
  y_label_pos <- y_max * 0.95

  if (!is.na(input$ref_low)) {
    abline(v = input$ref_low, col = "red", lty = 2, lwd = 2)
    text(x = input$ref_low, y = y_label_pos,
         labels = round(input$ref_low, 2),
         col = "red", cex = 1.1, pos = 4)
  }

  if (!is.na(input$ref_high)) {
    abline(v = input$ref_high, col = "blue", lty = 2, lwd = 2)
    text(x = input$ref_high, y = y_label_pos,
         labels = round(input$ref_high, 2),
         col = "blue", cex = 1.1, pos = 2)
  }
  dev.off()
  print(paste("Plot saved as:", filename))
}