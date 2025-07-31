# R/server/data_observers.R

initialize_data_observers <- function(input, output, session, data_reactive, message_rv, clear_messages) {

  # Observer for file input: reads the uploaded Excel file into data_reactive
  observeEvent(input$data_file, {
    req(input$data_file)
    data <- read_excel(input$data_file$datapath)
    data_reactive(data) # Update the reactive value with the new data
    message_rv(list(type = "success", text = "Data file uploaded and loaded successfully."))
    print("Data file uploaded and loaded.")

    col_names <- colnames(data)

    # Add "None" as a selectable option at the beginning of the choices list for optional columns
    all_col_choices_with_none <- c("None" = "", col_names)

    # Helper to try and guess a default selection for a column // AI-generated
    guess_column <- function(cols_available, common_names) {
      for (name in common_names) {
        match_idx <- grep(paste0("^", name, "$"), cols_available, ignore.case = TRUE)
        if (length(match_idx) > 0) {
          return(cols_available[match_idx[1]])
        }
      }
      return("") # Return "" (for "None") if no guess found
    }

    # Update 'Values' column selector // AI-generated
    # This now also gets a "None" option and defaults to it if no guess.
    default_value_col <- guess_column(col_names, c("HB_value", "Value", "Result", "Measurement", "Waarde"))
    updateSelectInput(session, "col_value",
                      choices = all_col_choices_with_none, # Include "None"
                      selected = default_value_col) # Will default to "" (None) if no guess

    # Update 'Age' column selector
    default_age_col <- guess_column(col_names, c("leeftijd", "age", "AgeInYears", "Years"))
    updateSelectInput(session, "col_age",
                      choices = all_col_choices_with_none, # Include "None"
                      selected = default_age_col)

    # Update 'Gender' column selector
    default_gender_col <- guess_column(col_names, c("geslacht", "gender", "sex", "Gender", "Sex"))
    updateSelectInput(session, "col_gender",
                      choices = all_col_choices_with_none, # Include "None"
                      selected = default_gender_col)

  })

  # Observer for reset button: clears file input and reactive data // AI-generated
  observeEvent(input$reset_btn, {
    shinyjs::reset("data_file")
    data_reactive(NULL)
    clear_messages(message_rv)
    output$result_text <- renderPrint({ cat("") })
    output$result_plot <- renderPlot(plot.new())

    # Reset column selectors, defaulting all to "None"
    updateSelectInput(session, "col_value", choices = c("None" = ""), selected = "")
    updateSelectInput(session, "col_age", choices = c("None" = ""), selected = "")
    updateSelectInput(session, "col_gender", choices = c("None" = ""), selected = "")

    print("App reset: Data cleared.")
  })

}