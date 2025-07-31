# R/server.R (Complete Script)

library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse) # Includes dplyr, ggplot2, tibble, etc.
library(mclust) # For GMM analysis
library(moments) # For skewness calculation
library(shinyjs) # For UI manipulations
library(car) # Required for powerTransform (Yeo-Johnson)
library(refineR) # Explicitly load refineR here

# Source utility R files (ensure these paths are correct relative to server.R)
# Corrected paths: they should be relative to the R/ directory where server.R resides
source(file.path("serversecond", "gmms.R"), local = TRUE)
source(file.path("serversecond", "plotting.R"), local = TRUE)
source(file.path("serversecond", "data_prep.R"), local = TRUE)
source(file.path("server", "analysis_observers.R"), local = TRUE)
source(file.path("server", "data_observers.R"), local = TRUE)
source(file.path("server", "file_observers.R"), local = TRUE)
source(file.path("server", "reactive_values.R"), local = TRUE)
source(file.path("server", "output_renderers.R"), local = TRUE)
source(file.path("server", "refiner.R"), local = TRUE)
source(file.path("utils.R"), local = TRUE)


# Reactive value for analysis status
analysis_running_rv <- reactiveVal(FALSE)

# Reactive value for messages
message_rv <- reactiveVal(list(text = "Welcome!", type = "info"))


server <- function(input, output, session) {

  # =========================================================================
  # Global Observers and Reactive Values
  # =========================================================================

  # Observer to prevent tab switching while analysis is running
  observeEvent(input$tabs, {
    if (analysis_running_rv()) {
      message_rv(list(text = "Please wait, an analysis is currently running. Tab switching is disabled.", type = "warning"))
      # Revert to the previous tab if an analysis is running
      # This requires storing the last active tab, which is more complex.
      # For now, just display a message and disable the tabs.
    }
  }, ignoreInit = TRUE)

  # Display messages
  output$message_output <- renderUI({
    msg <- message_rv()
    if (!is.null(msg$text)) {
      class_name <- switch(msg$type,
                           "info" = "alert alert-info",
                           "success" = "alert alert-success",
                           "warning" = "alert alert-warning",
                           "error" = "alert alert-danger")
      div(class = class_name, msg$text)
    }
  })


  # Reactive values for main analysis
  analysis_running <- reactiveVal(FALSE)
  current_tab <- reactiveVal("Main Analysis") # Tracks the currently selected tab
  data_reactive <- reactiveVal(NULL)
  selected_dir_reactive <- reactiveVal(NULL)
  message_rv <- reactiveVal(NULL)

  # --- Shared/Centralized Logic ---
  # Centralized message display
  render_app_message(output, message_rv)

  # Call your main analysis observer
  call_analysis_observer(input, output, session, data_reactive, selected_dir_reactive, analysis_running, message_rv)

  # =========================================================================
  # Window 2: Subpopulation Detection (GMM) - REVISED
  # =========================================================================

  # Reactive value to store GMM data (uploaded and processed)
  gmm_uploaded_data_rv <- reactiveVal(NULL)
  gmm_processed_data_rv <- reactiveVal(NULL)
  gmm_transformation_details_rv <- reactiveVal(list(male_hgb_transformed = FALSE, female_hgb_transformed = FALSE))

  # Observers for file upload and column selection for GMM tab
  observeEvent(input$gmm_file_upload, {
    req(input$gmm_file_upload)
    tryCatch({
      data <- readxl::read_excel(input$gmm_file_upload$datapath)
      gmm_uploaded_data_rv(data)
      message_rv(list(text = "GMM data uploaded successfully.", type = "success"))
    }, error = function(e) {
      message_rv(list(text = paste("Error reading GMM file:", e$message), type = "error"))
      gmm_uploaded_data_rv(NULL)
    })
  })

  output$gmm_hgb_col_selector <- renderUI({
    data <- gmm_uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("gmm_hgb_col", "Select HGB Column:", choices = names(data),
                selected = c("HGB", "hgb", "HB", "hb")[c("HGB", "hgb", "HB", "hb") %in% names(data)][1])
  })

  output$gmm_age_col_selector <- renderUI({
    data <- gmm_uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("gmm_age_col", "Select Age Column:", choices = names(data),
                selected = c("Age", "age")[c("Age", "age") %in% names(data)][1])
  })

  output$gmm_gender_col_selector <- renderUI({
    data <- gmm_uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("gmm_gender_col", "Select Gender Column:", choices = names(data),
                selected = c("Gender", "gender", "Sex", "sex")[c("Gender", "gender", "Sex", "sex") %in% names(data)][1])
  })


  # Observer for GMM analysis button - REVISED LOGIC
  observeEvent(input$run_gmm_analysis_btn, {
    req(gmm_uploaded_data_rv(), input$gmm_hgb_col, input$gmm_age_col, input$gmm_gender_col)

    if (analysis_running_rv()) {
      message_rv(list(text = "An analysis is already running. Please wait.", type = "warning"))
      return(NULL)
    }

    # Set analysis running flag and block tab switching
    analysis_running_rv(TRUE)
    shinyjs::disable("gmm_results_tabs") # Disable tab switching during analysis

    # Show progress modal
    withProgress(message = 'Running GMM Analysis', value = 0, {
      incProgress(0.1, detail = "Loading data...")

      data <- gmm_uploaded_data_rv()
      hgb_col <- input$gmm_hgb_col
      age_col <- input$gmm_age_col
      gender_col <- input$gmm_gender_col

      if (!all(c(hgb_col, age_col, gender_col) %in% names(data))) {
        message_rv(list(text = "Selected columns not found in data. Please check selections.", type = "error"))
        analysis_running_rv(FALSE)
        shinyjs::enable("gmm_results_tabs")
        return(NULL)
      }

      # Prepare data for GMM
      gmm_data <- data %>%
        dplyr::select(HGB = !!sym(hgb_col), Age = !!sym(age_col), Gender_orig = !!sym(gender_col)) %>% # Keep original gender name
        mutate(original_row_index = row_number()) %>% # Keep original index for rejoining if needed
        na.omit() # Remove rows with any NA in selected columns

      if (nrow(gmm_data) == 0) {
        message_rv(list(text = "No complete rows for GMM after NA removal. Check data or selections.", type = "error"))
        analysis_running_rv(FALSE)
        shinyjs::enable("gmm_results_tabs")
        return(NULL)
      }

      incProgress(0.1, detail = "Applying universal plausibility limits...")
      # Apply universal plausibility limits (placeholder call)
      # Assuming apply_universal_plausibility_limits is in data_prep.R
      gmm_data <- apply_universal_plausibility_limits(gmm_data)
      if (nrow(gmm_data) == 0) {
        message_rv(list(text = "No data remains after applying plausibility limits.", type = "warning"))
        analysis_running_rv(FALSE)
        shinyjs::enable("gmm_results_tabs")
        return(NULL)
      }

      incProgress(0.1, detail = "Splitting data by gender and transforming...")

      # Normalize gender column to "Male" and "Female"
      gmm_data <- gmm_data %>%
        mutate(Gender = case_when(
          str_detect(Gender_orig, regex("male|m", ignore_case = TRUE)) ~ "Male",
          str_detect(Gender_orig, regex("female|f", ignore_case = TRUE)) ~ "Female",
          TRUE ~ "Other" # Label other values as 'Other'
        )) %>%
        filter(Gender %in% c("Male", "Female")) # Only proceed with Male/Female for this analysis

      male_data <- gmm_data %>% filter(Gender == "Male")
      female_data <- gmm_data %>% filter(Gender == "Female")

      combined_clustered_data <- tibble()
      male_hgb_transformed_flag <- FALSE
      female_hgb_transformed_flag <- FALSE

      # Process Male Data
      if (nrow(male_data) > 0) {
        message_rv(list(text = paste0("Processing ", nrow(male_data), " male records."), type = "info"))
        # Apply conditional Yeo-Johnson to HGB for males
        # Now using the function from yeo_johnson.R
        yj_result_male <- apply_conditional_yeo_johnson(male_data$HGB)
        male_data$HGB_transformed <- yj_result_male$transformed_data
        male_hgb_transformed_flag <- yj_result_male$transformation_applied

        # Z-standardize HGB (transformed) and Age for males
        male_data$HGB_z <- z_transform(male_data$HGB_transformed)
        male_data$Age_z <- z_transform(male_data$Age)

        incProgress(0.2, detail = "Running GMM for Male data...")
        tryCatch({
          # GMM runs on the z-transformed data
          male_gmm_model <- run_gmm(male_data %>% dplyr::select(HGB = HGB_z, Age = Age_z))
          male_data <- assign_clusters(male_data, male_gmm_model)
          # Ensure cluster is a factor for plotting
          male_data$cluster <- as.factor(male_data$cluster)
          message_rv(list(text = "GMM for male data complete.", type = "success"))
        }, error = function(e) {
          message_rv(list(text = paste("Error running GMM for male data:", e$message), type = "error"))
        })
        # Select relevant columns and add to combined, keeping original HGB for plotting
        combined_clustered_data <- bind_rows(combined_clustered_data,
                                             male_data %>% dplyr::select(HGB, Age, Gender, cluster))
      } else {
        message_rv(list(text = "No male data to process.", type = "warning"))
      }


      # Process Female Data
      if (nrow(female_data) > 0) {
        message_rv(list(text = paste0("Processing ", nrow(female_data), " female records."), type = "info"))
        # Apply conditional Yeo-Johnson to HGB for females
        # Now using the function from yeo_johnson.R
        yj_result_female <- apply_conditional_yeo_johnson(female_data$HGB)
        female_data$HGB_transformed <- yj_result_female$transformed_data
        female_hgb_transformed_flag <- yj_result_female$transformation_applied

        # Z-standardize HGB (transformed) and Age for females
        female_data$HGB_z <- z_transform(female_data$HGB_transformed)
        female_data$Age_z <- z_transform(female_data$Age)

        incProgress(0.2, detail = "Running GMM for Female data...")
        tryCatch({
          # GMM runs on the z-transformed data
          female_gmm_model <- run_gmm(female_data %>% dplyr::select(HGB = HGB_z, Age = Age_z))
          female_data <- assign_clusters(female_data, female_gmm_model)
          # Ensure cluster is a factor for plotting
          female_data$cluster <- as.factor(female_data$cluster)
          message_rv(list(text = "GMM for female data complete.", type = "success"))
        }, error = function(e) {
          message_rv(list(text = paste("Error running GMM for female data:", e$message), type = "error"))
        })
        # Select relevant columns and add to combined, keeping original HGB for plotting
        combined_clustered_data <- bind_rows(combined_clustered_data,
                                             female_data %>% dplyr::select(HGB, Age, Gender, cluster))
      } else {
        message_rv(list(text = "No female data to process.", type = "warning"))
      }

      # Store transformation details
      gmm_transformation_details_rv(list(male_hgb_transformed = male_hgb_transformed_flag,
                                         female_hgb_transformed = female_hgb_transformed_flag))

      if (nrow(combined_clustered_data) > 0) {
        gmm_processed_data_rv(combined_clustered_data)
        message_rv(list(text = "GMM analysis complete!", type = "success"))
      } else {
        message_rv(list(text = "No data available after GMM processing for plotting/summary.", type = "error"))
        gmm_processed_data_rv(NULL)
      }

      incProgress(0.1, detail = "Generating plots and summaries...")
    }) # End of withProgress

    analysis_running_rv(FALSE)
    shinyjs::enable("gmm_results_tabs") # Re-enable tab switching

  }) # End of observeEvent for run_gmm_analysis_btn


  # Observer for resetting GMM tab
  observeEvent(input$reset_gmm_analysis_btn, {
    gmm_uploaded_data_rv(NULL)
    gmm_processed_data_rv(NULL)
    gmm_transformation_details_rv(list(male_hgb_transformed = FALSE, female_hgb_transformed = FALSE))
    # Reset UI elements visually
    shinyjs::reset("gmm_file_upload")
    message_rv(list(text = "GMM data and results reset.", type = "info"))
  })


  # Render GMM Plot
  output$plot_output_gmm <- renderPlot({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No GMM data available for plotting.", size = 6, color = "grey50"))
    }

    # Pass transformation flags to the plotting function for title/notes
    plot_age_hgb(plot_data,
                 male_hgb_transformed = gmm_transformation_details_rv()$male_hgb_transformed,
                 female_hgb_transformed = gmm_transformation_details_rv()$female_hgb_transformed)
  })


  # Render GMM Summary (updated to handle gender-split summary)
  output$gmm_summary_output <- renderPrint({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return("No GMM analysis results to display.")
    }

    # Summarize for Male data
    male_summary <- plot_data %>%
      filter(Gender == "Male") %>%
      group_by(cluster) %>%
      summarise(
        Proportion = n() / nrow(.) * 100,
        Mean_HGB = mean(HGB, na.rm = TRUE),
        SD_HGB = sd(HGB, na.rm = TRUE),
        Mean_Age = mean(Age, na.rm = TRUE),
        SD_Age = sd(Age, na.rm = TRUE),
        Min_Age = min(Age, na.rm = TRUE),
        Max_Age = max(Age, na.rm = TRUE)
      ) %>%
      mutate_if(is.numeric, ~round(., 2))

    # Summarize for Female data
    female_summary <- plot_data %>%
      filter(Gender == "Female") %>%
      group_by(cluster) %>%
      summarise(
        Proportion = n() / nrow(.) * 100,
        Mean_HGB = mean(HGB, na.rm = TRUE),
        SD_HGB = sd(HGB, na.rm = TRUE),
        Mean_Age = mean(Age, na.rm = TRUE),
        SD_Age = sd(Age, na.rm = TRUE),
        Min_Age = min(Age, na.rm = TRUE),
        Max_Age = max(Age, na.rm = TRUE)
      ) %>%
      mutate_if(is.numeric, ~round(., 2))

    cat("--- GMM Analysis Summary (Male Subpopulations) ---\n")
    if (nrow(male_summary) > 0) {
      print(male_summary)
    } else {
      cat("No male subpopulations detected.\n")
    }

    cat("\n--- GMM Analysis Summary (Female Subpopulations) ---\n")
    if (nrow(female_summary) > 0) {
      print(female_summary)
    } else {
      cat("No female subpopulations detected.\n")
    }

    # Add a note about transformation if applied
    if (gmm_transformation_details_rv()$male_hgb_transformed || gmm_transformation_details_rv()$female_hgb_transformed) {
      cat("\nNote: HGB values were transformed (Yeo-Johnson) for GMM input due to skewness. Reported HGB values are original.\n")
    }
  })


  # Render GMM Cluster Age Group Summary (updated for gender)
  output$gmm_age_group_summary_output <- renderTable({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(NULL)
    }

    # Predefined age bins for reporting (ensure these are consistent with any other use)
    age_bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf)
    age_labels <- c("<10", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

    plot_data %>%
      mutate(age_group_label = cut(Age, breaks = age_bins, labels = age_labels, right = FALSE)) %>%
      group_by(Gender, age_group_label, cluster) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = cluster, values_from = Count, values_fill = 0)
  }, rownames = FALSE)

}