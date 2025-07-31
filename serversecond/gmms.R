library(mclust)

run_gmm <- function(data_mat, G_range) { # data_mat is now expected to be a multi-column matrix/dataframe
  print("DEBUG (gmms.R): run_gmm function called.")
  print(paste("DEBUG (gmms.R): Input data_mat class:", class(data_mat)))
  print(paste("DEBUG (gmms.R): Input data_mat dimensions:", paste(dim(data_mat), collapse = "x")))
  print(paste("DEBUG (gmms.R): G_range parameter (for optimal G selection):", paste(G_range, collapse = ":")))


  # Ensure numeric and no NAs are already handled upstream by server.R
  # These checks are now more of a safeguard
  if (!is.matrix(data_mat) && !is.data.frame(data_mat)) {
    stop("Input data_mat must be a matrix or data frame for GMM analysis.")
  }
  if (!all(sapply(data_mat, is.numeric))) {
    stop("All columns in data_mat must be numeric.")
  }
  if (any(is.na(data_mat))) {
    stop("Input data_mat contains NA values. Please remove or impute before clustering.")
  }

  # Mclust will automatically choose the best model and G within the given G_range
  # for multivariate data. Common modelNames for multivariate: EII, VII, EEI, VEI, EVI, VVI, EEE, VVE, VEV, EVV, VVV
  # Using a subset of common models for robustness or let Mclust search all
  # For 2D (like HGB and Age), Mclust can choose from a set of common multivariate models.
  multivariate_model_names <- c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "VVE", "VEV", "EVV", "VVV") # Common robust choices for 2D

  tryCatch({
    print(paste("DEBUG (gmms.R): Attempting Mclust fit with G range =", paste(G_range, collapse = ":"), " and modelNames =", paste(multivariate_model_names, collapse = ", ")))
    # Mclust will select optimal G and model from the provided range/list based on BIC
    gmm_model <- Mclust(data_mat, G = G_range, modelNames = multivariate_model_names)
    print("DEBUG (gmms.R): Mclust fit completed. Optimal G chosen:")
    print(gmm_model$G)
    print("DEBUG (gmms.R): Optimal model chosen:")
    print(gmm_model$modelName)

    return(gmm_model)
  }, error = function(e) {
    print(paste("DEBUG ERROR (gmms.R): Error during Mclust fit:", e$message))
    stop(paste("GMM Mclust Error:", e$message)) # Re-throw for upstream handling
  })
}

assign_clusters <- function(df, gmm_model) {
  print("DEBUG (gmms.R): assign_clusters function called.")
  if (is.null(gmm_model) || is.null(gmm_model$classification)) {
    warning("GMM model or classification is NULL. Cannot assign clusters.")
    return(df)
  }
  # It's crucial that 'df' (which is 'data_for_mclust' in server.R now)
  # aligns with the data used for Mclust. If it has NAs removed already, sizes should match.
  if (nrow(df) != length(gmm_model$classification)) {
    warning("Dataframe row count does not match GMM classification length. This might indicate a mismatch if data was filtered after Mclust or passed incorrectly.")
    # More robust assignment would involve matching by row names/IDs if data was heavily filtered
  }
  df$cluster <- gmm_model$classification
  print("DEBUG (gmms.R): Clusters assigned.")
  return(df)
}