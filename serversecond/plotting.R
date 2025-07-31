library(ggplot2)

plot_ri_bars <- function(df_summary) {
  ggplot(df_summary, aes(x = subgroup, ymin = lower, ymax = upper)) +
    geom_errorbar(width = 0.2) +
    geom_point(aes(y = mean), size = 3) +
    theme_minimal() +
    labs(title = "Estimated Reference Intervals by Subgroup",
         y = "HGB (g/dL)", x = "Subgroup")
}

plot_age_hgb <- function(df, age, hgb) { # Parameter names reflect usage in aes()
  # Check if columns exist before plotting
  if (!age %in% colnames(df) || !hgb %in% colnames(df)) {
    warning(paste("Columns '", age, "' or '", hgb, "' not found in data frame for plotting.", sep=""))
    return(ggplot() + theme_void() + geom_text(aes(x=0.5, y=0.5, label="Missing age or HGB column for plot.")))
  }
  
  ggplot(df, aes(x = .data[[age]], y = .data[[hgb]], color = factor(cluster))) + # Use .data[[]] for dynamic column names
    geom_point(alpha = 0.7) +
    theme_minimal() +
    labs(title = "HGB vs Age by Cluster",
         x = age, y = hgb, color = "Cluster") # Label the axes with the actual column names
}