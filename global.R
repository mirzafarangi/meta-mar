# Meta-Mar: Global Settings and Functions
# Version: 4.0.2
# Author: Ashkan Beheshti
# Description: This is a simplified global.R file containing helper functions
#              and global settings for the Meta-Mar application.

# Common functions ----

# Function to format data for display
format_for_display <- function(x) {
  if (is.data.frame(x)) {
    # Format data frame nicely
    paste(capture.output(print(x, width = 80, row.names = FALSE)), collapse = "\n")
  } else if (is.list(x)) {
    # Format list/summary nicely
    paste(capture.output(print(x)), collapse = "\n")
  } else {
    # Default formatting
    paste(capture.output(print(x)), collapse = "\n")
  }
}

# Function to check requirements for meta-analysis models
validate_meta_data <- function(data, model) {
  required_cols <- switch(model,
                          "metacont" = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
                          "metabin" = c("studlab", "event.e", "n.e", "event.c", "n.c"),
                          "metagen" = c("studlab", "TE", "seTE"),
                          "metacor" = c("studlab", "cor", "n")
  )
  
  missing_cols <- setdiff(required_cols, names(data))
  
  if(length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      missing = missing_cols,
      message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    ))
  }
  
  # Check for numerical values where required
  numeric_cols <- switch(model,
                         "metacont" = c("n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
                         "metabin" = c("event.e", "n.e", "event.c", "n.c"),
                         "metagen" = c("TE", "seTE"),
                         "metacor" = c("cor", "n")
  )
  
  invalid_cols <- character(0)
  for(col in numeric_cols) {
    if(!all(is.numeric(data[[col]]))) {
      invalid_cols <- c(invalid_cols, col)
    }
  }
  
  if(length(invalid_cols) > 0) {
    return(list(
      valid = FALSE,
      invalid = invalid_cols,
      message = paste("Non-numeric values found in columns:", paste(invalid_cols, collapse = ", "))
    ))
  }
  
  return(list(
    valid = TRUE,
    message = "Data validation successful"
  ))
}

# Function to create a demo dataset for example purposes
create_demo_data <- function(model = "metacont") {
  if(model == "metacont") {
    data <- data.frame(
      studlab = paste("Study", 1:5),
      n.e = c(50, 45, 60, 90, 70),
      mean.e = c(12.5, 13.2, 14.1, 15.0, 13.7),
      sd.e = c(2.5, 2.8, 3.1, 2.9, 3.0),
      n.c = c(48, 47, 55, 88, 65),
      mean.c = c(10.2, 10.8, 11.5, 11.9, 10.5),
      sd.c = c(2.4, 2.5, 3.0, 3.1, 2.8),
      subgroup = c("A", "A", "B", "B", "B"),
      year = c(2015, 2016, 2018, 2020, 2022),
      quality_score = c(8, 7, 9, 6, 8)
    )
  } else if(model == "metabin") {
    data <- data.frame(
      studlab = paste("Study", 1:5),
      event.e = c(15, 20, 25, 30, 18),
      n.e = c(100, 120, 150, 200, 110),
      event.c = c(10, 15, 20, 25, 12),
      n.c = c(100, 120, 150, 200, 110),
      subgroup = c("A", "A", "B", "B", "B"),
      year = c(2015, 2016, 2018, 2020, 2022),
      quality_score = c(8, 7, 9, 6, 8)
    )
  } else if(model == "metagen") {
    data <- data.frame(
      studlab = paste("Study", 1:5),
      TE = c(0.5, 0.6, 0.3, 0.4, 0.7),
      seTE = c(0.2, 0.15, 0.25, 0.18, 0.22),
      subgroup = c("A", "A", "B", "B", "B"),
      year = c(2015, 2016, 2018, 2020, 2022),
      quality_score = c(8, 7, 9, 6, 8)
    )
  } else if(model == "metacor") {
    data <- data.frame(
      studlab = paste("Study", 1:5),
      cor = c(0.35, 0.42, 0.28, 0.51, 0.39),
      n = c(80, 120, 90, 150, 110),
      subgroup = c("A", "A", "B", "B", "B"),
      year = c(2015, 2016, 2018, 2020, 2022),
      quality_score = c(8, 7, 9, 6, 8)
    )
  }
  
  return(data)
}

# Helper function to prettify meta-analysis output
format_meta_summary <- function(ma) {
  if(is.null(ma)) return("No meta-analysis results available.")
  
  # Extract key information
  model_type <- ma$sm
  num_studies <- ma$k
  
  # Create header
  output <- paste0(
    "META-ANALYSIS SUMMARY\n",
    "==========================================\n",
    "Model: ", ma$method, "\n",
    "Effect measure: ", model_type, "\n",
    "Number of studies: ", num_studies, "\n\n"
  )
  
  # Fixed effect results
  if(!is.null(ma$TE.fixed)) {
    output <- paste0(
      output,
      "FIXED EFFECT MODEL\n",
      "------------------------------------------\n",
      "Effect estimate: ", round(ma$TE.fixed, 4), " (95% CI: ", 
      round(ma$lower.fixed, 4), " to ", round(ma$upper.fixed, 4), ")\n",
      "z = ", round(ma$zval.fixed, 4), ", p = ", 
      format.pval(ma$pval.fixed, digits = 4), "\n\n"
    )
  }
  
  # Random effects results
  if(!is.null(ma$TE.random)) {
    output <- paste0(
      output,
      "RANDOM EFFECTS MODEL\n",
      "------------------------------------------\n",
      "Effect estimate: ", round(ma$TE.random, 4), " (95% CI: ", 
      round(ma$lower.random, 4), " to ", round(ma$upper.random, 4), ")\n",
      "z = ", round(ma$zval.random, 4), ", p = ", 
      format.pval(ma$pval.random, digits = 4), "\n\n"
    )
  }
  
  # Heterogeneity
  output <- paste0(
    output,
    "HETEROGENEITY\n",
    "------------------------------------------\n",
    "τ² = ", round(ma$tau^2, 4), "\n",
    "I² = ", round(ma$I2 * 100, 1), "%\n",
    "H = ", round(ma$H, 2), "\n",
    "Q = ", round(ma$Q, 2), ", df = ", ma$df.Q, ", p = ", 
    format.pval(ma$pval.Q, digits = 4), "\n"
  )
  
  return(output)
}

# Example data for demonstration purposes
example_data <- list(
  continuous = create_demo_data("metacont"),
  binary = create_demo_data("metabin"),
  generic = create_demo_data("metagen"),
  correlation = create_demo_data("metacor")
)