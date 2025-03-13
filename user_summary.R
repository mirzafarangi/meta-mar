# Meta-Mar: User Summary Functions
# Version: 4.0.2
# Author: Ashkan Beheshti
# Description: This file contains functions to generate and format user summaries
#              for meta-analysis results.

# Initialize user summary structure
init_user_summary <- function() {
  reactive({
    list(
      # Model settings
      model = list(
        type = NULL,
        measure = NULL,
        method_tau = NULL,
        method_ci = NULL,
        method_predict = NULL,
        method_tau_ci = NULL,
        smd_method = NULL
      ),
      
      # Meta-analysis results
      results = list(
        common = list(
          estimate = NULL,
          ci_lower = NULL,
          ci_upper = NULL,
          z_value = NULL,
          p_value = NULL
        ),
        random = list(
          estimate = NULL,
          ci_lower = NULL,
          ci_upper = NULL,
          z_value = NULL,
          p_value = NULL
        ),
        heterogeneity = list(
          tau_squared = NULL,
          i_squared = NULL,
          h = NULL,
          q = NULL,
          df = NULL,
          p_value = NULL
        ),
        prediction = list(
          lower = NULL,
          upper = NULL
        ),
        studies = NULL
      ),
      
      # Publication bias results
      bias = list(
        egger = list(
          statistic = NULL,
          p_value = NULL
        ),
        trim_fill = list(
          added = NULL,
          original_effect = NULL,
          adjusted_effect = NULL
        )
      ),
      
      # Meta-regression results
      metareg = list(
        variables = NULL,
        coefficients = NULL,
        p_values = NULL,
        r_squared = NULL
      )
    )
  })
}

# Update model settings in the user summary
update_model_settings <- function(user_summary, input) {
  user_data <- user_summary()
  
  user_data$model$type <- input$model
  user_data$model$measure <- input$sm
  user_data$model$method_tau <- input$method.tau
  user_data$model$method_ci <- input$method.random.ci
  user_data$model$method_predict <- input$method.predict
  user_data$model$method_tau_ci <- input$method.tau.ci
  
  if (!is.null(input$smd_method)) {
    user_data$model$smd_method <- input$smd_method
  }
  
  user_summary(user_data)
  return(user_summary)
}

# Update meta-analysis results in the user summary
update_meta_analysis_results <- function(user_summary, ma) {
  user_data <- user_summary()
  
  # Fixed/Common effect results
  if (!is.null(ma$TE.fixed)) {
    user_data$results$common$estimate <- ma$TE.fixed
    user_data$results$common$ci_lower <- ma$lower.fixed
    user_data$results$common$ci_upper <- ma$upper.fixed
    user_data$results$common$z_value <- ma$zval.fixed
    user_data$results$common$p_value <- ma$pval.fixed
  }
  
  # Random effects results
  if (!is.null(ma$TE.random)) {
    user_data$results$random$estimate <- ma$TE.random
    user_data$results$random$ci_lower <- ma$lower.random
    user_data$results$random$ci_upper <- ma$upper.random
    user_data$results$random$z_value <- ma$zval.random
    user_data$results$random$p_value <- ma$pval.random
  }
  
  # Heterogeneity
  user_data$results$heterogeneity$tau_squared <- ma$tau^2
  user_data$results$heterogeneity$i_squared <- ma$I2
  user_data$results$heterogeneity$h <- ma$H
  user_data$results$heterogeneity$q <- ma$Q
  user_data$results$heterogeneity$df <- ma$df.Q
  user_data$results$heterogeneity$p_value <- ma$pval.Q
  
  # Prediction interval
  if (!is.null(ma$lower.predict) && !is.null(ma$upper.predict)) {
    user_data$results$prediction$lower <- ma$lower.predict
    user_data$results$prediction$upper <- ma$upper.predict
  }
  
  # Number of studies
  user_data$results$studies <- ma$k
  
  user_summary(user_data)
  return(user_summary)
}

# Update publication bias results in the user summary
update_publication_bias <- function(user_summary, ma, input) {
  user_data <- user_summary()
  
  tryCatch({
    # Egger's test
    egger_test <- metabias(ma, method = "linreg")
    
    if (!is.null(egger_test)) {
      user_data$bias$egger$statistic <- egger_test$statistic
      user_data$bias$egger$p_value <- egger_test$pval
    }
    
    # Trim and fill
    tf <- trimfill(ma)
    
    if (!is.null(tf)) {
      user_data$bias$trim_fill$added <- tf$k - ma$k
      
      if (!is.null(ma$TE.random) && !is.null(tf$TE.random)) {
        user_data$bias$trim_fill$original_effect <- ma$TE.random
        user_data$bias$trim_fill$adjusted_effect <- tf$TE.random
      } else if (!is.null(ma$TE.fixed) && !is.null(tf$TE.fixed)) {
        user_data$bias$trim_fill$original_effect <- ma$TE.fixed
        user_data$bias$trim_fill$adjusted_effect <- tf$TE.fixed
      }
    }
  }, error = function(e) {
    # Handle errors silently - publication bias tests often fail with small numbers of studies
  })
  
  user_summary(user_data)
  return(user_summary)
}

# Update meta-regression results in the user summary
update_meta_regression <- function(user_summary, ma, vars) {
  user_data <- user_summary()
  
  if (length(vars) > 0) {
    formula <- as.formula(paste("~", paste(vars, collapse = " + ")))
    
    tryCatch({
      mr <- metareg(ma, formula)
      
      if (!is.null(mr)) {
        user_data$metareg$variables <- vars
        user_data$metareg$coefficients <- mr$b
        user_data$metareg$p_values <- mr$pval
        user_data$metareg$r_squared <- mr$R2
      }
    }, error = function(e) {
      # Handle errors silently
    })
  }
  
  user_summary(user_data)
  return(user_summary)
}

# Format user summary as text for display and download
format_user_summary_as_text <- function(user_data) {
  if (is.null(user_data$model$type)) {
    return("No meta-analysis has been conducted yet. Please upload data and run the analysis.")
  }
  
  # Model information
  output <- "META-ANALYSIS SUMMARY REPORT\n"
  output <- paste0(output, "=======================================\n\n")
  
  output <- paste0(output, "### MODEL INFORMATION\n\n")
  output <- paste0(output, "Model type: ", user_data$model$type, "\n")
  output <- paste0(output, "Effect measure: ", user_data$model$measure, "\n")
  
  if (!is.null(user_data$model$smd_method) && user_data$model$measure == "SMD") {
    output <- paste0(output, "SMD method: ", user_data$model$smd_method, "\n")
  }
  
  output <- paste0(output, "τ² estimator: ", user_data$model$method_tau, "\n")
  output <- paste0(output, "CI method: ", user_data$model$method_ci, "\n")
  output <- paste0(output, "Prediction interval method: ", user_data$model$method_predict, "\n")
  output <- paste0(output, "τ² CI method: ", ifelse(user_data$model$method_tau_ci == "", "None", user_data$model$method_tau_ci), "\n")
  output <- paste0(output, "Number of studies: ", user_data$results$studies, "\n\n")
  
  # Results
  output <- paste0(output, "### RESULTS\n\n")
  
  # Common effect
  if (!is.null(user_data$results$common$estimate)) {
    output <- paste0(output, "* Common effect model:\n")
    output <- paste0(output, "  - Effect estimate: ", round(user_data$results$common$estimate, 4), 
                     " (95% CI: ", round(user_data$results$common$ci_lower, 4), " to ", 
                     round(user_data$results$common$ci_upper, 4), ")\n")
    output <- paste0(output, "  - z = ", round(user_data$results$common$z_value, 4), 
                     ", p = ", format.pval(user_data$results$common$p_value, digits = 4), "\n\n")
  }
  
  # Random effects
  if (!is.null(user_data$results$random$estimate)) {
    output <- paste0(output, "* Random effects model:\n")
    output <- paste0(output, "  - Effect estimate: ", round(user_data$results$random$estimate, 4), 
                     " (95% CI: ", round(user_data$results$random$ci_lower, 4), " to ", 
                     round(user_data$results$random$ci_upper, 4), ")\n")
    output <- paste0(output, "  - z = ", round(user_data$results$random$z_value, 4), 
                     ", p = ", format.pval(user_data$results$random$p_value, digits = 4), "\n\n")
  }
  
  # Prediction interval
  if (!is.null(user_data$results$prediction$lower) && !is.null(user_data$results$prediction$upper)) {
    output <- paste0(output, "* Prediction interval: [", 
                     round(user_data$results$prediction$lower, 4), " to ", 
                     round(user_data$results$prediction$upper, 4), "]\n\n")
  }
  
  # Heterogeneity
  output <- paste0(output, "### HETEROGENEITY\n\n")
  output <- paste0(output, "* τ² = ", round(user_data$results$heterogeneity$tau_squared, 4), "\n")
  output <- paste0(output, "* I² = ", round(user_data$results$heterogeneity$i_squared * 100, 1), "%\n")
  output <- paste0(output, "* H = ", round(user_data$results$heterogeneity$h, 2), "\n")
  output <- paste0(output, "* Q = ", round(user_data$results$heterogeneity$q, 2), 
                   " (df = ", user_data$results$heterogeneity$df, 
                   ", p = ", format.pval(user_data$results$heterogeneity$p_value, digits = 4), ")\n\n")
  
  # Publication bias
  output <- paste0(output, "### PUBLICATION BIAS\n\n")
  
  # Egger's test
  if (!is.null(user_data$bias$egger$statistic) && !is.null(user_data$bias$egger$p_value)) {
    output <- paste0(output, "* Egger's test for funnel plot asymmetry:\n")
    output <- paste0(output, "  - t = ", round(user_data$bias$egger$statistic, 4), 
                     ", p = ", format.pval(user_data$bias$egger$p_value, digits = 4), "\n")
    
    if (user_data$bias$egger$p_value < 0.05) {
      output <- paste0(output, "  - Evidence of small-study effects\n\n")
    } else {
      output <- paste0(output, "  - No significant evidence of small-study effects\n\n")
    }
  } else {
    output <- paste0(output, "* Egger's test: Could not be performed (usually due to too few studies)\n\n")
  }
  
  # Trim and fill
  if (!is.null(user_data$bias$trim_fill$added)) {
    output <- paste0(output, "* Trim and fill method:\n")
    output <- paste0(output, "  - Number of studies added: ", user_data$bias$trim_fill$added, "\n")
    
    if (!is.null(user_data$bias$trim_fill$original_effect) && !is.null(user_data$bias$trim_fill$adjusted_effect)) {
      output <- paste0(output, "  - Original effect estimate: ", round(user_data$bias$trim_fill$original_effect, 4), "\n")
      output <- paste0(output, "  - Adjusted effect estimate: ", round(user_data$bias$trim_fill$adjusted_effect, 4), "\n")
      
      percent_change <- abs((user_data$bias$trim_fill$adjusted_effect - user_data$bias$trim_fill$original_effect) / 
                              user_data$bias$trim_fill$original_effect) * 100
      
      output <- paste0(output, "  - Change: ", round(percent_change, 1), "%\n\n")
    } else {
      output <- paste0(output, "  - Effect estimates not available\n\n")
    }
  } else {
    output <- paste0(output, "* Trim and fill: Could not be performed\n\n")
  }
  
  # Meta-regression
  if (!is.null(user_data$metareg$variables) && length(user_data$metareg$variables) > 0) {
    output <- paste0(output, "### META-REGRESSION\n\n")
    output <- paste0(output, "* Variables: ", paste(user_data$metareg$variables, collapse = ", "), "\n")
    
    if (!is.null(user_data$metareg$coefficients) && !is.null(user_data$metareg$p_values)) {
      output <- paste0(output, "* Coefficients:\n")
      
      for (i in 1:length(user_data$metareg$coefficients)) {
        var_name <- names(user_data$metareg$coefficients)[i]
        coef_value <- user_data$metareg$coefficients[i]
        p_value <- user_data$metareg$p_values[i]
        
        output <- paste0(output, "  - ", var_name, ": ", round(coef_value, 4), 
                         " (p = ", format.pval(p_value, digits = 4), ")\n")
      }
      
      if (!is.null(user_data$metareg$r_squared)) {
        output <- paste0(output, "* R² analog: ", round(user_data$metareg$r_squared * 100, 1), "%\n")
      }
    }
  }
  
  return(output)
}