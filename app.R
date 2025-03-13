# Meta-Mar: AI-Integrated Meta-Analysis Platform
# Version: 4.0.2
# Author: Ashkan Beheshti
# Description: This is a simplified version of the app.R file for the Meta-Mar project
#              demonstrating key functionality of the meta-analysis platform.

# Required libraries
library(shiny)
library(meta)
library(pimeta)
library(readxl)
library(ggplot2)
library(dplyr)
library(metafor)
library(DT)
library(httr)
library(markdown)
library(jsonlite)
library(promises)
library(future)
library(shinyjs)

# Set up multisession for parallel processing
plan(multisession)

# Load source files
# Note: In the actual application, these would contain additional functionality
# source("global.R")
# source("user_summary.R")
# source("documentation_MetaMar.R")

# UI Definition
ui <- fluidPage(
  shinyjs::useShinyjs(),
  # Add necessary CSS styles 
  tags$head(
    tags$style(HTML('
        /* Base styles */
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
        }
        
        /* Enhanced loader styles */
        .loader {
            width: 50px;
            height: 50px;
            border: 5px solid #f3f3f3;
            border-top: 5px solid #007bff;
            border-radius: 50%;
            animation: spin 1s linear infinite;
            margin: 30px auto;
            box-shadow: 0 0 10px rgba(0,0,0,0.1);
        }
        
        @keyframes spin {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(360deg); }
        }
    '))
  ),
  titlePanel("Meta-Mar v4.0.2"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel or CSV File", accept = c(".xlsx", ".csv")),
      selectInput("model", "Choose the Meta-Analysis Model",
                  choices = c("Continuous outcome" = "metacont",
                              "Binary outcome" = "metabin",
                              "Generic inverse variance (effect size)" = "metagen",
                              "Correlations" = "metacor")),
      
      # This would be populated based on model selection
      uiOutput("sm_ui"),
      uiOutput("smd_method_ui"),
      uiOutput("pooling_method_ui"),
      
      selectInput("method.tau", "τ² Estimator (estimates the between-study variance)",
                  choices = c("REML: Restricted maximum-likelihood (Viechtbauer, 2005)" = "REML",
                              "PM: Paule-Mandel (Paule and Mandel, 1982)" = "PM",
                              "DL: DerSimonian-Laird (DerSimonian and Laird, 1986)" = "DL",
                              "ML: Maximum-likelihood (Viechtbauer, 2005)" = "ML",
                              "HS: Hunter-Schmidt (Hunter and Schmidt, 2015)" = "HS",
                              "SJ: Sidik-Jonkman (Sidik and Jonkman, 2005)" = "SJ",
                              "HE: Hedges (Hedges and Olkin, 1985)" = "HE",
                              "EB: Empirical Bayes (Morris, 1983)" = "EB"),
                  selected = "REML"),
      
      selectInput("method.random.ci", "Select the Method for Calculating Confidence Intervals",
                  choices = c("Based on standard normal quantile (classic)" = "classic",
                              "Hartung-Knapp method" = "HK",
                              "Kenward-Roger method (only available with REML)" = "KR")),
      
      selectInput("method.predict", "Select the Method for Calculating Prediction Intervals",
                  choices = c("Based on t-distribution (HTS)" = "HTS",
                              "Hartung-Knapp method (HK)" = "HK",
                              "Kenward-Roger method (KR)" = "KR",
                              "Bootstrap approach (NNF)" = "NNF",
                              "Based on standard normal quantile (S)" = "S")),
      
      selectInput("method.tau.ci", "Select the Method for Calculating τ² Confidence Intervals",
                  choices = c("Jackson method" = "J",
                              "Biggerstaff and Jackson method" = "BJ",
                              "Q-Profile method" = "QP",
                              "Profile-Likelihood method" = "PL",
                              "No confidence interval" = "")),
      
      actionButton("run", "Run Analysis", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Meta-Mar",
                           div(
                             style = "text-align: center; padding: 25px; margin: 20px auto; max-width: 1000px;",
                             
                             # Header
                             div(
                               style = "margin-bottom: 30px;",
                               h2("Welcome to Meta-Mar v4.0.2", style = "color: #3498db; margin-bottom: 15px;"),
                               p("A comprehensive platform for meta-analysis with AI integration", style = "font-size: 1.2em; color: #555;")
                             ),
                             
                             # Main options in card layout
                             div(
                               style = "display: flex; justify-content: center; flex-wrap: wrap; gap: 20px; margin: 30px 0;",
                               
                               # Option 1: AI Assistant
                               div(
                                 style = "background-color: white; padding: 25px; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); flex: 1; min-width: 220px; max-width: 280px;",
                                 h3("AI Assistant", style = "color: #3498db; margin-top: 15px; margin-bottom: 15px;"),
                                 p("Chat with our specialized AI to get help with meta-analysis concepts and using Meta-Mar."),
                                 div(style = "margin-top: 20px;",
                                     actionButton("goto_ai_home", "Talk with AI", 
                                                  style = "background-color: #3498db; color: white; border: none; border-radius: 4px; padding: 8px 15px; width: 100%;")
                                 )
                               ),
                               
                               # Option 2: Documentation
                               div(
                                 style = "background-color: white; padding: 25px; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); flex: 1; min-width: 220px; max-width: 280px;",
                                 h3("Documentation", style = "color: #3498db; margin-top: 15px; margin-bottom: 15px;"),
                                 p("Access comprehensive guides about meta-analysis methods and how to use Meta-Mar effectively."),
                                 div(style = "margin-top: 20px;",
                                     actionButton("goto_docs_home", "Read Docs", 
                                                  style = "background-color: #3498db; color: white; border: none; border-radius: 4px; padding: 8px 15px; width: 100%;")
                                 )
                               ),
                               
                               # Option 3: Start Analysis
                               div(
                                 style = "background-color: white; padding: 25px; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); flex: 1; min-width: 220px; max-width: 280px;",
                                 h3("Start Analysis", style = "color: #3498db; margin-top: 15px; margin-bottom: 15px;"),
                                 p("Upload your data and run a meta-analysis with our comprehensive statistical tools."),
                                 div(style = "margin-top: 20px;",
                                     actionButton("start_analysis", "Upload Data", 
                                                  style = "background-color: #3498db; color: white; border: none; border-radius: 4px; padding: 8px 15px; width: 100%;")
                                 )
                               )
                             ),
                             
                             # Version info and citation
                             div(
                               style = "margin-top: 30px; font-size: 0.9em; color: #777; border-top: 1px solid #eee; padding-top: 20px;",
                               p("Meta-Mar v4.0.2 | A next-generation meta-analysis platform"),
                               p("To cite: https://doi.org/10.1186/s12888-020-2442-7")
                             )
                           )
                  ),
                  
                  tabPanel("Data Summary", 
                           conditionalPanel(
                             condition = "!input.file",
                             div(
                               style = "text-align: center; padding: 25px; background-color: #f8f9fa; border-radius: 8px; margin: 20px auto; max-width: 800px; border-left: 4px solid #3498db;",
                               h3("No Data Uploaded Yet", style = "color: #3498db; margin-bottom: 20px;"),
                               p("To view data summary statistics, please upload your data using the file selector in the sidebar.")
                             )
                           ),
                           conditionalPanel(
                             condition = "input.file",
                             DTOutput("data_table"),
                             verbatimTextOutput("data_summary_text")
                           )
                  ),
                  
                  tabPanel("Meta-Analysis Summary", 
                           verbatimTextOutput("ma_summary")),
                  
                  tabPanel("Plots",
                           tabsetPanel(
                             tabPanel("Forest Plot", 
                                      plotOutput("forest_plot_common", height = "800px"),
                                      br(),
                                      plotOutput("forest_plot_random", height = "800px")),
                             tabPanel("Funnel Plot", 
                                      plotOutput("funnel_plot", height = "600px")),
                             tabPanel("Other Plots",
                                      selectInput("other_plot", "Select Plot Type",
                                                  choices = c("Galbraith Plot", "L'Abbe Plot", "Baujat Plot", "Bubble Plot")),
                                      plotOutput("other_plot", height = "600px"))
                           )),
                  
                  tabPanel("Meta-Regression", 
                           p("Meta-regression is used to explore the relationship between study-level covariates and effect sizes."),
                           uiOutput("metareg_vars"),
                           verbatimTextOutput("metareg_summary")),
                  
                  tabPanel("Bias Analysis", 
                           h4("Test for funnel plot asymmetry"),
                           verbatimTextOutput("bias_analysis"))
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Data Loading and Validation ------------------------------------
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    
    tryCatch(
      {
        if(ext == "csv") {
          read.csv(input$file$datapath)
        } else if(ext == "xlsx") {
          read_excel(input$file$datapath)
        } else {
          stop("Unsupported file format")
        }
      },
      error = function(e) {
        showNotification(
          HTML("Error reading file. Please make sure it's a valid CSV or Excel file."), 
          type = "error"
        )
        NULL
      }
    )
  })
  
  # UI Sidebar Outputs ------------------------------------
  output$sm_ui <- renderUI({
    req(input$model)
    choices <- switch(input$model,
                      "metacont" = c(
                        "Mean Difference (MD)" = "MD",
                        "Standardized Mean Difference (SMD)" = "SMD",
                        "Ratio of Means (ROM)" = "ROM"
                      ),
                      "metabin" = c(
                        "Odds Ratio (OR)" = "OR",
                        "Risk Ratio (RR)" = "RR",
                        "Risk Difference (RD)" = "RD"
                      ),
                      "metagen" = c(
                        "Mean Difference (MD)" = "MD",
                        "Standardized Mean Difference (SMD)" = "SMD",
                        "Ratio of Means (ROM)" = "ROM",
                        "Odds Ratio (OR)" = "OR",
                        "Risk Ratio (RR)" = "RR",
                        "Risk Difference (RD)" = "RD",
                        "Hazard Ratio (HR)" = "HR",
                        "Incidence Rate Ratio (IRR)" = "IRR"
                      ),
                      "metacor" = c(
                        "Fisher's z-transformed correlation (ZCOR)" = "ZCOR",
                        "Untransformed correlation (COR)" = "COR"
                      )
    )
    
    selectInput("sm", "Summary measure used for pooling of studies", choices = choices)
  })
  
  output$smd_method_ui <- renderUI({
    req(input$sm)
    
    if (input$sm == "SMD") {
      selectInput("smd_method", "SMD method",
                  choices = c(
                    "Hedges' g" = "Hedges",
                    "Cohen's d" = "Cohen",
                    "Glass' delta" = "Glass"
                  )
      )
    }
  })
  
  output$pooling_method_ui <- renderUI({
    req(input$model)
    
    if (input$model == "metabin") {
      selectInput("pooling_method", "Method for pooling studies",
                  choices = c(
                    "Mantel-Haenszel" = "MH",
                    "Inverse Variance" = "Inverse",
                    "Peto" = "Peto"
                  )
      )
    } else {
      selectInput("pooling_method", "Method for pooling studies",
                  choices = c("Inverse Variance" = "Inverse"),
                  selected = "Inverse"
      )
    }
  })
  
  # Meta Analysis ------------------------------------
  meta_analysis <- eventReactive(input$run, {
    req(data())
    
    # Validate data first (this is simplified; full validation would be more comprehensive)
    validate_data <- function() {
      required_cols <- switch(input$model,
                              "metacont" = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
                              "metabin" = c("studlab", "event.e", "n.e", "event.c", "n.c"),
                              "metagen" = c("studlab", "TE", "seTE"),
                              "metacor" = c("studlab", "cor", "n")
      )
      
      missing_cols <- setdiff(required_cols, names(data()))
      
      if(length(missing_cols) > 0) {
        showNotification(
          HTML(sprintf(
            "Missing required columns: %s", 
            paste(missing_cols, collapse=", ")
          )), 
          type = "error"
        )
        return(FALSE)
      }
      return(TRUE)
    }
    
    if (!validate_data()) return(NULL)
    
    # Get the appropriate meta-analysis function
    meta_func <- switch(input$model,
                        "metacont" = metacont,
                        "metabin" = metabin,
                        "metagen" = metagen,
                        "metacor" = metacor
    )
    
    # Prepare arguments based on UI inputs
    args <- list(
      sm = input$sm,
      method.tau = input$method.tau,
      method.random.ci = input$method.random.ci,
      method.predict = input$method.predict,
      method.tau.ci = input$method.tau.ci
    )
    
    if (input$model == "metabin") {
      args$method <- input$pooling_method
    }
    
    if (input$model == "metacont" && input$sm == "SMD" && !is.null(input$smd_method)) {
      args$method.smd <- input$smd_method
    }
    
    # For each model, extract the required columns from data
    model_data <- switch(input$model,
                         "metacont" = list(
                           n.e = data()$n.e,
                           mean.e = data()$mean.e,
                           sd.e = data()$sd.e,
                           n.c = data()$n.c,
                           mean.c = data()$mean.c,
                           sd.c = data()$sd.c,
                           studlab = data()$studlab
                         ),
                         "metabin" = list(
                           event.e = data()$event.e,
                           n.e = data()$n.e,
                           event.c = data()$event.c,
                           n.c = data()$n.c,
                           studlab = data()$studlab
                         ),
                         "metagen" = list(
                           TE = data()$TE,
                           seTE = data()$seTE,
                           studlab = data()$studlab
                         ),
                         "metacor" = list(
                           cor = data()$cor,
                           n = data()$n,
                           studlab = data()$studlab
                         )
    )
    
    # Combine model-specific data with general arguments
    all_args <- c(model_data, args)
    
    # Run the meta-analysis
    tryCatch(
      {
        result <- do.call(meta_func, all_args)
        result
      },
      error = function(e) {
        showNotification(
          HTML(sprintf(
            "Error in meta-analysis: %s", 
            e$message
          )), 
          type = "error"
        )
        NULL
      }
    )
  })
  
  # Data Summary Outputs ------------------------------------
  output$data_table <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$data_summary_text <- renderPrint({
    req(data())
    summary(data())
  })
  
  output$ma_summary <- renderPrint({
    req(meta_analysis())
    summary(meta_analysis())
  })
  
  # Plot Outputs ------------------------------------
  output$forest_plot_common <- renderPlot({
    req(meta_analysis())
    forest(meta_analysis(), 
           common = TRUE, 
           random = FALSE,
           main = "Common Effect Model Forest Plot")
  })
  
  output$forest_plot_random <- renderPlot({
    req(meta_analysis())
    forest(meta_analysis(), 
           common = FALSE, 
           random = TRUE,
           main = "Random Effects Model Forest Plot")
  })
  
  output$funnel_plot <- renderPlot({
    req(meta_analysis())
    funnel(meta_analysis(), 
           studlab = TRUE,
           contour = c(0.9, 0.95, 0.99),
           col.contour = c("darkgreen", "green", "lightgreen"))
  })
  
  output$other_plot <- renderPlot({
    req(meta_analysis())
    
    switch(input$other_plot,
           "Galbraith Plot" = radial(meta_analysis()),
           "L'Abbe Plot" = {
             if(input$model == "metabin") {
               labbe(meta_analysis())
             } else {
               plot(NULL, xlab = "", ylab = "", 
                    main = "L'Abbe plot only available for binary outcomes")
             }
           },
           "Baujat Plot" = baujat(meta_analysis()),
           "Bubble Plot" = {
             tryCatch(
               {
                 # This is a placeholder for meta-regression
                 plot(NULL, xlab = "", ylab = "", 
                      main = "Bubble plot requires meta-regression")
               },
               error = function(e) {
                 plot(NULL, xlab = "", ylab = "", 
                      main = "Bubble plot not available")
               }
             )
           }
    )
  })
  
  # Meta-regression Outputs ------------------------------------
  output$metareg_vars <- renderUI({
    req(data())
    
    basic_vars <- switch(input$model,
                         "metacont" = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
                         "metabin" = c("studlab", "event.e", "n.e", "event.c", "n.c"),
                         "metagen" = c("studlab", "TE", "seTE"),
                         "metacor" = c("studlab", "cor", "n")
    )
    
    additional_vars <- setdiff(names(data()), basic_vars)
    
    if (length(additional_vars) > 0) {
      checkboxGroupInput("metareg_vars", 
                         "Choose variables for meta-regression:",
                         choices = additional_vars,
                         selected = NULL)
    } else {
      helpText("No additional variables available for meta-regression.")
    }
  })
  
  output$metareg_summary <- renderPrint({
    req(meta_analysis(), input$metareg_vars)
    
    if (length(input$metareg_vars) > 0) {
      formula <- as.formula(paste("~", paste(input$metareg_vars, collapse = " + ")))
      
      tryCatch(
        {
          mr <- metareg(meta_analysis(), formula)
          summary(mr)
        },
        error = function(e) {
          cat("Meta-regression error: ", e$message, "\n")
          cat("Please check your data structure and selected variables.")
        }
      )
    } else {
      cat("No variables selected for meta-regression.")
    }
  })
  
  # Bias analysis
  output$bias_analysis <- renderPrint({
    req(meta_analysis())
    
    tryCatch({
      # Create a metagen object for bias testing if not already in correct format
      bias_ma <- meta_analysis()
      
      # Egger's Test for funnel plot asymmetry
      cat("1. EGGER'S TEST FOR FUNNEL PLOT ASYMMETRY\n")
      cat("------------------------------------------\n")
      tryCatch({
        egger_test <- metabias(bias_ma, method = "linreg")
        print(egger_test)
        
        if(egger_test$pval < 0.05) {
          cat("\nInterpretation: Significant evidence of small-study effects (p < 0.05),\n")
          cat("suggesting potential publication bias.\n")
        } else {
          cat("\nInterpretation: No significant evidence of small-study effects (p >= 0.05),\n")
          cat("suggesting no detectable publication bias using this method.\n")
        }
      }, error = function(e) {
        cat("Could not perform Egger's test: ", conditionMessage(e), "\n")
      })
      
      cat("\n\n")
      
      # Trim and Fill Analysis
      cat("2. TRIM AND FILL ANALYSIS\n")
      cat("------------------------\n")
      tryCatch({
        tf <- trimfill(bias_ma)
        
        # Extract key information
        k_original <- bias_ma$k
        k_added <- tf$k - bias_ma$k
        
        # Create a nice output
        cat("Number of observed studies:    ", k_original, "\n")
        cat("Estimated missing studies:     ", k_added, "\n")
        cat("Total studies after trim-fill: ", tf$k, "\n\n")
        
        # Add interpretation
        if(k_added > 0) {
          cat("Interpretation: The trim and fill method suggests the presence of\n")
          cat(k_added, "potentially missing studies, indicating possible publication bias.\n")
        } else {
          cat("Interpretation: No asymmetry detected. The trim and fill method did not\n")
          cat("identify any potentially missing studies, suggesting no evidence of\n")
          cat("publication bias using this method.\n")
        }
      }, error = function(e) {
        cat("Could not perform trim and fill analysis: ", conditionMessage(e), "\n")
      })
      
    }, error = function(e) {
      cat("Error in publication bias analysis: ", conditionMessage(e), "\n\n")
      cat("This may occur because:\n")
      cat("1. There are too few studies (most tests need at least 10 studies)\n")
      cat("2. The meta-analysis structure doesn't contain the expected elements\n")
    })
  })
  
  # UI interactions
  observeEvent(input$start_analysis, {
    updateTabsetPanel(session, "tabs", selected = "Data Summary")
  })
  
  observeEvent(input$goto_ai_home, {
    showNotification("AI assistant would be available in the full version", type = "message")
  })
  
  observeEvent(input$goto_docs_home, {
    showNotification("Documentation would be available in the full version", type = "message")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)