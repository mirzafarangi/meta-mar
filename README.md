# Meta-Mar: AI-Integrated Meta-Analysis Platform


<p align="center">
  <a href="https://www.meta-mar.com">Launch Meta-Mar v4.0.2</a> •
  <a href="#features">Features</a> •
  <a href="#getting-started">Getting Started</a> •
  <a href="#examples">Examples</a> •
  <a href="#citation">Citation</a>
</p>

## Overview

Meta-Mar is an open-source statistical platform for conducting meta-analyses, developed for both research and educational purposes. The platform integrates AI assistance to guide users through methodological decisions and interpretation of results.

The tool supports a wide range of meta-analytic procedures, from basic effect size calculations to advanced heterogeneity assessment and publication bias evaluation. It is designed for researchers, educators, and students across various fields where evidence synthesis is valuable.



## Features

### Core Capabilities
- **Methodological Flexibility**: Support for various outcome types (continuous, binary, correlations, effect size) and multiple model types
- **Visualization Tools**: Publication-quality forest plots, funnel plots, and other meta-analytic visualizations
- **Heterogeneity Assessment**: I², τ², and Cochran's Q statistics with subgroup analysis options
- **Publication Bias Analysis**: Egger's test, trim-and-fill analysis, and fail-safe N calculations
- **Meta-Regression**: Tools for exploring relationships between study characteristics and effect sizes
- **AI Assistance**: Interactive chatbot for methodological guidance and AI-powered report generation

### Technical Details
- Built with R Shiny for an interactive web interface
- Leverages established R meta-analysis packages (meta, metafor, pimeta)
- Integrates AI capabilities using OpenAI's GPT models
- Full support for data import/export and result sharing

## Getting Started

### Online Access
The easiest way to use Meta-Mar is through our hosted version:
- [Launch Meta-Mar v4.0.2](https://www.meta-mar.com)

### Local Installation

To run Meta-Mar locally:

1. Clone this repository
```bash
git clone https://github.com/yourusername/meta-mar.git
cd meta-mar
```

2. Install required R packages
```r
install.packages(c("shiny", "meta", "pimeta", "readxl", "ggplot2", "dplyr", 
                   "metafor", "DT", "httr", "markdown", "jsonlite", 
                   "promises", "future", "shinyjs"))
```

3. Run the application
```r
shiny::runApp()
```

## Examples

### Basic Meta-Analysis

Here's a simple example of how to perform a meta-analysis using Meta-Mar's underlying code:

```r
# Load libraries
library(meta)
library(metafor)

# Example data for continuous outcomes
data <- data.frame(
  studlab = paste("Study", 1:5),
  n.e = c(50, 45, 60, 90, 70),
  mean.e = c(12.5, 13.2, 14.1, 15.0, 13.7),
  sd.e = c(2.5, 2.8, 3.1, 2.9, 3.0),
  n.c = c(48, 47, 55, 88, 65),
  mean.c = c(10.2, 10.8, 11.5, 11.9, 10.5),
  sd.c = c(2.4, 2.5, 3.0, 3.1, 2.8)
)

# Run meta-analysis
ma <- metacont(
  n.e = data$n.e,
  mean.e = data$mean.e,
  sd.e = data$sd.e,
  n.c = data$n.c,
  mean.c = data$mean.c,
  sd.c = data$sd.c,
  studlab = data$studlab,
  sm = "SMD",
  method.smd = "Hedges",
  method.tau = "REML",
  method.random.ci = "HK",
  prediction = TRUE
)

# Print results
summary(ma)

# Create forest plot
forest(ma, 
       common = FALSE, 
       random = TRUE,
       prediction = TRUE,
       lab.e = "Intervention",
       lab.c = "Control",
       comb.random = TRUE,
       text.random = "Random effects model",
       text.predict = "Prediction interval")
```

For more complex examples and demonstrations, please visit our [documentation](https://meta-mar.shinyapps.io/metamar_llm/).

## Repository Structure

```
meta-mar/
├── app.R                  # Main application file
├── global.R               # Global settings and functions
├── user_summary.R         # User summary functionality
├── documentation_MetaMar.R # Documentation content
├── assets/                # Images and static resources
├── example_data/          # Example datasets
└── README.md              # This readme file
```

## Citation

If you use Meta-Mar in your research, please cite it as follows:

```
Beheshti, A., Chavanon, M. L., & Christiansen, H. (2020). Emotion dysregulation in adults with attention deficit hyperactivity disorder: a meta-analysis. BMC psychiatry, 20, 1-11.
https://www.meta-mar.com
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact

For questions or support, please contact: [a.beheshti@posteo.de](mailto:a.beheshti@posteo.de)
