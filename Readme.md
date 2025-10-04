# Thenmap API Shiny Explorer

Interactive exploration of historical administrative boundary data using the [AdvancedRHT2025Lab05](https://github.com/rukminishipra01/AdvancedRHT2025Lab05) R package.

## About

This Shiny application provides an interactive interface to explore historical administrative boundaries from the [Thenmap API](https://thenmap.net/). The app allows you to visualize how regional boundaries have changed over time across different Nordic countries and worldwide.

## Features

- **Dataset Selection**: Choose from multiple datasets (Swedish, Norwegian, Danish, Finnish municipalities, and World countries)
- **Date Selection**: Explore historical boundaries at any point in time
- **Temporal Comparison**: Compare two time periods to identify new regions added between dates
- **Interactive Map**: Visualize geographical boundaries with hover labels and detailed information
- **Data Table**: Browse and filter data with interactive tables
- **Metadata View**: Inspect dataset metadata and API information

## Running the App

### From GitHub:

# Install required packages if needed
install.packages(c("shiny", "DT", "leaflet", "sf", "bslib"))

# Install the API package
devtools::install_github("rukminishipra01/AdvancedRHT2025Lab05")

# Run the app
shiny::runGitHub("AdvancedRHT2025Lab05-shiny", "rukminishipra01")
