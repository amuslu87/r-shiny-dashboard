# global.R - Configuration file for Indiana Salmonella Surveillance Dashboard
# This file contains shared functions and variables

# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(ggplot2)
library(sf)
library(RColorBrewer)
library(viridis)
library(lubridate)
library(htmltools)
library(shinyjs)
library(rmarkdown)
library(igraph)
library(ggraph)
library(tidygraph)
library(dendextend)  # For the improved phylogenetic tree
library(kableExtra)
library(ape)


improved_serotype_colors <- c(
  "Agbeni" = "#8B0000",       # Dark red
  "Braenderup" = "#4B0082",   # Indigo
  "Enteritidis" = "#1E90FF",  # Dodger blue
  "I 4:i:-" = "#696969",      # Dim gray
  "Infantis" = "#FF1493",     # Deep pink
  "Javiana" = "#006400",      # Dark green
  "Newport" = "#008B8B",      # Dark cyan
  "Paratyphi B var. L(+) tartrate+" = "#FF8C00", # Dark orange
  "Senftenberg" = "#9932CC",  # Dark orchid
  "Stanley" = "#FFD700",      # Gold
  "Saintpaul" = "#00CED1",    # Dark turquoise
  "Thompson" = "#2E8B57",     # Sea green
  "Typhimurium" = "#FFA07A",  # Light salmon
  "Typhi" = "#DC143C",        # Crimson
  "Paratyphi A" = "#FF4500",  # Orange red
  "Paratyphi B" = "#FF6347",  # Tomato
  "Paratyphi C" = "#FF7F50",  # Coral
  "Africana" = "#800080",     # Purple
  "Other" = "#A9A9A9"         # Dark gray
)


age_colors <- viridis(5, option = "D")  
specimen_colors <- brewer.pal(8, "Dark2")
outbreak_colors <- colorRampPalette(brewer.pal(8, "Set2"))(15)
county_colors <- viridis(30, option = "D", direction = -1)  # Darker colors for higher values

# Function to load and preprocess the data
load_and_process_data <- function() {
  # Load the Salmonella data
  salmonella_data <- read.csv("Salmonella_May.csv", stringsAsFactors = FALSE)
  
  # Format the Collection_Date field
  salmonella_data$Collection_Date <- as.Date(salmonella_data$Collection_Date, format = "%m/%d/%Y")
  
  # Filter for data from January 2023 onwards
  salmonella_data <- salmonella_data %>%
    filter(Collection_Date >= as.Date("2023-01-01"))
  
  # Replace NA values with "Unknown"
  salmonella_data <- salmonella_data %>%
    mutate_all(~ifelse(is.na(.) | . == "N/A", "Unknown", .))
  
  # Standardize state field
  salmonella_data <- salmonella_data %>%
    mutate(State = case_when(
      State == "IN" ~ "Indiana",
      State != "Indiana" & State != "Unknown" ~ "Out-of-state",
      TRUE ~ State
    ))
  
  # Create age groups
  salmonella_data <- salmonella_data %>%
    mutate(Age_Group = case_when(
      Patient_Age_Years < 1 | (Patient_Age_Years == "Unknown" & Patient_Age_Months < 12) ~ "< 1 year",
      Patient_Age_Years >= 1 & Patient_Age_Years <= 5 ~ "1-5 years",
      Patient_Age_Years > 5 & Patient_Age_Years <= 18 ~ "6-18 years",
      Patient_Age_Years > 18 & Patient_Age_Years <= 65 ~ "19-65 years", 
      Patient_Age_Years > 65 ~ "> 65 years",
      TRUE ~ "Unknown"
    ))
  
  # Standardize sex field
  salmonella_data <- salmonella_data %>%
    mutate(Sex = toupper(Sex)) %>%
    mutate(Sex = case_when(
      Sex == "F" ~ "FEMALE",
      Sex == "M" ~ "MALE",
      TRUE ~ Sex
    ))
  
  # Create year-month field for trending
  salmonella_data <- salmonella_data %>%
    mutate(Year_Month = format(Collection_Date, "%Y-%m"))
  
  # Correct MLST format if needed
  salmonella_data <- salmonella_data %>%
    mutate(MLST_ST = ifelse(
      !is.na(MLST_ST) & MLST_ST != "Unknown" & !grepl("^ST", MLST_ST),
      paste0("ST", MLST_ST),
      MLST_ST
    ))
  
  return(salmonella_data)
}

# Function to standardize plots with colorblind-friendly defaults
create_standard_plot <- function(plot_data, x_var, y_var, fill_var = NULL, 
  color_var = NULL, title = "", x_label = "", y_label = "") {
  
  # Base plot
  p <- ggplot(plot_data, aes_string(x = x_var, y = y_var))
  
  # Add fill if specified
  if (!is.null(fill_var)) {
    p <- p + aes_string(fill = fill_var)
  }
  
  # Add color if specified
  if (!is.null(color_var)) {
    p <- p + aes_string(color = color_var)
  }
  
  # Add common theme and labels
  p <- p + 
    theme_minimal() +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    )
  
  return(p)
}

# Load Indiana counties shapefile (placeholder - would need actual data in production)
# In a real implementation, this would use proper shapefile data from an authoritative source
# For example:
# indiana_counties <- sf::st_read("data/indiana_counties.shp")

# Initialize the data when global.R is sourced
# This preloads the data for faster startup
salmonella_data <- load_and_process_data()