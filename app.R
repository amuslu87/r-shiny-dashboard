###############################################
# Salmonella Surveillance Dashboard
# Created: January 4, 2025
# Updated: April 29, 2025
###############################################


install.packages(c(
  "shiny", 
  "shinydashboard", 
  "tidyverse", 
  "DT", 
  "plotly", 
  "ggplot2", 
  "sf", 
  "RColorBrewer", 
  "viridis", 
  "lubridate", 
  "htmltools", 
  "shinyjs", 
  "rmarkdown",
  "kableExtra", 
  "ape", 
  "dendextend", 
  "igraph", 
  "ggraph", 
  "tidygraph",
  "treemapify",  
  "scales",      
  "tigris",      
  "leaflet"     
))

install.packages("rsconnect") 

#################################
# Load libraries

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
library(kableExtra)
library(ape)
library(dendextend)
library(igraph)
library(ggraph)
library(tidygraph)
library(treemapify)  
library(scales)     
library(tigris)     
library(leaflet)    
library(rsconnect)

# Specific color palettes
improved_serotype_colors <- c(
  "Agbeni" = "#BA6000",      
  "Braenderup" = "#4B0082",  
  "Enteritidis" = "#1E90FF",  
  "I 4:i:-" = "#696969",      
  "Infantis" = "#AF1493",    
  "Javiana" = "#006400",      
  "Newport" = "#4baaa2",     
  "Paratyphi B var. L(+) tartrate+" = "#FF8C00",
  "Senftenberg" = "#9932CC",  
  "Stanley" = "#FFAA99",      
  "Saintpaul" = "#00CED1",   
  "Thompson" = "#2E8B57",    
  "Typhimurium" = "#FD507A", 
  "Typhi" = "#EA1",   
  "Paratyphi A" = "#FF4500",  
  "Paratyphi B" = "#CA6347", 
  "Paratyphi C" = "#F07F50",  
  "Africana" = "#000080",     
  "Other" = "#A9A9A9"        
)

# Using color blind friendly palettes
serotype_colors <- colorRampPalette(c("#E69F00", "#96B4d9", "#AA9E73", "#61A111", "#0072B2", 
  "#D55E00", "#CC79A7", "#9dd", "#4AA999", "#882255"))(20)  

age_colors <- viridis(5, option = "D")  
specimen_colors <- brewer.pal(8, "Dark2")
outbreak_colors <- colorRampPalette(brewer.pal(8, "Set2"))(15)

# Set up the UI
ui <- dashboardPage(
  dashboardHeader(title = "Indiana Salmonella Surveillance"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Serotype Analysis", tabName = "serotypes", icon = icon("bacteria")),
      menuItem("MLST Typing", tabName = "mlst", icon = icon("dna")),
      menuItem("Geographic Distribution", tabName = "geography", icon = icon("map-location-dot")),
      menuItem("Detailed Heatmap", tabName = "detailed_heatmap", icon = icon("fire")),
      menuItem("Outbreaks", tabName = "outbreaks", icon = icon("biohazard")),
      menuItem("Indiana Outbreaks", tabName = "indiana_outbreaks", icon = icon("virus-covid")),
      menuItem("Settings", tabName = "settings", icon = icon("gear"))
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    # CSS for responsive value boxes
    tags$head(
      tags$style(HTML("
    /* Make value boxes more responsive */
    .small-box {
      border-radius: 5px;
      box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
      overflow: hidden !important;
      min-height: 120px;
    }
    .small-box h3 {
      font-size: 20px !important;
      white-space: normal !important;
      word-wrap: break-word !important;
      overflow: visible !important;
    }
    .small-box p {
      font-size: 13px !important;
      white-space: normal !important;
      word-wrap: break-word !important;
    }
    .small-box .icon-large {
      font-size: 60px !important;
      top: 10px !important;
      right: 10px !important;
    }
    /* Fixed color classes */
    .bg-green {
      background-color: #009E73 !important;
      color: white !important;
    }
    .bg-red {
      background-color: #D55E00 !important;
      color: white !important;
    }
    .bg-purple {
      background-color: #0079B2 !important;
      color: white !important;
    }
    .bg-blue {
      background-color: #56B4E9 !important;
      color: white !important;
    }

     /* Significantly increase date range input font size */
    .dateRangeInput label {
      font-size: 18px !important; /* Larger label text */
      font-weight: 500 !important;
    }
    
    /* Target the actual date input fields */
    .dateRangeInput input.form-control {
      font-size: 18px !important; /* Larger input text */
      height: 40px !important;    /* Taller input boxes */
      padding: 8px 12px !important; /* More padding */
    }
    
    /* Style the separator between date fields */
    .dateRangeInput .input-group-addon {
      font-size: 18px !important;
      padding: 8px !important;
      min-width: 40px !important;
    }
    
    /* Make the date picker calendar larger when opened */
    .datepicker {
      font-size: 16px !important;
    }
    
    /* Make the date range area have more space */
    #date_range {
      margin-bottom: 15px !important;
    }
    
    /* Increase value box content size */
    .small-box h3 {
      font-size: 24px !important; /* Previously 20px - increased by 4 points */
      font-weight: 600 !important;
    }
    
    .small-box p {
      font-size: 17px !important; /* Previously 13px - increased by 4 points */
      font-weight: 500 !important;
    }
    
    /* Make sure the boxes are sized appropriately for larger text */
    .small-box {
      min-height: 130px !important; /* Give a bit more room for larger text */
    }
    
    /* Ensure the icons scale appropriately */
    .small-box .icon-large {
      font-size: 70px !important; /* Larger icon to match text */
    }
    
 /* Improve container layout for the detailed heatmap tab */
    .box {
      display: flex;
      flex-direction: column;
    }
    
    .box-body {
      flex: 1;
      overflow: auto;
      padding-bottom: 15px;
    }
    
    /* Make the plot fill available space */
    .plotly, .dataTables_wrapper {
      height: calc(100% - 30px) !important;
    }
    
    /* Style for description text */
    .box-primary .box-header {
      background-color: #3c8dbc;
      color: white;
    }
    
    /* Better styling for download buttons */
    .btn-default {
      margin-top: 5px;
      margin-bottom: 10px;
    }
    
    /* Ensure tables display properly */
    .dataTable {
      width: 100% !important;
    }
    
    /* Improve serotype labels in clinical serotypes plot */
    .clinical-serotypes-plot .xtick text {
      text-anchor: middle !important;
      transform: rotate(0deg) !important; /* Remove the 45 degree angle */
      font-size: 12px !important; /* Larger font */
      font-weight: 500 !important; /* Bolder text */
      dy: 5px !important; /* Add a bit more space below */
    }
    
    /* Ensure proper padding at the bottom for labels */
    .clinical-serotypes-plot .plotly {
      padding-bottom: 25px !important;
    }
    
    
    /* Ensure Geographic Distribution box has proper padding */
    .box-body {
      padding: 10px !important;
      overflow: visible !important;
    }
    
    /* Ensure all plot outputs resize properly */
    .shiny-plot-output {
      width: 100% !important;
    }
    
  "))
    ),
    
    tabItems(
      # Dashboard Tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            title = "Surveillance Period",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            dateRangeInput(
              "date_range", 
              "Select Date Range:",
              start = "2023-01-01",
              end = Sys.Date(),
              min = "2023-01-01",
              max = Sys.Date()
            )
          )
        ),
        fluidRow(
          valueBoxOutput("total_cases_box", width = 3),
          valueBoxOutput("enteritidis_box", width = 3),
          valueBoxOutput("blood_isolates_box", width = 3),
          valueBoxOutput("demographics_box", width = 3)
        ),
        fluidRow(
          box(
            title = "Serotype Trends Over Time",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotlyOutput("serotype_trend_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Clinically Significant Serotypes",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            plotlyOutput("clinical_serotypes_plot", height = "300px")
          )
        ),
        fluidRow(
          box(
            title = "Age Group Distribution by Sex",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            plotlyOutput("age_sex_plot", height = "350px")
          ),
          box(
            title = "Specimen Distribution",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            plotlyOutput("specimen_plot", height = "350px")
          )
        )
      ),
      
      # Serotype Analysis Tab
      tabItem(
        tabName = "serotypes",
        fluidRow(
          box(
            title = "Serotype Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("serotype_distribution", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Serotype by Specimen Type",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("serotype_specimen", height = "400px")
          ),
          box(
            title = "Serotype by Age Group",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("serotype_age", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Monthly Serotype Heatmap",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("serotype_heatmap", height = "500px")
          )
        )
      ),
      
      # MLST Typing Tab
      tabItem(
        tabName = "mlst",
        fluidRow(
          box(
            title = "Top MLST Sequence Types",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("mlst_distribution", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "MLST-Serotype Correlation",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("mlst_serotype_correlation", height = "500px")
          ),
          box(
            title = "ANI Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("ani_distribution", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "MLST Sequence Types Table",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("mlst_table")
          )
        )
      ),
      
      # Geographic Distribution Tab
      tabItem(
        tabName = "geography",
        fluidRow(
          box(
            title = "Genetic Clusters by ANI",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("genetic_clusters_plot", height = "600px")
          )
        ),
        fluidRow(
          box(
            title = "County Cases by Serotype",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "700px",
            selectInput("county_select", "Select County:", choices = c("All Counties")),
            plotlyOutput("county_serotype", height = "600px")
          ),
          box(
            title = "Top Counties",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "700px",
            plotlyOutput("top_counties", height = "600px")
          )
        )
      ),
      
      tabItem(
        tabName = "detailed_heatmap",
        fluidRow(
          box(
            title = "Detailed Serotype Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p(style = "font-size: 16px;", "This tab shows the distribution of Salmonella serotypes by county and provides diversity metrics.")
          )
        ),
        fluidRow(
          box(
            title = "Detailed Serotype Heatmap by County",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "750px",
            plotlyOutput("serotype_county_heatmap", height = "650px"),
            div(style = "text-align: center; padding: 12px;",
              downloadButton("download_heatmap", "Download Heatmap")
            )
          ),
          box(
            title = "Serotype Diversity Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "750px",
            DTOutput("diversity_table"),
            div(style = "text-align: center; padding: 12px; margin-top: 10px;",
              downloadButton("download_diversity_analysis", "Download Diversity Analysis"))
          )
        )
      ),
      
      # Outbreaks Tab
      tabItem(
        tabName = "outbreaks",
        fluidRow(
          box(
            title = "Significant Outbreaks (3+ Cases)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "700px", 
            plotlyOutput("outbreaks_plot", height = "600px") 
          )
        ),
        fluidRow(
          box(
            title = "Outbreaks Timeline",
            status = "primary",
            solidHeader = TRUE, 
            width = 12,
            height = "700px", 
            plotlyOutput("outbreak_timeline", height = "600px")
          )
        ),
        # Adding CSS for better visualization height management
        tags$head(
          tags$style(HTML("
          /* Make box content fill the available space */
         .box-body {
           height: calc(100% - 45px) !important;
           overflow: hidden;
         }
      
         /* Make plotly outputs fill the available space */
         .plotly {
           height: 100% !important;
         }
      
         /* Ensure the full tab height is utilized */
         .tab-content {
           height: calc(100vh - 150px);
           overflow-y: auto;
         }
        "))
        )
      ),
      
      # Indiana Outbreaks Tab
      tabItem(
        tabName = "indiana_outbreaks",
        fluidRow(
          column(12,
            div(style = "margin-bottom: 15px;",
              selectizeInput(
                "outbreak", "Outbreak code:",
                choices  = c("All", ""), 
                selected = "All",
                width    = "100%"
              )
            )
          )
        ),
        
        tabsetPanel(
          tabPanel("Treemap",
            div(
              plotOutput("outbreaks_treemap", height = "70vh"),
              div(style = "margin-top: 15px; margin-bottom: 20px; font-size: 14pt; text-align: center;",
                "This treemap visualizes the distribution of Salmonella serotypes across different outbreaks. 
          Each colored rectangle represents a serotype, with the size indicating the number of cases."
              )
            )
          ),
          tabPanel("Sankey",
            div(style = "height: 100vh; padding-bottom: 0;", 
              plotlyOutput("outbreaks_sankey", height = "calc(100vh - 130px)") 
            )
          ),
          
          # Bubble Chart tab 
          tabPanel("Bubble Chart",
            div(style = "height: 90vh; padding-bottom: 0;", 
              plotlyOutput("outbreaks_bubble", height = "calc(90vh - 125px)")
            )
          ),
          
          tabPanel("Map & Bar",
            fluidRow(
              column(5, 
                div(style = "height: 80vh;", 
                  leafletOutput("outbreaks_map", height = "100%", width = "100%") 
                )
              ),
              column(7, 
                div(style = "height: 80vh;",
                  plotOutput("outbreaks_countyBar", height = "100%", width = "100%") 
                )
              )
            )
          ),
          tabPanel("Data Table",
            div(
              DTOutput("outbreaks_table")
            )
          )
        )
      ),
      
      # Settings Tab
      tabItem(
        tabName = "settings",
        fluidRow(
          box(
            title = "Report Generation",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            h4("Download Reports"),
            p("Generate reports based on the currently filtered data."),
            br(),
            
            # Summary Report Options
            h5("Summary Report", style = "font-weight: bold;"),
            p("Contains all charts and tables from the dashboard with brief explanations."),
            fluidRow(
              column(6, downloadButton("download_summary_pdf", "PDF Format")),
              column(6, downloadButton("download_summary_word", "Word Format"))
            ),
            br(), hr(), br(),
            
            # Full Report Options
            h5("Full Report", style = "font-weight: bold;"),
            p("Contains detailed analysis, explanations, and appendices with recommendations."),
            fluidRow(
              column(6, downloadButton("download_full_pdf", "PDF Format")),
              column(6, downloadButton("download_full_word", "Word Format"))
            ),
            br(),
            p("Note: Report generation may take up to a minute depending on the amount of data.")
          ),
          box(
            title = "Raw Data Download",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            p("Download the currently filtered dataset for further analysis."),
            br(),
            downloadButton("download_data", "Download Filtered Dataset (CSV)")
          )
        ),
        fluidRow(
          box(
            title = "Raw Data Preview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("data_preview")
          )
        ),
        fluidRow(
          box(
            title = "About This Dashboard",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tags$div(
              tags$h4("Indiana Salmonella Surveillance Dashboard"),
              tags$p("This dashboard provides comprehensive surveillance information for Salmonella cases in Indiana from January 2023 onwards."),
              tags$p("The dashboard includes:"),
              tags$ul(
                tags$li("Case distribution by serotype, specimen type, and demographics"),
                tags$li("Geographic distribution of cases at the county level"),
                tags$li("MLST sequence typing analysis"),
                tags$li("Outbreak tracking and analysis"),
                tags$li("Downloadable reports with detailed analysis")
              ),
              tags$p("For questions or support, please contact the Indiana Department of Health Epidemiology Resource Center.")
            )
          )
        )
      )
    )
  )
)

# Load and prepare the data
load_data <- function() {
  
  # Load the Salmonella data
  salmonella_data <- read.csv("Salmonella_May.csv", stringsAsFactors = FALSE)
  
  # Initial debugging
  print(paste("Raw date format check:", head(salmonella_data$Collection_Date, 10)))
  print(paste("Raw date class:", class(salmonella_data$Collection_Date)))
  
  # Initial cleaning
  salmonella_data$Collection_Date <- as.character(salmonella_data$Collection_Date)
  
  # After converting to character
  print(paste("Character date format check:", head(salmonella_data$Collection_Date, 10)))
  print(paste("Character date class:", class(salmonella_data$Collection_Date)))
  
  # Handle non-date values first
  salmonella_data$Collection_Date[salmonella_data$Collection_Date == "N/A" | 
      salmonella_data$Collection_Date == ""] <- NA
  
  # Using lubridate for more flexible date parsing
  parsed_dates <- mdy(salmonella_data$Collection_Date)
  
  # After parsing with lubridate
  print(paste("Parsed date format check:", head(parsed_dates, 10)))
  print(paste("Parsed date class:", class(parsed_dates)))
  
  # Update the Collection_Date column with parsed dates
  salmonella_data$Collection_Date <- parsed_dates
  
  # After updating the Collection_Date column
  print(paste("Updated date format check:", head(salmonella_data$Collection_Date, 10)))
  print(paste("Updated date class:", class(salmonella_data$Collection_Date)))
  
  # Filter for data from January 2023 onwards
  salmonella_data <- salmonella_data %>%
    filter(!is.na(Collection_Date) & Collection_Date >= as.Date("2023-01-01"))
  
  # After filtering for 2023 onwards
  print(paste("Filtered date format check:", head(salmonella_data$Collection_Date, 10)))
  print(paste("Filtered date class:", class(salmonella_data$Collection_Date)))
  print(paste("Number of rows after date filtering:", nrow(salmonella_data)))
  
  # Handle NA values in all columns EXCEPT Collection_Date
  columns_to_clean <- setdiff(names(salmonella_data), "Collection_Date")
  
  for (col in columns_to_clean) {
    salmonella_data[[col]] <- ifelse(is.na(salmonella_data[[col]]) | 
        salmonella_data[[col]] == "N/A", 
      "Unknown", 
      salmonella_data[[col]])
  }
  
  # Standardize state field
  salmonella_data <- salmonella_data %>%
    mutate(State = case_when(
      State == "IN" ~ "Indiana",
      State != "Indiana" & State != "Unknown" ~ "Out-of-state",
      TRUE ~ State
    ))
  
  # Additional processing for numeric columns
  
  salmonella_data$Patient_Age_Years <- as.numeric(as.character(salmonella_data$Patient_Age_Years))
  salmonella_data$Patient_Age_Months <- as.numeric(as.character(salmonella_data$Patient_Age_Months))
  
  # Create age groups
  salmonella_data <- salmonella_data %>%
    mutate(Age_Group = case_when(
      is.na(Patient_Age_Years) ~ "Unknown",
      Patient_Age_Years < 1 ~ "< 1 year",
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
  
  # Creating year-month field for trending
  salmonella_data <- salmonella_data %>%
    mutate(Year_Month = format(Collection_Date, "%Y-%m"))
  
  # Just before returning the data
  print(paste("Final date format check:", head(salmonella_data$Collection_Date, 10)))
  print(paste("Final date class:", class(salmonella_data$Collection_Date)))
  print(paste("Final number of rows:", nrow(salmonella_data)))
  
  return(salmonella_data)
}

# New function to load outbreak-specific data
load_outbreak_data <- function() {
  # Load outbreak-specific data for the Indiana Outbreaks tab
  outbreak_data <- read.csv("Salmonella_Outbreaks_Only.csv", stringsAsFactors = FALSE) %>%
    filter(
      !Serotype_wgs %in% c("N/A", "Needs further review"),
      !is.na(Outbreak),
      !is.na(Collection_Date)
    ) %>%
    mutate(
      Collection_Date = as.Date(Collection_Date, format = "%m/%d/%Y"),
      Serotype = Serotype_wgs
    )
  
  return(outbreak_data)
}

# Function to load county geometry
load_county_geometry <- function() {
  # Preload county geometry
  counties_sf <- suppressMessages(
    counties(state = "IN", cb = TRUE) %>%
      st_as_sf() %>%
      rename(County = NAME)
  )
  
  return(counties_sf)
}

##################      server     ############################

server <- function(input, output, session) {
  # Reactive data filtering based on date range
  filtered_data <- reactive({
    data <- load_data()
    data %>% filter(Collection_Date >= input$date_range[1] & 
        Collection_Date <= input$date_range[2])
  })
  
  # Load outbreak data for the Outbreaks tab
  outbreak_data <- reactive({
    load_outbreak_data()
  })
  
  # Load county geometry for maps
  county_geometry <- reactive({
    load_county_geometry()
  })
  
  # Filtered outbreak data based on selection
  outbreak_filtered <- reactive({
    if(input$outbreak == "All") {
      outbreak_data()
    } else {
      outbreak_data() %>% 
        filter(Outbreak == input$outbreak)
    }
  })
  
  # Update county selection choices based on available data
  observe({
    counties <- c("All Counties", unique(filtered_data()$County))
    updateSelectInput(session, "county_select", choices = counties)
  })
  
  # Update outbreak selection choices for main dashboard
  observe({
    outbreaks <- filtered_data() %>%
      filter(Outbreak != "Unknown" & Outbreak != "N/A") %>%
      group_by(Outbreak) %>%
      summarize(count = n()) %>%
      filter(count >= 3) %>%
      pull(Outbreak)
    
    updateSelectInput(session, "outbreak_select", 
      choices = c("All Outbreaks", outbreaks))
  })
  
  # Update outbreak selection for Indiana Outbreaks tab
  observe({
    outbreaks <- unique(outbreak_data()$Outbreak)
    updateSelectizeInput(session, "outbreak", 
      choices = c("All", sort(outbreaks)))
  })
  
  
  
  
  #===============================================
  # Dashboard tab outputs
  #===============================================
  
  # Summary boxes
  output$total_cases_box <- renderValueBox({
    total <- nrow(filtered_data())
    valueBox(
      value = total,
      subtitle = "TOTAL CONFIRMED CASES",
      icon = icon("clipboard-list"),
      color = "blue"
    )
  })
  
  output$enteritidis_box <- renderValueBox({
    enteritidis_count <- filtered_data() %>%
      filter(Serotype_wgs == "Enteritidis") %>%
      nrow()
    
    total <- nrow(filtered_data())
    percentage <- round((enteritidis_count / total) * 100, 1)
    
    valueBox(
      value = "ENTERITIDIS",
      subtitle = paste0(enteritidis_count, " CASES (", percentage, "%)"),
      icon = icon("bacteria"),
      color = "green"
    )
  })
  
  output$blood_isolates_box <- renderValueBox({
    blood_count <- filtered_data() %>%
      filter(Specimen == "Blood") %>%
      nrow()
    
    total <- nrow(filtered_data())
    percentage <- round((blood_count / total) * 100, 1)
    
    valueBox(
      value = "BLOOD ISOLATES",
      subtitle = paste0(blood_count, " CASES (", percentage, "%)"),
      icon = icon("droplet"),
      color = "red"
    )
  })
  
  output$demographics_box <- renderValueBox({
    # Calculate median age
    ages <- filtered_data() %>%
      filter(Patient_Age_Years != "Unknown") %>%
      pull(Patient_Age_Years) %>%
      as.numeric()
    
    median_age <- if(length(ages) > 0) round(median(ages), 1) else "Unknown"
    
    # Calculate sex percentages
    total <- nrow(filtered_data())
    female_pct <- round((sum(filtered_data()$Sex == "FEMALE") / total) * 100, 1)
    male_pct <- round((sum(filtered_data()$Sex == "MALE") / total) * 100, 1)
    
    valueBox(
      value = paste0("AGE: ", median_age, " YRS"),
      subtitle = paste0(female_pct, "% FEMALE, ", male_pct, "% MALE"),
      icon = icon("users"),
      color = "purple"
    )
  })
  
  # Serotype trend plot with updated colors and smaller elements
  output$serotype_trend_plot <- renderPlotly({
    # Get top 5 serotypes for the selected period
    top_serotypes <- filtered_data() %>%
      count(Serotype_wgs) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      pull(Serotype_wgs)
    
    # Prepare monthly trend data
    trend_data <- filtered_data() %>%
      filter(Serotype_wgs %in% top_serotypes) %>%
      mutate(Year_Month = as.Date(paste0(Year_Month, "-01"))) %>%
      count(Year_Month, Serotype_wgs) %>%
      complete(Year_Month, Serotype_wgs, fill = list(n = 0))
    
    # Create the plot with improved colors
    p <- ggplot(trend_data, aes(x = Year_Month, y = n, color = Serotype_wgs, group = Serotype_wgs)) +
      geom_line(size = 0.7) +
      geom_point(size = 1.5) +
      scale_color_manual(values = improved_serotype_colors) +
      theme_minimal() +
      labs(
        x = "Month",
        y = "Number of Cases",
        color = "Serotype"
      ) +
      theme(
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(legend = list(orientation = "v", y = 0.5))
  })
  
  # Clinical serotypes plot
  output$clinical_serotypes_plot <- renderPlotly({
    # Define clinically important serotypes
    clinical_serotypes <- c("Typhi", "Paratyphi A", "Paratyphi B", 
      "Paratyphi B var. L(+) tartrate+", "Paratyphi C")
    
    # Filter for these serotypes
    clinical_data <- filtered_data() %>%
      filter(Serotype_wgs %in% clinical_serotypes)
    
    # Check if we have any data
    if (nrow(clinical_data) > 0) {
      # Count by serotype and specimen
      plot_data <- clinical_data %>%
        count(Serotype_wgs, Specimen) %>%
        mutate(is_invasive = ifelse(Specimen == "Blood", "Invasive", "Non-invasive"))
      
      # Create plot
      p <- ggplot(plot_data, aes(x = Serotype_wgs, y = n, fill = is_invasive)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c("Invasive" = "#D05E00", "Non-invasive" = "#DDDCCC")) +
        scale_y_continuous(breaks = seq(0, 12, by = 2)) + 
        theme_minimal() +
        labs(
          title = "Clinically Significant Serotypes",
          x = "Serotype",
          y = "Number of Cases",
          fill = "Invasiveness"
        ) +
        theme(
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11, face = "bold"), 
          plot.margin = margin(5, 5, 20, 5) 
        )
      
      # Convert to plotly with added class for CSS targeting
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(margin = list(b = 80)) %>% 
        htmlwidgets::onRender("
      function(el) {
        el.classList.add('clinical-serotypes-plot');
      }
    ")
    }
  })
  
  # Age group by sex plot
  output$age_sex_plot <- renderPlotly({
    age_sex_data <- filtered_data() %>%
      filter(Sex %in% c("MALE", "FEMALE"), Age_Group != "Unknown") %>%
      count(Age_Group, Sex) %>%
      complete(Age_Group, Sex, fill = list(n = 0))
    
    # Order age groups correctly
    age_order <- c("< 1 year", "1-5 years", "6-18 years", "19-65 years", "> 65 years")
    age_sex_data$Age_Group <- factor(age_sex_data$Age_Group, levels = age_order)
    
    p <- ggplot(age_sex_data, aes(x = Age_Group, y = n, fill = Sex)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("FEMALE" = "#C179A7", "MALE" = "#0072B2")) +
      theme_minimal() +
      labs(
        x = "Age Group",
        y = "Number of Cases",
        fill = "Sex"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(legend = list(orientation = "h", y = 1.1))
  })
  
  # Specimen distribution plot
  output$specimen_plot <- renderPlotly({
    specimen_data <- filtered_data() %>%
      count(Specimen) %>%
      arrange(desc(n)) %>%
      filter(Specimen != "Unknown")
    
    # New distinct color palette 
    specimen_colors <- c(
      "Stool" = "#F17E80",   
      "Urine" = "#92b5F5",   
      "Blood" = "#FF5252",    
      "Wound" = "#66BB6A",   
      "Other" = "#11199e9e",    
      "Beef" = "#FF7043",    
      "Melon" = "#26A69A",    
      "Abscess" = "#EC407A",  
      "Sputum" = "#5E57C2",   
      "Gall Bladder" = "#1C6BC0"
    )
    
    p <- ggplot(specimen_data, aes(x = reorder(Specimen, n), y = n, fill = Specimen)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = specimen_colors) +
      coord_flip() +
      scale_y_continuous(
        breaks = c(0, 200, 400, 600, 800, 1000, 1200),
        limits = c(0, 1500)  
      ) +
      theme_minimal() +
      labs(
        title = "Specimen Distribution",
        x = "Specimen Type",
        y = "Number of Cases",
        fill = "Specimen Type"
      ) +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#E5E5E5"),
        legend.position = "none"
      )
    
    # Add count labels to the bars
    p <- p + geom_text(
      aes(label = n), 
      hjust = -0.2, 
      color = "black", 
      size = 3.5,
      position = position_stack(vjust = 0.5)
    )
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Clinical info box
  output$clinical_info_box <- renderUI({
    # Count clinically important serotypes
    clinical_serotypes <- c("Typhi", "Paratyphi A", "Paratyphi B", 
      "Paratyphi B var. L(+) tartrate+", "Paratyphi C")
    
    clinical_count <- filtered_data() %>%
      filter(Serotype_wgs %in% clinical_serotypes) %>%
      nrow()
    
    blood_clinical_count <- filtered_data() %>%
      filter(Serotype_wgs %in% clinical_serotypes, Specimen == "Blood") %>%
      nrow()
    
    # Create information box
    box(
      title = "About Clinically Significant Serotypes",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      HTML(paste0(
        "<p><strong>Typhi and Paratyphi</strong> serotypes cause enteric fever, a severe systemic infection requiring prompt medical attention and antimicrobial therapy.</p>",
        "<p><strong>Current surveillance period:</strong> ", clinical_count, " cases detected, with ", 
        blood_clinical_count, " invasive infections (", 
        ifelse(clinical_count > 0, round(blood_clinical_count/clinical_count*100, 1), 0), 
        "% invasive rate).</p>",
        "<p>These serotypes are associated with travel to endemic regions and require special public health attention due to their severity and transmission patterns.</p>"
      ))
    )
  })
  
  
  
  
  
  #===============================================
  # Serotype Analysis tab outputs
  #===============================================
  
  # Serotype distribution
  distinct_colors <- c(
    "#3B5998", 
    "#FFA500", 
    "#9932CC", 
    "#90B2AA",  
    "#FF6347", 
    "#6A5ACD",  
    "#F08080",  
    "#DAA520", 
    "#9682B4",  
    "#099ED3"   
  )
  
  output$serotype_distribution <- renderPlotly({
    serotype_data <- filtered_data() %>%
      count(Serotype_wgs) %>%
      arrange(desc(n)) %>%
      top_n(10)
    
    p <- ggplot(serotype_data, aes(x = reorder(Serotype_wgs, n), y = n, fill = Serotype_wgs)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = distinct_colors) +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Top 10 Salmonella Serotypes",
        x = "Serotype",
        y = "Number of Cases"
      ) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Serotype by specimen type
  diverse_colors2 <- c(
    "#377EB8", "#FF7F00", "#F781BF", "#000080", "#66C2A5", "#8DA0CB")
  
  output$serotype_specimen <- renderPlotly({
    top_serotypes <- filtered_data() %>%
      count(Serotype_wgs) %>%
      top_n(6) %>%
      pull(Serotype_wgs)
    
    serotype_specimen_data <- filtered_data() %>%
      filter(Serotype_wgs %in% top_serotypes, Specimen != "Unknown") %>%
      count(Serotype_wgs, Specimen)
    
    p <- ggplot(serotype_specimen_data, aes(x = Serotype_wgs, y = n, fill = Specimen)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_manual(values = diverse_colors2) +
      theme_minimal() +
      labs(
        title = "Specimen Distribution by Serotype",
        x = "Serotype",
        y = "Proportion",
        fill = "Specimen Type"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Serotype by age group
  output$serotype_age <- renderPlotly({
    top_serotypes <- filtered_data() %>%
      count(Serotype_wgs) %>%
      top_n(5) %>%
      pull(Serotype_wgs)
    
    # Order age groups correctly
    age_order <- c("< 1 year", "1-5 years", "6-18 years", "19-65 years", "> 65 years")
    
    serotype_age_data <- filtered_data() %>%
      filter(Serotype_wgs %in% top_serotypes, Age_Group != "Unknown") %>%
      count(Serotype_wgs, Age_Group) %>%
      mutate(Age_Group = factor(Age_Group, levels = age_order))
    
    p <- ggplot(serotype_age_data, aes(x = Serotype_wgs, y = n, fill = Age_Group)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_manual(values = distinct_colors) +
      theme_minimal() +
      labs(
        title = "Age Distribution by Serotype",
        x = "Serotype",
        y = "Proportion",
        fill = "Age Group"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Serotype heatmap
  output$serotype_heatmap <- renderPlotly({
    # Include clinically important serotypes 
    clinical_serotypes <- c("Typhi", "Paratyphi A", "Paratyphi B", "Paratyphi C")
    
    # Get top common serotypes
    top_common_serotypes <- filtered_data() %>%
      count(Serotype_wgs) %>%
      filter(!(Serotype_wgs %in% clinical_serotypes)) %>%
      top_n(6) %>%  # Reduced from 8 to make room for clinical serotypes
      pull(Serotype_wgs)
    
    # Combine clinical and common serotypes
    display_serotypes <- c(clinical_serotypes, top_common_serotypes)
    
    # Get date range for complete month sequence
    date_range <- range(filtered_data()$Collection_Date, na.rm = TRUE)
    first_month <- floor_date(date_range[1], "month")
    last_month <- floor_date(date_range[2], "month")
    
    # Create a sequence of all months in the range
    all_months <- seq(from = first_month, to = last_month, by = "1 month")
    
    # Filter to include only our selected serotypes
    heatmap_data <- filtered_data() %>%
      filter(Serotype_wgs %in% display_serotypes) %>%
      mutate(Year_Month = as.Date(paste0(Year_Month, "-01"))) %>%
      count(Year_Month, Serotype_wgs) %>%
      # Complete the grid for all months and selected serotypes
      complete(Year_Month = all_months, Serotype_wgs = display_serotypes, fill = list(n = 0))
    
    # Create a factor for serotypes to control their order (clinical serotypes first)
    heatmap_data$Serotype_wgs <- factor(
      heatmap_data$Serotype_wgs, 
      levels = c(clinical_serotypes, top_common_serotypes)
    )
    
    # Define custom color palette
    custom_palette <- c("#d3f8ff",  "#3E90aa",  "#000080")
    
    p <- ggplot(heatmap_data, aes(x = Year_Month, y = Serotype_wgs, fill = n)) +
      geom_tile(color = "white") +
      # Use the original custom color palette
      scale_fill_gradientn(
        colors = custom_palette,
        name = "Cases",
        
        values = c(0, 0.3, 0.6, 1)
      ) +
      theme_minimal() +
      labs(
        title = "Monthly Serotype Distribution",
        x = "Month",
        y = "Serotype",
        fill = "Cases"
      ) +
      # Show every month
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b %Y",
        expand = c(0.01, 0.01)
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        plot.title = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  
  
  #===============================================
  # MLST tab outputs
  #===============================================
  
  # MLST distribution
  output$mlst_distribution <- renderPlotly({
    # Get data with MLST and most common serotype
    mlst_data <- filtered_data() %>%
      filter(MLST_ST != "Unknown") %>%
      group_by(MLST_ST) %>%
      mutate(CommonSerotype = names(which.max(table(Serotype_wgs)))) %>%
      ungroup() %>%
      count(MLST_ST, CommonSerotype) %>%
      arrange(desc(n)) %>%
      top_n(10)
    
    # Create combined labels
    mlst_data <- mlst_data %>%
      mutate(MLST_Label = paste0(MLST_ST, " (", CommonSerotype, ")"))
    
    # Create the plot with combined labels
    p <- ggplot(mlst_data, aes(x = reorder(MLST_Label, n), y = n, fill = MLST_Label)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_d(option = "A") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Top 10 MLST Sequence Types",
        x = "MLST Sequence Type (Serotype)",
        y = "Number of Isolates"
      ) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # MLST-Serotype correlation
  output$mlst_serotype_correlation <- renderPlotly({
    # Get top MLST types for correlation visualization
    top_mlst <- filtered_data() %>%
      filter(MLST_ST != "Unknown") %>%
      count(MLST_ST) %>%
      arrange(desc(n)) %>%
      top_n(5) %>%
      pull(MLST_ST)
    
    # Get data showing the correlation between MLST types and serotypes
    mlst_serotype_data <- filtered_data() %>%
      filter(MLST_ST %in% top_mlst) %>%
      count(MLST_ST, Serotype_wgs) %>%
      group_by(MLST_ST) %>%
      # For each MLST type, get the top 3 serotypes
      top_n(3, n) %>%
      ungroup()
    
    # Create the correlation plot
    p <- ggplot(mlst_serotype_data, aes(x = MLST_ST, y = n, fill = Serotype_wgs)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_viridis_d(option = "D") +
      theme_minimal() +
      labs(
        title = "MLST-Serotype Correlation",
        x = "MLST Sequence Type",
        y = "Number of Isolates",
        fill = "Serotype"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # ANI distribution
  output$ani_distribution <- renderPlotly({
    # Simplify the ANI analysis by creating broader ANI range categories
    ani_data <- filtered_data() %>%
      mutate(
        ANI_numeric = as.numeric(as.character(ANI)),
        ANI_Range = case_when(
          is.na(ANI_numeric) ~ "Unknown",
          ANI_numeric >= 99.0 ~ ">99%",
          ANI_numeric >= 95.0 & ANI_numeric < 99.0 ~ "95-99%",
          TRUE ~ "<95%"
        )
      ) %>%
      filter(ANI_Range != "Unknown") %>%
      count(ANI_Range, Serotype_wgs) %>%
      # Get only top serotypes for better visualization
      group_by(Serotype_wgs) %>%
      summarize(Total = sum(n)) %>%
      top_n(7, Total) %>%
      pull(Serotype_wgs) -> top_sero
    
    # Prepare final plot data
    ani_plot_data <- filtered_data() %>%
      mutate(
        ANI_numeric = as.numeric(as.character(ANI)),
        ANI_Range = case_when(
          is.na(ANI_numeric) ~ "Unknown",
          ANI_numeric >= 99.0 ~ ">99%",
          ANI_numeric >= 95.0 & ANI_numeric < 99.0 ~ "95-99%",
          TRUE ~ "<95%"
        ),
        SerotypePlot = ifelse(Serotype_wgs %in% top_sero, Serotype_wgs, "Other")
      ) %>%
      filter(ANI_Range != "Unknown") %>%
      count(ANI_Range, SerotypePlot)
    
    # Order ANI ranges correctly
    ani_order <- c("<95%", "95-99%", ">99%")
    ani_plot_data$ANI_Range <- factor(ani_plot_data$ANI_Range, levels = ani_order)
    
    # Create the plot with improved colors
    p <- ggplot(ani_plot_data, aes(x = ANI_Range, y = n, fill = SerotypePlot)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = improved_serotype_colors) +
      theme_minimal() +
      labs(
        title = "ANI Distribution by Serotype",
        x = "Average Nucleotide Identity (ANI)",
        y = "Number of Isolates",
        fill = "Serotype"
      ) +
      theme(
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 14)
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # MLST table
  output$mlst_table <- renderDT({
    mlst_table_data <- filtered_data() %>%
      group_by(MLST_ST) %>%
      summarize(
        Count = n(),
        `Most Common Serotype` = names(which.max(table(Serotype_wgs))),
        `Invasive Cases` = sum(Specimen == "Blood"),
        `% Invasive` = round(sum(Specimen == "Blood") / n() * 100, 1)
      ) %>%
      filter(MLST_ST != "Unknown") %>%
      arrange(desc(Count)) %>%
      head(20)
    
    datatable(
      mlst_table_data,
      options = list(
        pageLength = 10,
        searching = TRUE,
        dom = 'ftip'
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Count',
        background = styleColorBar(range(mlst_table_data$Count), '#0072B2')
      ) %>%
      formatStyle(
        '% Invasive',
        background = styleColorBar(c(0, 100), '#D55E00')
      )
  })
  
  #===============================================
  # Geographic Distribution tab outputs
  #===============================================
  
  # Genetic clusters plot
  output$genetic_clusters_plot <- renderPlotly({
    # Create ANI clusters data
    ani_data <- filtered_data() %>%
      filter(!is.na(ANI) & ANI != "Unknown" & !is.na(ANI_Coverage) & ANI_Coverage != "Unknown") %>%
      mutate(
        ANI = as.numeric(as.character(ANI)),
        ANI_Coverage = as.numeric(as.character(ANI_Coverage))
      ) %>%
      filter(!is.na(ANI) & !is.na(ANI_Coverage))  # Filter out any conversion errors
    
    # Get top 10 serotypes for coloring and shapes
    top_serotypes <- filtered_data() %>%
      count(Serotype_wgs) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      pull(Serotype_wgs)
    
    # Adding a category for other serotypes
    ani_data <- ani_data %>%
      mutate(SerotypeCat = ifelse(Serotype_wgs %in% top_serotypes, 
        as.character(Serotype_wgs), 
        "Other"))
    
    # Creating scatter plot of ANI vs Coverage
    p <- ggplot(ani_data, aes(x = ANI, y = ANI_Coverage, color = SerotypeCat)) +
      geom_point(alpha = 0.7, size = 3) +
      # Add vertical lines for typical boundaries
      geom_vline(xintercept = 95.0, linetype = "dashed", color = "gray") +
      geom_vline(xintercept = 99.5, linetype = "dashed", color = "gray") +
      # Use viridis color scale
      scale_color_viridis_d(option = "D", name = "Serotype") +
      labs(
        title = "Genetic Clusters by ANI",
        x = "Average Nucleotide Identity (ANI)",
        y = "ANI Coverage (%)",
        color = "Serotype"
      ) +
      theme_minimal() +
      # Add annotations for boundaries
      annotate("text", x = 95.0, y = max(ani_data$ANI_Coverage, na.rm = TRUE), 
        label = "Species boundary", angle = 90, vjust = 1.5, hjust = 1) +
      annotate("text", x = 99.5, y = max(ani_data$ANI_Coverage, na.rm = TRUE), 
        label = "Strain-level similarity", angle = 90, vjust = 1.5, hjust = 1)
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  
  # County serotype plot
  output$county_serotype <- renderPlotly({
    # Filter by selected county if applicable
    if (input$county_select != "All Counties") {
      county_serotype_data <- filtered_data() %>%
        filter(County == input$county_select) %>%
        count(Serotype_wgs) %>%
        arrange(desc(n)) %>%
        top_n(10)
      
      title <- paste0("Serotype Distribution in ", input$county_select, " County")
    } else {
      county_serotype_data <- filtered_data() %>%
        filter(State == "Indiana") %>%
        count(Serotype_wgs) %>%
        arrange(desc(n)) %>%
        top_n(10)
      
      title <- "Serotype Distribution in Indiana"
    }
    
    p <- ggplot(county_serotype_data, aes(x = reorder(Serotype_wgs, n), y = n, fill = Serotype_wgs)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = improved_serotype_colors) +
      coord_flip() +
      theme_minimal() +
      labs(
        title = title,
        x = "",
        y = "Number of Cases"
      ) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y")) %>% 
      layout(
        height = 500, 
        margin = list(l = 120, r = 50, b = 50, t = 50), 
        autosize = TRUE ) 
  })
  
  # Top counties plot
  output$top_counties <- renderPlotly({
    top_counties_data <- filtered_data() %>%
      filter(State == "Indiana", County != "Unknown") %>%
      count(County) %>%
      arrange(desc(n)) %>%
      top_n(15)
    
    p <- ggplot(top_counties_data, aes(x = reorder(County, n), y = n, fill = n)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c(option = "A", direction = -1)+
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Top 15 Counties by Salmonella Case Count",
        x = "",
        y = "Number of Cases"
      ) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y")) %>% 
      layout(
        height = 500,
        margin = list(l = 120, r = 50, b = 50, t = 50), 
        autosize = TRUE 
      )
  })
  
  
  
  #===============================================
  # Detailed Heatmap tab outputs
  #===============================================
  
  # Serotype by county heatmap
  output$serotype_county_heatmap <- renderPlotly({
    # Identify clinically important serotypes
    clinical_serotypes <- c("Typhi", "Paratyphi A", "Paratyphi B", "Paratyphi C")
    
    # Get count of clinical serotypes in data
    clinical_count <- filtered_data() %>%
      filter(Serotype_wgs %in% clinical_serotypes) %>%
      nrow()
    
    # Get top serotypes for better visualization
    top_serotypes <- filtered_data() %>%
      filter(Serotype_wgs != "I 4:i:-") %>%
      count(Serotype_wgs) %>%
      arrange(desc(n)) %>%
      top_n(10) %>%
      pull(Serotype_wgs)
    
    # Make sure clinical serotypes are included if they exist in the data
    if (clinical_count > 0) {
      present_clinical <- filtered_data() %>%
        filter(Serotype_wgs %in% clinical_serotypes) %>%
        pull(Serotype_wgs) %>%
        unique()
      
      top_serotypes <- unique(c(present_clinical, top_serotypes))
    }
    
    # Get top counties
    top_counties <- filtered_data() %>%
      filter(State == "Indiana", County != "Unknown") %>%
      count(County) %>%
      arrange(desc(n)) %>%
      top_n(15) %>%
      pull(County)
    
    # Create heatmap data
    heatmap_data <- filtered_data() %>%
      filter(Serotype_wgs %in% top_serotypes, 
        County %in% top_counties,
        State == "Indiana") %>%
      count(County, Serotype_wgs) %>%
      complete(County, Serotype_wgs, fill = list(n = 0))
    
    # Create the heatmap plot
    p <- ggplot(heatmap_data, aes(x = Serotype_wgs, y = County, fill = n)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#1a1a4c", high = "#F8CF59", name = "Count") +
      theme_minimal() +
      labs(
        title = "Detailed Salmonella Serotypes by County",
        x = "Serotype",
        y = ""
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(
        height = 600, 
        margin = list(l = 100, r = 50, b = 100, t = 50),
        autosize = TRUE ) 
  })
  
  # Diversity metrics table
  output$diversity_table <- renderDT({
    # Calculate diversity metrics by county
    diversity_data <- filtered_data() %>%
      filter(State == "Indiana", County != "Unknown", Serotype_wgs != "Unknown", Serotype_wgs != "I 4:i:-") %>%
      group_by(County) %>%
      summarize(
        `Total Cases` = n(),
        `Serotype Count` = n_distinct(Serotype_wgs),
        # Shannon Diversity Index calculation
        `Shannon Diversity` = round(-sum((table(Serotype_wgs)/n()) * log(table(Serotype_wgs)/n()), na.rm = TRUE), 3),
        # Evenness calculation (Shannon diversity divided by log of richness)
        Evenness = round(`Shannon Diversity`/log(`Serotype Count`), 3)
      ) %>%
      ungroup() %>%
      arrange(desc(`Total Cases`))
    
    datatable(
      diversity_data,
      options = list(
        pageLength = 10,
        searching = TRUE,
        dom = 'ftip'
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Total Cases',
        background = styleColorBar(c(0, max(diversity_data$`Total Cases`)), '#59B4E9')
      ) %>%
      formatStyle(
        'Serotype Count',
        background = styleColorBar(c(0, max(diversity_data$`Serotype Count`)), '#008E73')
      ) %>%
      formatStyle(
        'Shannon Diversity',
        background = styleColorBar(c(0, max(diversity_data$`Shannon Diversity`)), '#C189A7')
      ) %>%
      formatStyle(
        'Evenness',
        background = styleColorBar(c(0, 1), '#E29F00')
      )
  })
  
  #===============================================
  # Outbreaks tab outputs
  #===============================================
  
  # Significant Outbreaks Plot with improved colors and larger size
  output$outbreaks_plot <- renderPlotly({
    outbreak_data <- filtered_data() %>%
      filter(Outbreak != "Unknown" & Outbreak != "N/A") %>%
      group_by(Outbreak) %>%
      summarize(
        Count = n(),
        `Primary Serotype` = names(which.max(table(Serotype_wgs)))
      ) %>%
      filter(Count >= 3) %>%
      arrange(desc(Count))
    
    # Create a diverse color palette for primary serotypes
    serotype_palette <- c(
      "Africana" = "#E41A1C",      
      "Agbeni" = "#984EA3",         
      "Braenderup" = "#FF7900",     
      "Enteritidis" = "#377EB8",    
      "I 4:i:-" = "#4DAF4A",        
      "Infantis" = "#CFFF33",       
      "Javiana" = "#A65628",       
      "Newport" = "#F781BF",      
      "Paratyphi B var. L(+) tartrate+" = "#FFA500",
      "Senftenberg" = "#00CED1",    
      "Stanley" = "#FFAA99",       
      "Saintpaul" = "#9ACD32",      
      "Thompson" = "#8A2BE2",       
      "Typhimurium" = "#000080",    
      "Typhi" = "#DC143C",         
      "Paratyphi A" = "#FF4500",    
      "Paratyphi B" = "#FF6347",    
      "Paratyphi C" = "#FF9F50",   
      "Other" = "#A9A9A9"        
    )
    
    # For any missing serotypes, assign a color from a standard palette
    missing_serotypes <- setdiff(unique(outbreak_data$`Primary Serotype`), names(serotype_palette))
    if(length(missing_serotypes) > 0) {
      extra_colors <- scales::brewer_pal(palette = "Set3")(length(missing_serotypes))
      names(extra_colors) <- missing_serotypes
      serotype_palette <- c(serotype_palette, extra_colors)
    }
    
    # Create an enhanced horizontal bar plot for outbreaks
    p <- ggplot(outbreak_data, aes(x = reorder(Outbreak, Count), y = Count, fill = `Primary Serotype`)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = serotype_palette) +
      coord_flip() +
      geom_text(aes(label = Count), hjust = -0.2, size = 3.5) +
      theme_minimal() +
      labs(
        title = "Significant Outbreaks (3+ Cases)",
        x = "Outbreak ID",
        y = "Number of Cases",
        fill = "Primary Serotype"
      ) +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        # Add padding for the count labels
        plot.margin = margin(5, 60, 5, 5)
      )
    
    # Convert to plotly with improved layout
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        legend = list(orientation = "v", y = 0.5),
        margin = list(r = 100, t = 50, b = 50, l = 100)
      )
  })
  
  # Outbreaks timeline with improved size and visual appeal
  output$outbreak_timeline <- renderPlotly({
    outbreak_timeline_data <- filtered_data() %>%
      filter(Outbreak != "Unknown" & Outbreak != "N/A") %>%
      group_by(Outbreak) %>%
      summarize(
        Start = min(Collection_Date),
        End = max(Collection_Date),
        Count = n(),
        `Primary Serotype` = names(which.max(table(Serotype_wgs))),
        Duration_Days = as.numeric(difftime(End, Start, units = "days"))
      ) %>%
      filter(Count >= 3) %>%
      arrange(Start)
    
    # Add formatted date display
    outbreak_timeline_data <- outbreak_timeline_data %>%
      mutate(
        Start_Label = format(Start, "%b %d, %Y"),
        End_Label = format(End, "%b %d, %Y"),
        Hover_Text = paste0(
          "Outbreak: ", Outbreak, "<br>",
          "Serotype: ", `Primary Serotype`, "<br>",
          "Cases: ", Count, "<br>",
          "Start: ", Start_Label, "<br>",
          "End: ", End_Label, "<br>",
          "Duration: ", Duration_Days, " days"
        )
      )

    
    # For any missing serotypes, assign a color from a standard palette
    missing_serotypes <- setdiff(unique(outbreak_timeline_data$`Primary Serotype`), names(serotype_palette))
    if(length(missing_serotypes) > 0) {
      extra_colors <- scales::brewer_pal(palette = "Set3")(length(missing_serotypes))
      names(extra_colors) <- missing_serotypes
      serotype_palette <- c(serotype_palette, extra_colors)
    }
    
    # Get date range for proper axis formatting
    date_min <- min(outbreak_timeline_data$Start)
    date_max <- max(outbreak_timeline_data$End)
    
    # Calculate sequence of months for the axis
    all_months <- seq(
      from = floor_date(date_min, "month"),
      to = ceiling_date(date_max, "month"),
      by = "1 month"
    )
    
    # Create the plot
    p <- ggplot(outbreak_timeline_data, aes(x = Start, y = reorder(Outbreak, Start), 
      color = `Primary Serotype`, size = Count,
      text = Hover_Text)) +
      geom_point() +
      geom_segment(aes(x = Start, xend = End, y = Outbreak, yend = Outbreak), 
        size = 1, alpha = 0.7) +
      scale_color_manual(values = serotype_palette) +
      scale_size_continuous(range = c(3, 8)) +
      scale_x_date(
        breaks = all_months,
        date_labels = "%b\n%Y",  
        expand = c(0.02, 0.02)
      ) +
      theme_minimal() +
      labs(
        title = "Outbreak Timeline with Monthly Markers",
        x = "Date",
        y = "Outbreak ID",
        color = "Primary Serotype",
        size = "Case Count"
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),  
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid.major.x = element_line(color = "lightgray", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
    
    # Convert to plotly with enhanced tooltip
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(
          tickfont = list(size = 9),
          title = list(font = list(size = 11))
        ),
        yaxis = list(
          tickfont = list(size = 10),
          title = list(font = list(size = 11))
        ),
        legend = list(orientation = "v", y = 0.5),
        margin = list(b = 80, l = 120, r = 50, t = 50) 
      )
  })
  
  #===============================================
  # Indiana Outbreaks tab outputs
  #===============================================
  
  # Treemap - Updated
  output$outbreaks_treemap <- renderPlot({
    outbreak_filtered() %>%
      count(Outbreak, Serotype) %>%
      ggplot(aes(area = n, fill = Serotype, subgroup = Outbreak, label = Serotype)) +
      geom_treemap() +
      geom_treemap_subgroup_border(color = "white", size = 1.5) +
      geom_treemap_text(colour = "white", place = "centre", grow = TRUE, reflow = TRUE, size = 14) +
      theme_void() +
      theme(legend.position = "none") +
      scale_fill_viridis_d(option = "D", direction = -1)
  })
  
  # Plotly Sankey - Fixed layout issues
  output$outbreaks_sankey <- renderPlotly({
    cnt   <- outbreak_filtered() %>% count(Serotype, Outbreak)
    seros <- unique(cnt$Serotype)
    outs  <- unique(cnt$Outbreak)
    nodes <- data.frame(name = c(seros, outs), stringsAsFactors = FALSE)
    
    if(length(seros) > 0 && length(outs) > 0) {
      links <- cnt %>%
        mutate(
          source = match(Serotype, nodes$name) - 1,
          target = match(Outbreak,  nodes$name) - 1
        )
      
      viridis_cols <- viridis(max(1, length(seros)), option = "D")
      node_cols <- c(viridis_cols, rep("#CCCCCC", length(outs)))
      link_cols <- alpha(node_cols[links$source + 1], 0.5)
      
      plot_ly(
        type        = 'sankey',
        orientation = "h",
        node = list(
          label     = nodes$name,
          color     = node_cols,
          pad       = 20,
          thickness = 30,
          line      = list(color = "black", width = 0.5)
        ),
        link = list(
          source = links$source,
          target = links$target,
          value  = links$n,
          color  = link_cols
        )
      ) %>%
        layout(
          font = list(size = 18),
          margin = list(l = 50, r = 50, t = 25, b = 10), 
          autosize = TRUE 
        )
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected filter",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  
  # Bubble Chart - Fixed filter parameter issue
  output$outbreaks_bubble <- renderPlotly({
    cnt <- outbreak_filtered() %>% count(date = Collection_Date, Serotype)
    
    if(nrow(cnt) > 0) {
      unique_serotypes <- unique(cnt$Serotype)
      
      serotype_positions <- data.frame(
        Serotype = unique_serotypes,
        y_position = seq(1, length(unique_serotypes))
      )
      
      cnt <- cnt %>%
        left_join(serotype_positions, by = "Serotype") %>%
        # Add this filter to limit data to 2017 onwards
        filter(date >= as.Date("2017-01-01"))
      
      p <- ggplot(cnt, aes(x = date, y = y_position, size = n, color = Serotype, text = paste("Serotype:", Serotype, "<br>Date:", format(date, "%b %d, %Y"), "<br>Count:", n))) +
        geom_point(alpha = 0.7) +
        scale_y_continuous(
          breaks = serotype_positions$y_position,
          labels = serotype_positions$Serotype
        ) +
        theme_minimal(base_size = 16) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y = element_text(size = 15, hjust = 1),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          # Remove plot margins to maximize space
          plot.margin = margin(5, 5, 5, 5)
        ) +
        scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
        labs(x = NULL, y = NULL)
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          margin = list(l = 180, b = 75, r = 50, t = 25), 
          autosize = TRUE 
        )
    } else {
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected filter",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  
  
  # County bar chart - Fixed selected_outbreak_county function
  # Define reactive for selected county
  selected_outbreak_county <- reactive({
    if(is.null(input$outbreaks_map_shape_click)) {
      # Default to the county with most cases if none selected
      county_counts <- outbreak_filtered() %>%
        count(County)
      
      if(nrow(county_counts) > 0) {
        top_county <- county_counts %>%
          arrange(desc(n)) %>%
          slice(1) %>%
          pull(County)
        return(top_county)
      } else {
        return(NULL)
      }
    } else {
      return(input$outbreaks_map_shape_click$id)
    }
  })
  
  
  
  # County bar chart - Updated to handle null selection
  
  output$outbreaks_map <- renderLeaflet({
    agg <- outbreak_filtered() %>% count(County)
    sf2 <- left_join(county_geometry(), agg, by = "County")
    
    # Make sure n column exists and has values
    if(!"n" %in% names(sf2)) {
      sf2$n <- 0
    }
    
    # Replace NA values with 0
    sf2$n <- ifelse(is.na(sf2$n), 0, sf2$n)
    
    # Create a blue gradient color palette
    blue_palette <- colorNumeric(
      palette = colorRampPalette(c("#E6F2FF", "#99CCFF", "#3399FF", "#0066CC", "#003366"))(50),
      domain = c(0, max(sf2$n, 1)),
      na.color = "#EEEEEE"
    )
    
    # Create map with improved legend
    leaflet(sf2, width = "100%", height = "100%") %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor    = ~blue_palette(n),
        fillOpacity  = 0.9,
        color        = "#444444",
        weight       = 0.5,
        layerId      = ~County,
        label        = ~paste0(County, ": ", n),
        labelOptions = labelOptions(
          style     = list("font-size" = "16px", "font-weight" = "bold"),
          textsize  = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position  = "bottomright",
        pal       = blue_palette,
        values    = sf2$n,
        title     = "Case Count",
        opacity   = 1.0,
        labFormat = labelFormat(suffix = ""),
        className = "info legend"
      ) %>%
      setView(
        lng = -86.2,
        lat = 39.8,
        zoom = 7
      )
  })
  
  
  
  
  # County bar chart 
  output$outbreaks_countyBar <- renderPlot({
    county <- selected_outbreak_county()
    
    if(is.null(county)) {
      # Show message if no county selected/available
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Select a county on the map", size = 6) +
        theme_void()
    } else {
      # Filter data for selected county
      county_data <- outbreak_filtered() %>%
        filter(County == county) %>%
        count(Serotype) %>%
        arrange(desc(n))
      
      if(nrow(county_data) == 0) {
        # No data for this county
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
            label = paste0("No data available for ", county, " County"), size = 6) +
          theme_void()
      } else {
        # Create the bar chart 
        ggplot(county_data, aes(x = reorder(Serotype, n), y = n, fill = Serotype)) +
          geom_col(show.legend = FALSE) +
          scale_fill_viridis_d(option = "D") +
          coord_flip() +
          labs(
            title = paste0("Serotypes in ", county, " County"),
            x = NULL, 
            y = "Count"
          ) +
          theme_minimal(base_size = 18) + 
          theme(
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16),
            axis.text.y = element_text(size = 16, hjust = 1),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            plot.margin = margin(10, 10, 10, 10)
          )
      }
    }
  }, res = 96) 
  
  
  
  #### Adding for error check #########
  
  observeEvent(input$outbreak, {
    # For debugging
    print(paste("Selected outbreak:", input$outbreak))
    print(paste("Number of records:", nrow(outbreak_filtered())))
  })
  
  
  
  # Data Table
  output$outbreaks_table <- renderDT({
    req(outbreak_filtered())
    
    if(nrow(outbreak_filtered()) == 0) {
      # Return empty data frame with message if no data
      data.frame(Message = "No data available for the selected filter") %>%
        datatable(
          options = list(dom = 't'),
          rownames = FALSE
        )
    } else {
      # Return filtered data
      outbreak_filtered() %>%
        select(Key, Outbreak, Serotype, Collection_Date, State, County) %>%
        mutate(Collection_Date = as.character(Collection_Date)) %>%
        datatable(
          filter = "top",
          rownames = FALSE,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            dom = 'ftipr'
          ),
          style = 'bootstrap',
          class = 'cell-border stripe'
        ) %>%
        formatStyle(columns = 1:6, fontSize = '14px')
    }
  })
  
  
  
  #===============================================
  # Settings tab outputs
  #===============================================
  
  # Data preview
  output$data_preview <- renderDT({
    filtered_data() %>%
      select(Collection_Date, Serotype_wgs, County, State, MLST_ST, Specimen, Sex, Age_Group) %>%
      head(100)
  }, options = list(scrollX = TRUE, pageLength = 10))
  
  # Download handlers
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("salmonella_surveillance_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$download_heatmap <- downloadHandler(
    filename = function() {
      paste0("salmonella_county_heatmap_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      # Identify clinically important serotypes
      clinical_serotypes <- c("Typhi", "Paratyphi A", "Paratyphi B", "Paratyphi C")
      
      # Get count of clinical serotypes in data
      clinical_count <- filtered_data() %>%
        filter(Serotype_wgs %in% clinical_serotypes) %>%
        nrow()
      
      # Get top serotypes for better visualization
      top_serotypes <- filtered_data() %>%
        filter(Serotype_wgs != "I 4:i:-") %>%
        count(Serotype_wgs) %>%
        arrange(desc(n)) %>%
        top_n(10) %>%
        pull(Serotype_wgs)
      
      # Make sure clinical serotypes are included if they exist in the data
      if (clinical_count > 0) {
        present_clinical <- filtered_data() %>%
          filter(Serotype_wgs %in% clinical_serotypes) %>%
          pull(Serotype_wgs) %>%
          unique()
        
        top_serotypes <- unique(c(present_clinical, top_serotypes))
      }
      
      # Get top counties
      top_counties <- filtered_data() %>%
        filter(State == "Indiana", County != "Unknown") %>%
        count(County) %>%
        arrange(desc(n)) %>%
        top_n(15) %>%
        pull(County)
      
      # Create heatmap data
      heatmap_data <- filtered_data() %>%
        filter(Serotype_wgs %in% top_serotypes, 
          County %in% top_counties,
          State == "Indiana") %>%
        count(County, Serotype_wgs) %>%
        complete(County, Serotype_wgs, fill = list(n = 0))
      
      # Create the heatmap 
      p <- ggplot(heatmap_data, aes(x = Serotype_wgs, y = County, fill = n)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "#1a1a4c", high = "#F9Cf16", name = "Count") +
        theme_minimal() +
        labs(
          title = "Detailed Salmonella Serotypes by County",
          x = "Serotype",
          y = ""
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 14, face = "bold"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 11)
        )
      
      # Save the plot
      ggsave(file, plot = p, width = 10, height = 8, units = "in", dpi = 300)
    }
  )
  
  output$download_diversity_analysis <- downloadHandler(
    filename = function() {
      paste0("salmonella_diversity_analysis_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      # Calculate diversity metrics by county
      diversity_data <- filtered_data() %>%
        filter(State == "Indiana", County != "Unknown", Serotype_wgs != "Unknown") %>%
        group_by(County) %>%
        summarize(
          `Total Cases` = n(),
          `Serotype Count` = n_distinct(Serotype_wgs),
          # Shannon Diversity Index calculation
          `Shannon Diversity` = round(-sum((table(Serotype_wgs)/n()) * log(table(Serotype_wgs)/n()), na.rm = TRUE), 3),
          # Evenness calculation
          Evenness = round(`Shannon Diversity`/log(`Serotype Count`), 3)
        ) %>%
        ungroup() %>%
        arrange(desc(`Total Cases`))
      
      write.csv(diversity_data, file, row.names = FALSE)
    }
  )
  
  # Download handlers for Summary Report (PDF)
  output$download_summary_pdf <- downloadHandler(
    filename = function() {
      paste0("salmonella_summary_report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Copy the report file to a temporary directory
      tempReport <- file.path(tempdir(), "summary_report.Rmd")
      file.copy("summary_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        start_date = input$date_range[1],
        end_date = input$date_range[2],
        data = filtered_data(),
        format = "pdf"
      )
      
      # Render the report
      rmarkdown::render(tempReport, output_file = file,
        params = params,
        envir = new.env(parent = globalenv()))
    }
  )
  
  # Download handlers for Summary Report (Word)
  output$download_summary_word <- downloadHandler(
    filename = function() {
      paste0("salmonella_summary_report_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      # Copy the report file to a temporary directory
      tempReport <- file.path(tempdir(), "summary_report.Rmd")
      file.copy("summary_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        start_date = input$date_range[1],
        end_date = input$date_range[2],
        data = filtered_data(),
        format = "docx"
      )
      
      # Render the report
      rmarkdown::render(tempReport, 
        output_format = "word_document",
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv()))
    }
  )
  
  # Download handlers for Full Report (PDF)
  output$download_full_pdf <- downloadHandler(
    filename = function() {
      paste0("salmonella_full_report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Copy the report file to a temporary directory
      tempReport <- file.path(tempdir(), "full_report.Rmd")
      file.copy("full_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        start_date = input$date_range[1],
        end_date = input$date_range[2],
        data = filtered_data(),
        format = "pdf"
      )
      
      # Render the report
      rmarkdown::render(tempReport, output_file = file,
        params = params,
        envir = new.env(parent = globalenv()))
    }
  )
  
  # Download handlers for Full Report (Word)
  output$download_full_word <- downloadHandler(
    filename = function() {
      paste0("salmonella_full_report_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      # Copy the report file to a temporary directory
      tempReport <- file.path(tempdir(), "full_report.Rmd")
      file.copy("full_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        start_date = input$date_range[1],
        end_date = input$date_range[2],
        data = filtered_data(),
        format = "docx"
      )
      
      # Render the report
      rmarkdown::render(tempReport, 
        output_format = "word_document",
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_exec_report <- downloadHandler(
    filename = function() {
      paste0("salmonella_executive_report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Copy the report file to a temporary directory
      tempReport <- file.path(tempdir(), "executive_report.Rmd")
      file.copy("executive_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        start_date = input$date_range[1],
        end_date = input$date_range[2],
        data = filtered_data()
      )
      
      # Render the report
      rmarkdown::render(tempReport, output_file = file,
        params = params,
        envir = new.env(parent = globalenv()))
    }
  )
  
  # Observer to refresh table when page length changes
  observeEvent(input$table_page_length, {
    # This will trigger the datatable to redraw
    session$sendCustomMessage(type = 'datatable-page-length', 
      message = list(id = 'cdc_isolates_table', length = input$table_page_length))
  })
  
  # Define reactive values for zoom state
  tree_zoom <- reactiveValues(x = NULL, y = NULL, zoom_level = 1)
  
  # Reset zoom when filters change
  observeEvent(c(input$cdc_date_range, input$cdc_outbreak_filter, input$cdc_serotype_filter), {
    tree_zoom$x <- NULL
    tree_zoom$y <- NULL
    tree_zoom$zoom_level <- 1
  })
  
  # Reset zoom button handler
  observeEvent(input$reset_tree_zoom, {
    tree_zoom$x <- NULL
    tree_zoom$y <- NULL
    tree_zoom$zoom_level <- 1
  })
  
  # Handle double click for zooming
  observeEvent(input$tree_dblclick, {
    if (tree_zoom$zoom_level == 1) {
      # Zoom in on double click
      tree_zoom$x <- c(input$tree_dblclick$x - input$tree_dblclick$x/4, 
        input$tree_dblclick$x + input$tree_dblclick$x/4)
      tree_zoom$y <- c(input$tree_dblclick$y - input$tree_dblclick$y/4, 
        input$tree_dblclick$y + input$tree_dblclick$y/4)
      tree_zoom$zoom_level <- 2
    } else {
      # Reset zoom on second double click
      tree_zoom$x <- NULL
      tree_zoom$y <- NULL
      tree_zoom$zoom_level <- 1
    }
  })
  
  # Handle brush zooming
  observeEvent(input$tree_brush, {
    if (!is.null(input$tree_brush)) {
      tree_zoom$x <- c(input$tree_brush$xmin, input$tree_brush$xmax)
      tree_zoom$y <- c(input$tree_brush$ymin, input$tree_brush$ymax)
      tree_zoom$zoom_level <- 2
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# rsconnect::deployApp()

rsconnect::setAccountInfo(name='matt-muslu',
                          token='0000',
                          secret='000000')


rsconnect::deployApp(
  appDir = "C:/Users/",
  appFiles = c(
    "app.R",
    "global.R",
    "summary_report.Rmd",
    "salmonella_dashboard.Rmd",
    "heatmap_module.R",
    "onlyoutbreakscode.R",
    "Salmonella_May.csv",
    "Salmonella_Outbreaks_Only.csv",
    "README.Rmd"
  ),
  appName = "may14th"
)
