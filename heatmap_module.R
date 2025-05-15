# heatmap_module.R
# Module for generating detailed heatmaps for the Salmonella dashboard

# UI function for heatmap module
heatmapModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 3,
        selectInput(
          ns("heatmap_type"),
          "Select Heatmap Type:",
          choices = c(
            "Serotype by Month" = "serotype_month",
            "MLST Type by Month" = "mlst_month",
            "Serotype by County" = "serotype_county",
            "Age Group by Serotype" = "age_serotype"
          )
        ),
        selectInput(
          ns("color_scale"),
          "Color Scale:",
          choices = c(
            "Viridis (Colorblind-friendly)" = "viridis",
            "Magma" = "magma",
            "Plasma" = "plasma", 
            "Inferno" = "inferno",
            "Blues" = "blues",
            "Reds" = "reds"
          ),
          selected = "viridis"
        ),
        checkboxInput(
          ns("reverse_colors"),
          "Reverse Colors (Dark for Higher Values)",
          value = TRUE
        ),
        downloadButton(ns("download_heatmap"), "Download Heatmap"),
        hr(),
        h4("Filtering Options:"),
        sliderInput(
          ns("min_count"),
          "Minimum Count to Display:",
          min = 0,
          max = 20,
          value = 0,
          step = 1
        ),
        checkboxGroupInput(
          ns("top_items"),
          "Show Only Top Items:",
          choices = c(
            "Top 10" = 10,
            "Top 15" = 15,
            "Top 20" = 20,
            "All" = "all"
          ),
          selected = 10
        )
      ),
      column(
        width = 9,
        plotlyOutput(ns("heatmap_plot"), height = "650px"),
        hr(),
        div(
          style = "font-size: 12px; color: #696969;",
          p("Hover over cells to see exact counts. Darker colors indicate higher values."),
          p("Use the filtering options on the left to customize the display.")
        )
      )
    )
  )
}

# Server function for heatmap module
heatmapModule <- function(input, output, session, data) {
  # Reactive expression to generate the heatmap data
  heatmap_data <- reactive({
    req(data())
    
    filtered_data <- data()
    
    # Different processing based on selected heatmap type
    switch(
      input$heatmap_type,
      
      # Serotype by Month
      serotype_month = {
        # Get top serotypes if filtering is applied
        if (input$top_items != "all") {
          top_n <- as.numeric(input$top_items)
          top_serotypes <- filtered_data %>%
            count(Serotype_wgs) %>%
            arrange(desc(n)) %>%
            head(top_n) %>%
            pull(Serotype_wgs)
          
          filtered_data <- filtered_data %>%
            filter(Serotype_wgs %in% top_serotypes)
        }
        
        # Create the heatmap data
        heatmap_df <- filtered_data %>%
          mutate(Month = floor_date(Collection_Date, "month")) %>%
          count(Month, Serotype_wgs) %>%
          # Filter for minimum count if specified
          filter(n >= input$min_count) %>%
          # Complete the grid with zeros
          complete(Month, Serotype_wgs, fill = list(n = 0))
        
        return(list(
          data = heatmap_df,
          x = "Month",
          y = "Serotype_wgs",
          fill = "n",
          title = "Serotype Distribution by Month",
          x_label = "Month",
          y_label = "Serotype"
        ))
      },
      
      # MLST Type by Month
      mlst_month = {
        # Filter out unknown MLST values
        filtered_data <- filtered_data %>%
          filter(MLST_ST != "Unknown")
        
        # Get top MLST types if filtering is applied
        if (input$top_items != "all") {
          top_n <- as.numeric(input$top_items)
          top_mlst <- filtered_data %>%
            count(MLST_ST) %>%
            arrange(desc(n)) %>%
            head(top_n) %>%
            pull(MLST_ST)
          
          filtered_data <- filtered_data %>%
            filter(MLST_ST %in% top_mlst)
        }
        
        # Create the heatmap data
        heatmap_df <- filtered_data %>%
          mutate(Month = floor_date(Collection_Date, "month")) %>%
          count(Month, MLST_ST) %>%
          filter(n >= input$min_count) %>%
          complete(Month, MLST_ST, fill = list(n = 0))
        
        return(list(
          data = heatmap_df,
          x = "Month",
          y = "MLST_ST",
          fill = "n",
          title = "MLST Type Distribution by Month",
          x_label = "Month",
          y_label = "MLST Sequence Type"
        ))
      },
      
      # Serotype by County
      serotype_county = {
        # Filter for Indiana counties and known values
        filtered_data <- filtered_data %>%
          filter(State == "Indiana", County != "Unknown")
        
        # Get top serotypes if filtering is applied
        if (input$top_items != "all") {
          top_n <- as.numeric(input$top_items)
          top_serotypes <- filtered_data %>%
            count(Serotype_wgs) %>%
            arrange(desc(n)) %>%
            head(top_n) %>%
            pull(Serotype_wgs)
          
          filtered_data <- filtered_data %>%
            filter(Serotype_wgs %in% top_serotypes)
        }
        
        # Create the heatmap data
        heatmap_df <- filtered_data %>%
          count(County, Serotype_wgs) %>%
          filter(n >= input$min_count) %>%
          complete(County, Serotype_wgs, fill = list(n = 0))
        
        return(list(
          data = heatmap_df,
          x = "Serotype_wgs",
          y = "County",
          fill = "n",
          title = "Serotype Distribution by County",
          x_label = "Serotype",
          y_label = "County"
        ))
      },
      
      # Age Group by Serotype
      age_serotype = {
        # Filter for known age groups
        filtered_data <- filtered_data %>%
          filter(Age_Group != "Unknown")
        
        # Get top serotypes if filtering is applied
        if (input$top_items != "all") {
          top_n <- as.numeric(input$top_items)
          top_serotypes <- filtered_data %>%
            count(Serotype_wgs) %>%
            arrange(desc(n)) %>%
            head(top_n) %>%
            pull(Serotype_wgs)
          
          filtered_data <- filtered_data %>%
            filter(Serotype_wgs %in% top_serotypes)
        }
        
        # Create the heatmap data
        heatmap_df <- filtered_data %>%
          count(Age_Group, Serotype_wgs) %>%
          filter(n >= input$min_count) %>%
          complete(Age_Group, Serotype_wgs, fill = list(n = 0))
        
        # Ensure age groups are in the correct order
        age_order <- c("< 1 year", "1-5 years", "6-18 years", "19-65 years", "> 65 years")
        heatmap_df$Age_Group <- factor(heatmap_df$Age_Group, levels = age_order)
        
        return(list(
          data = heatmap_df,
          x = "Serotype_wgs",
          y = "Age_Group",
          fill = "n",
          title = "Age Group Distribution by Serotype",
          x_label = "Serotype",
          y_label = "Age Group"
        ))
      }
    )
  })
  
  # Generate the heatmap plot
  output$heatmap_plot <- renderPlotly({
    req(heatmap_data())
    
    hm_data <- heatmap_data()
    
    # Define color scale based on input
    color_scale <- switch(
      input$color_scale,
      viridis = "viridis",
      magma = "magma",
      plasma = "plasma",
      inferno = "inferno",
      blues = "blues",
      reds = "reds"
    )
    
    # Set direction based on checkbox
    direction <- if(input$reverse_colors) -1 else 1
    
    # Create the plot
    p <- ggplot(hm_data$data, 
      aes_string(x = hm_data$x, y = hm_data$y, fill = hm_data$fill)) +
      geom_tile(color = "white") +
      {
        if(color_scale %in% c("viridis", "magma", "plasma", "inferno")) {
          scale_fill_viridis_c(option = substr(color_scale, 1, 1), direction = direction, name = "Cases")
        } else {
          scale_fill_distiller(palette = color_scale, direction = direction, name = "Cases")
        }
      } +
      theme_minimal() +
      labs(
        title = hm_data$title,
        x = hm_data$x_label,
        y = hm_data$y_label
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
    
    # Convert to plotly for interactivity
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(margin = list(l = 100, r = 50, b = 100, t = 50))
  })
  
  # Download handler for the heatmap
  output$download_heatmap <- downloadHandler(
    filename = function() {
      paste0("salmonella_heatmap_", input$heatmap_type, "_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      req(heatmap_data())
      
      hm_data <- heatmap_data()
      
      # Define color scale based on input
      color_scale <- switch(
        input$color_scale,
        viridis = "viridis",
        magma = "magma",
        plasma = "plasma",
        inferno = "inferno",
        blues = "blues",
        reds = "reds"
      )
      
      # Set direction based on checkbox
      direction <- if(input$reverse_colors) -1 else 1
      
      # Create the plot for saving
      p <- ggplot(hm_data$data, 
        aes_string(x = hm_data$x, y = hm_data$y, fill = hm_data$fill)) +
        geom_tile(color = "white") +
        {
          if(color_scale %in% c("viridis", "magma", "plasma", "inferno")) {
            scale_fill_viridis_c(option = substr(color_scale, 1, 1), direction = direction, name = "Cases")
          } else {
            scale_fill_distiller(palette = color_scale, direction = direction, name = "Cases")
          }
        } +
        theme_minimal() +
        labs(
          title = hm_data$title,
          x = hm_data$x_label,
          y = hm_data$y_label
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 14, face = "bold"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)
        )
      
      # Save the plot
      ggsave(file, plot = p, width = 10, height = 8, units = "in", dpi = 300)
    }
  )
}