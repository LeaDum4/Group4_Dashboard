# Social Media Addiction vs Relationships Dashboard
# Complete Shiny Application

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(leaflet)
library(countrycode)
library(RColorBrewer)
library(corrplot)
library(viridis)

# Load and prepare data
load_data <- function() {
  # Read the CSV file
  data <- read.csv("C:/Users/Administrator/Documents/Students Social Media Addiction.csv", 
                   stringsAsFactors = FALSE)
  
  # Convert relevant columns to factors
  data$Gender <- as.factor(data$Gender)
  data$Academic_Level <- as.factor(data$Academic_Level)
  data$Most_Used_Platform <- as.factor(data$Most_Used_Platform)
  data$Relationship_Status <- as.factor(data$Relationship_Status)
  data$Affects_Academic_Performance <- as.factor(data$Affects_Academic_Performance)
  
  # Add manual country coordinates
  country_coords <- data.frame(
    Country = c("Bangladesh", "India", "USA", "UK", "Canada"),
    lat = c(23.685, 20.5937, 37.0902, 55.3781, 56.1304),
    lon = c(90.3563, 78.9629, -95.7129, -3.4360, -106.3468)
  )
  
  # Merge coordinates into dataset
  data <- merge(data, country_coords, by = "Country", all.x = TRUE)
  
  # Fill missing coordinates with 0 (optional)
  data$lat[is.na(data$lat)] <- 0
  data$lon[is.na(data$lon)] <- 0
  
  return(data)
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Social Media Addiction Analysis Dashboard"),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Addiction Analysis", tabName = "addiction", icon = icon("brain")),
      menuItem("Geographic View", tabName = "geographic", icon = icon("globe")),
      menuItem("Relationships", tabName = "relationships", icon = icon("heart")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    ),
    
    # Filters Panel
    hr(),
    h4("Filters", style = "color: white; margin-left: 15px;"),
    
    selectInput("gender_filter", 
                "Gender:",
                choices = c("All", "Male", "Female"),
                selected = "All"),
    
    selectInput("academic_filter", 
                "Academic Level:",
                choices = c("All", "High School", "Undergraduate", "Graduate"),
                selected = "All"),
    
    selectInput("platform_filter", 
                "Most Used Platform:",
                choices = c("All", "Instagram", "TikTok", "YouTube", "Facebook", 
                            "Snapchat", "LinkedIn", "Twitter"),
                selected = "All"),
    
    selectInput("relationship_filter", 
                "Relationship Status:",
                choices = c("All", "Single", "In Relationship", "Complicated"),
                selected = "All"),
    
    sliderInput("usage_range", 
                "Daily Usage Hours:",
                min = 0, max = 10, value = c(0, 10), step = 0.5),
    
    sliderInput("addiction_range", 
                "Addiction Score:",
                min = 0, max = 10, value = c(0, 10), step = 1),
    
    actionButton("reset_filters", "Reset All Filters", 
                 style = "margin: 15px; width: 90%;")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_students"),
                valueBoxOutput("avg_addiction"),
                valueBoxOutput("avg_usage")
              ),
              
              fluidRow(
                box(
                  title = "Daily Usage vs Addiction Score", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("usage_addiction_plot")
                ),
                
                box(
                  title = "Platform Usage Distribution", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("platform_distribution")
                )
              ),
              
              fluidRow(
                box(
                  title = "Mental Health vs Social Media Usage", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("mental_health_plot")
                )
              )
      ),
      
      # Addiction Analysis Tab
      tabItem(tabName = "addiction",
              fluidRow(
                box(
                  title = "Addiction Score by Demographics", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("addiction_demographics")
                ),
                
                box(
                  title = "Sleep Hours vs Addiction", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("sleep_addiction_plot")
                )
              ),
              
              fluidRow(
                box(
                  title = "Academic Performance Impact", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("academic_impact_plot")
                ),
                
                box(
                  title = "Correlation Matrix", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("correlation_matrix")
                )
              )
      ),
      
      # Geographic View Tab
      tabItem(tabName = "geographic",
              fluidRow(
                box(
                  title = "Global Social Media Usage Map", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  height = "600px",
                  leafletOutput("usage_map", height = "550px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Usage by Country", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("country_usage_plot")
                )
              )
      ),
      
      # Relationships Tab
      tabItem(tabName = "relationships",
              fluidRow(
                box(
                  title = "Relationship Status Distribution", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("relationship_distribution")
                ),
                
                box(
                  title = "Conflicts by Relationship Status", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("conflicts_plot")
                )
              ),
              
              fluidRow(
                box(
                  title = "Social Media Impact on Relationships", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("relationship_impact_plot")
                )
              )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Dataset Explorer", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("data_table")
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Load data
  raw_data <- load_data()
  
  # Reactive data based on filters
  filtered_data <- reactive({
    data <- raw_data
    
    if (input$gender_filter != "All") {
      data <- data[data$Gender == input$gender_filter, ]
    }
    
    if (input$academic_filter != "All") {
      data <- data[data$Academic_Level == input$academic_filter, ]
    }
    
    if (input$platform_filter != "All") {
      data <- data[data$Most_Used_Platform == input$platform_filter, ]
    }
    
    if (input$relationship_filter != "All") {
      data <- data[data$Relationship_Status == input$relationship_filter, ]
    }
    
    data <- data[data$Avg_Daily_Usage_Hours >= input$usage_range[1] & 
                   data$Avg_Daily_Usage_Hours <= input$usage_range[2], ]
    
    data <- data[data$Addicted_Score >= input$addiction_range[1] & 
                   data$Addicted_Score <= input$addiction_range[2], ]
    
    return(data)
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "gender_filter", selected = "All")
    updateSelectInput(session, "academic_filter", selected = "All")
    updateSelectInput(session, "platform_filter", selected = "All")
    updateSelectInput(session, "relationship_filter", selected = "All")
    updateSliderInput(session, "usage_range", value = c(0, 10))
    updateSliderInput(session, "addiction_range", value = c(0, 10))
  })
  
  # Value Boxes
  output$total_students <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Total Students",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avg_addiction <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$Addicted_Score, na.rm = TRUE), 1),
      subtitle = "Avg Addiction Score",
      icon = icon("brain"),
      color = "red"
    )
  })
  
  output$avg_usage <- renderValueBox({
    valueBox(
      value = paste(round(mean(filtered_data()$Avg_Daily_Usage_Hours, na.rm = TRUE), 1), "hrs"),
      subtitle = "Avg Daily Usage",
      icon = icon("clock"),
      color = "green"
    )
  })
  
  # Plot 1: Daily Usage vs Addiction Score (Interactive Scatter Plot)
  output$usage_addiction_plot <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data, aes(x = Avg_Daily_Usage_Hours, y = Addicted_Score, 
                          color = Gender, size = Mental_Health_Score)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
      labs(x = "Average Daily Usage (Hours)", 
           y = "Addiction Score",
           title = "Relationship between Daily Usage and Addiction") +
      theme_minimal() +
      scale_color_viridis_d()
    
    ggplotly(p, tooltip = c("x", "y", "colour", "size"))
  })
  
  # Plot 2: Platform Distribution (Interactive Bar Chart)
  output$platform_distribution <- renderPlotly({
    data <- filtered_data()
    
    platform_summary <- data %>%
      group_by(Most_Used_Platform) %>%
      summarise(
        Count = n(),
        Avg_Addiction = round(mean(Addicted_Score), 2),
        Avg_Usage = round(mean(Avg_Daily_Usage_Hours), 2)
      )
    
    p <- ggplot(platform_summary, aes(x = reorder(Most_Used_Platform, Count), 
                                      y = Count, fill = Avg_Addiction)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Platform", y = "Number of Users",
           title = "Most Used Social Media Platforms") +
      theme_minimal() +
      scale_fill_viridis_c(name = "Avg\nAddiction")
    
    ggplotly(p)
  })
  
  # Plot 3: Mental Health vs Usage (Interactive Line Plot)
  output$mental_health_plot <- renderPlotly({
    data <- filtered_data()
    
    mental_summary <- data %>%
      mutate(Usage_Category = cut(Avg_Daily_Usage_Hours, 
                                  breaks = c(0, 2, 4, 6, 8, 10),
                                  labels = c("0-2h", "2-4h", "4-6h", "6-8h", "8-10h"))) %>%
      group_by(Usage_Category, Gender) %>%
      summarise(
        Avg_Mental_Health = mean(Mental_Health_Score, na.rm = TRUE),
        Count = n(),
        .groups = 'drop'
      )
    
    p <- ggplot(mental_summary, aes(x = Usage_Category, y = Avg_Mental_Health, 
                                    color = Gender, group = Gender)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(x = "Daily Usage Categories", 
           y = "Average Mental Health Score",
           title = "Mental Health Score by Usage Categories and Gender") +
      theme_minimal() +
      scale_color_viridis_d()
    
    ggplotly(p)
  })
  
  # Addiction Demographics Plot
  output$addiction_demographics <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data, aes(x = Academic_Level, y = Addicted_Score, fill = Gender)) +
      geom_boxplot(alpha = 0.7) +
      labs(x = "Academic Level", y = "Addiction Score",
           title = "Addiction Score Distribution by Demographics") +
      theme_minimal() +
      scale_fill_viridis_d()
    
    ggplotly(p)
  })
  
  # Sleep vs Addiction Plot
  output$sleep_addiction_plot <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data, aes(x = Sleep_Hours_Per_Night, y = Addicted_Score, 
                          color = Affects_Academic_Performance)) +
      geom_point(size = 2, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Sleep Hours Per Night", y = "Addiction Score",
           title = "Sleep Quality vs Addiction Score") +
      theme_minimal() +
      scale_color_viridis_d(name = "Affects\nAcademics")
    
    ggplotly(p)
  })
  
  # Academic Impact Plot
  output$academic_impact_plot <- renderPlotly({
    data <- filtered_data()
    
    academic_summary <- data %>%
      group_by(Affects_Academic_Performance, Most_Used_Platform) %>%
      summarise(
        Count = n(),
        Avg_Usage = mean(Avg_Daily_Usage_Hours),
        .groups = 'drop'
      )
    
    p <- ggplot(academic_summary, aes(x = Most_Used_Platform, y = Count, 
                                      fill = Affects_Academic_Performance)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(x = "Platform", y = "Number of Students",
           title = "Academic Performance Impact by Platform") +
      theme_minimal() +
      scale_fill_viridis_d(name = "Affects\nAcademics")
    
    ggplotly(p)
  })
  
  # Correlation Matrix
  output$correlation_matrix <- renderPlot({
    data <- filtered_data()
    
    numeric_cols <- data[, c("Age", "Avg_Daily_Usage_Hours", "Sleep_Hours_Per_Night",
                             "Mental_Health_Score", "Conflicts_Over_Social_Media", 
                             "Addicted_Score")]
    
    cor_matrix <- cor(numeric_cols, use = "complete.obs")
    
    corrplot(cor_matrix, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black",
             col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", 
                                      "#77AADD", "#4477AA"))(200))
  })
  
  # Geographic Map
  output$usage_map <- renderLeaflet({
    data <- filtered_data()
    
    country_summary <- data %>%
      group_by(Country, lat, lon) %>%
      summarise(
        Avg_Usage = mean(Avg_Daily_Usage_Hours),
        Avg_Addiction = mean(Addicted_Score),
        Count = n(),
        .groups = 'drop'
      )
    
    leaflet(country_summary) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~sqrt(Avg_Usage) * 3,
        color = ~ifelse(Avg_Addiction > 6, "red", 
                        ifelse(Avg_Addiction > 4, "orange", "green")),
        popup = ~paste("Country:", Country, "<br>",
                       "Avg Usage:", round(Avg_Usage, 2), "hours<br>",
                       "Avg Addiction:", round(Avg_Addiction, 2), "<br>",
                       "Students:", Count),
        fillOpacity = 0.7
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  # Country Usage Plot
  output$country_usage_plot <- renderPlotly({
    data <- filtered_data()
    
    country_summary <- data %>%
      group_by(Country) %>%
      summarise(
        Avg_Usage = mean(Avg_Daily_Usage_Hours),
        Avg_Addiction = mean(Addicted_Score),
        Count = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(Avg_Usage))
    
    p <- ggplot(country_summary, aes(x = reorder(Country, Avg_Usage), 
                                     y = Avg_Usage, fill = Avg_Addiction)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Country", y = "Average Daily Usage (Hours)",
           title = "Social Media Usage by Country") +
      theme_minimal() +
      scale_fill_viridis_c(name = "Avg\nAddiction")
    
    ggplotly(p)
  })
  
  # Relationship Distribution
  output$relationship_distribution <- renderPlotly({
    data <- filtered_data()
    
    relationship_summary <- data %>%
      group_by(Relationship_Status) %>%
      summarise(
        Count = n(),
        Avg_Conflicts = mean(Conflicts_Over_Social_Media),
        .groups = 'drop'
      )
    
    p <- ggplot(relationship_summary, aes(x = Relationship_Status, y = Count, 
                                          fill = Relationship_Status)) +
      geom_bar(stat = "identity") +
      labs(x = "Relationship Status", y = "Number of Students",
           title = "Distribution of Relationship Status") +
      theme_minimal() +
      scale_fill_viridis_d() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Conflicts Plot
  output$conflicts_plot <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data, aes(x = Relationship_Status, y = Conflicts_Over_Social_Media, 
                          fill = Relationship_Status)) +
      geom_boxplot(alpha = 0.7) +
      labs(x = "Relationship Status", y = "Social Media Conflicts",
           title = "Social Media Conflicts by Relationship Status") +
      theme_minimal() +
      scale_fill_viridis_d() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Relationship Impact Plot
  output$relationship_impact_plot <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data, aes(x = Avg_Daily_Usage_Hours, y = Conflicts_Over_Social_Media, 
                          color = Relationship_Status, size = Addicted_Score)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Average Daily Usage (Hours)", 
           y = "Social Media Conflicts",
           title = "Social Media Usage Impact on Relationships") +
      theme_minimal() +
      scale_color_viridis_d(name = "Relationship\nStatus") +
      scale_size_continuous(name = "Addiction\nScore")
    
    ggplotly(p)
  })
  
  # Data Table
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      filtered_data(),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      filter = 'top'
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)