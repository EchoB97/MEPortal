
#Note:
# 1.	SDGs and UHC indicators page
# -	compiled the dataset (SDGs and UHC data summary.csv) from GHO, FHSIS, PHS, and UHC/NOH, which I organized/categorized by Programme/Disease Programme
# -	🔍 Users can explore data in three ways:
#   •	By SDG tab 
# •	By UHC tab
# •	By Programme/Disease Programme tab 
# 
# Notes about the dataset:
# •	Latest data1 and Link1 = SDGs - GHO, SDGs - WHO Global Antimicrobial Resistance and Use Surveillance System (GLASS)
# •	Latest data2 and Link2 = FHSIS
# •	Latest data3 and Link3 = Philippine Health Statistics (PHS)
# •	Latest data4 = UHC/National Objectives for Health (no link); not solely numerical in nature as some have narrations/descriptions
# 
# 
# 2.	Programme Monitoring page
# -	interactive map where users can click on a province or area to view relevant program data
# 
# 
# 3.	TWG Portal page
# -	Cleaned dataset   TWG summary.xlsx 
# -	🔍 Users can search data by 
# •	Name (of TWG)
# •	Programme/Disease Programme (I added this, in line with the categories for the SDGs and UHC indicators page)
# •	Keyword/s (can add more options like by Date Established)




pacman::p_load(
  janitor,
  here,
  rio,
  shiny, shinydashboard,
  DT,dplyr,plotly,readr,
  ggplot2,
  sf,
  leaflet,
  stringr,
  shinycssloaders)





# 1. portal without the map, with health legislations page -----------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "M & E Portal"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table")),
      menuItem("Programme Analysis", tabName = "programme", icon = icon("chart-bar")),
      menuItem("SDG Indicators", tabName = "sdg", icon = icon("globe")),
      menuItem("UHC Indicators", tabName = "uhc", icon = icon("heart")),
      menuItem("Global Adult Tobacco Survey (GATS)", tabName = "gats", icon = icon("file-pdf")),
      menuItem("Health Legislation", tabName = "legislation", icon = icon("gavel")),
      menuItem("Antimicrobial Resistance (AMR)", tabName = "glass_amr_2025", icon = icon("microscope"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 8px;
        }
        .box-header {
          font-weight: bold;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_indicators"),
          valueBoxOutput("uhc_indicators")
        ),
        fluidRow(
          box(
            title = "Legend", status = "warning", solidHeader = TRUE,
            width = 12,
            div(class = "legend-box",
                div(class = "legend-item", strong("FHSIS"), " – Field Health Services Information System"),
                div(class = "legend-item", strong("PHS"), " – Philippine Health Statistics"),
                div(class = "legend-item", strong("NOH"), " – National Objectives for Health"),
                div(class = "legend-item", strong("UHC"), " – Universal Health Care")
            )
          )
        ),
        fluidRow(
          box(
            title = "Number of Indicators per Source", status = "primary", solidHeader = TRUE,
            width = 6, height = 400,
            withSpinner(plotlyOutput("source_distribution"))
          ),
          box(
            title = "Number of Indicators per Programme", status = "info", solidHeader = TRUE,
            width = 6, height = 400,
            withSpinner(plotlyOutput("programme_distribution"))
          )
        ),
        fluidRow(
          box(
            title = "Recent Data Summary", status = "success", solidHeader = TRUE,
            width = 12,
            withSpinner(DT::dataTableOutput("summary_table"))
          )
        )
      ),
      
      # Data Explorer Tab
      tabItem(
        tabName = "explorer",
        fluidRow(
          box(
            title = "Filters", status = "primary", solidHeader = TRUE,
            width = 3,
            selectInput("filter_programme", "Programme/Disease:",
                        choices = NULL, multiple = TRUE),
            selectInput("filter_sdg", "SDG Target:",
                        choices = NULL, multiple = TRUE),
            selectInput("filter_year", "Year:",
                        choices = NULL, multiple = TRUE),
            checkboxInput("filter_uhc", "UHC Indicators Only", FALSE),
            actionButton("reset_filters", "Reset Filters", class = "btn-warning")
          ),
          box(
            title = "Data Table", status = "primary", solidHeader = TRUE,
            width = 9,
            withSpinner(DT::dataTableOutput("main_table"))
          )
        )
      ),
      
      # Programme Analysis Tab
      tabItem(
        tabName = "programme",
        fluidRow(
          box(
            title = "Programme Selection", status = "primary", solidHeader = TRUE,
            width = 3,
            selectInput("selected_programme", "Select Programme:", choices = NULL),
            radioButtons("data_source", "Data Source:",
                         choices = list(
                           "GHO" = "Latest.data1",
                           "FHSIS" = "Latest.data2", 
                           "PHS" = "Latest.data3",
                           "UHC/NOH" = "Latest.data4"
                         ))
          ),
          box(
            title = "Programme Indicators", status = "info", solidHeader = TRUE,
            width = 9,
            withSpinner(plotlyOutput("programme_chart")),
            br(),
            withSpinner(DT::dataTableOutput("programme_table"))
          )
        )
      ),
      
      # SDG Indicators Tab
      tabItem(
        tabName = "sdg",
        fluidRow(
          box(
            title = "SDG Target Analysis", status = "primary", solidHeader = TRUE,
            width = 4,
            selectInput("sdg_target", "SDG Target:", choices = NULL),
            br(),
            h4("SDG Target Summary:"),
            verbatimTextOutput("sdg_summary")
          ),
          box(
            title = "SDG Indicators Overview", status = "info", solidHeader = TRUE,
            width = 8,
            withSpinner(plotlyOutput("sdg_chart")),
            br(),
            withSpinner(DT::dataTableOutput("sdg_table"))
          )
        )
      ),
      
      # UHC Indicators Tab
      tabItem(
        tabName = "uhc",
        fluidRow(
          box(
            title = "UHC Indicators Analysis", status = "success", solidHeader = TRUE,
            width = 12,
            withSpinner(plotlyOutput("uhc_chart")),
            br(),
            withSpinner(DT::dataTableOutput("uhc_table"))
          )
        )
      ),
      
      # GATS Tab
      tabItem(
        tabName = "gats",
        fluidRow(
          box(
            title = "Global Adult Tobacco Survey (GATS) Factsheets",
            status = "danger", solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("Zamboanga", 
                       p("Download the factsheet:"),
                       downloadButton("download_zamboanga", "Download PDF")),
              tabPanel("Quezon", 
                       p("Download the factsheet:"),
                       downloadButton("download_quezon", "Download PDF")),
              tabPanel("Philippines Comparison", 
                       p("Download the factsheet:"),
                       downloadButton("download_philcomp", "Download PDF")),
              tabPanel("Philippines Factsheet", 
                       p("Download the factsheet:"),
                       downloadButton("download_philfacts", "Download PDF")),
              tabPanel("General Santos", 
                       p("Download the factsheet:"),
                       downloadButton("download_gensan", "Download PDF")),
              tabPanel("Cebu", 
                       p("Download the factsheet:"),
                       downloadButton("download_cebu", "Download PDF")),
              tabPanel("Baguio", 
                       p("Download the factsheet:"),
                       downloadButton("download_baguio", "Download PDF"))
            )
          )
        )
      ),
      tabItem(
        tabName = "legislation",
        
        # PDP Priority Legislations
        fluidRow(
          box(
            title = "Health-related Legislations", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            p(strong("Priority legislation identified in the PDP Mid-term report subchapter on health:"))
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = tagList(icon("flask"), " Creation of the Virology & Vaccine Institute of the Philippines"),
              status = "info",
              solidHeader = FALSE,
              width = NULL
            )
          ),
          column(
            width = 4,
            box(
              title = tagList(icon("shield-virus"), " Creation of the Philippine Center for Disease Control & Prevention (CDC)"),
              status = "info",
              solidHeader = FALSE,
              width = NULL
            )
          ),
          column(
            width = 4,
            box(
              title = tagList(icon("users"), " Magna Carta for BHWs"),
              status = "info",
              solidHeader = FALSE,
              width = NULL
            )
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = tagList(icon("laptop-medical"), " Telemedicine Law"),
              status = "info",
              solidHeader = FALSE,
              width = NULL,
              p("Telemedicine Law/Act")
            )
          ),
          column(
            width = 4,
            box(
              title = tagList(icon("user-md"), " Establishment of Medical Reserve Corps (MRC)"),
              status = "info",
              solidHeader = FALSE,
              width = NULL
            )
          ),
          column(
            width = 4,
            box(
              title = tagList(icon("hospital"), " Health Facilities & Services Regulation Act"),
              status = "info",
              solidHeader = FALSE,
              width = NULL
            )
          ),
          
          column(
            width = 6,
            box(
              title = tagList(
                icon("file-medical")," RA 11332 Amendment"),
              status = "info",
              solidHeader = FALSE,
              width = NULL,
              p("Amendment on RA11332 (law on notifiable diseases)")
            )
          ),
          column(
            width = 6,
            box(
              title = tagList(
                icon("paw")," Amendment of the OP-AO on Philippine Interagency Committee on Zoonoses"),
              status = "info",
              solidHeader = FALSE,
              width = NULL
            )
          )
        ),
        
        # International Agreements & Amendments
        fluidRow(
          box(
            title = tagList(icon("globe"), " International Agreements & Key Amendments"),
            status = "warning",
            solidHeader = TRUE,
            width = 12
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            box(
              title = "Pandemic Agreement",
              status = "warning",
              solidHeader = FALSE,
              width = NULL,
              p("EO or Senate approval of the Pandemic Agreement")
            )
          ),
        ),
        
        fluidRow(
          column(
            width = 6,
            box(
              title = "Tobacco Protocol",
              status = "warning",
              solidHeader = FALSE,
              width = NULL,
              p("Accession to the WHO FCTC Protocol to Eliminate Illicit Trade in Tobacco Products")
            )
          )
        ),
        
        # DOH Administrative Orders
        fluidRow(
          box(
            title = tagList(icon("file-alt"), " DOH Administrative Orders & Revisions"),
            status = "success",
            solidHeader = TRUE,
            width = 12
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "EREID Program",
              status = "success",
              solidHeader = FALSE,
              width = NULL,
              p("Revise DOH-AO on roles and responsibilities of DOH-Emerging & Re-emerging Infectious Disease (EREID) program")
            )
          ),
          column(
            width = 4,
            box(
              title = "FWBD Program",
              status = "success",
              solidHeader = FALSE,
              width = NULL,
              p("Revise DOH-AO on roles and responsibilities of Food & Waterborne Disease (FWBD) program")
            )
          ),
          column(
            width = 4,
            box(
              title = "RITM Mandate",
              status = "success",
              solidHeader = FALSE,
              width = NULL,
              p("Revise OP-EO on the mandate of Research Institute for Tropical Medicine (RITM)")
            )
          )
        ),
        
        # Drug Policy & Environmental Health
        fluidRow(
          box(
            title = tagList(icon("pills"), " Drug Policy"),
            status = "danger",
            solidHeader = TRUE,
            width = 12
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            box(
              title = "Amendments of the Comprehensive Dangerous Drugs Act (RA 9165)",
              status = "danger",
              solidHeader = FALSE,
              width = NULL,
              p("WHO worked with DDB and UNODC")
            )
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("leaf"), " Environmental Health"),
            status = "success",
            solidHeader = TRUE,
            width = 12
          ),
          column(
            width = 6,
            box(
              title = "Environmental Health Act",
              status = "success",
              solidHeader = FALSE,
              width = NULL,
              p("Draft Environmental Health Act as revision of the old Sanitation Code (1975)")
            )
          )
        ),
        
        # Healthy Communities & Nutrition
        fluidRow(
          box(
            title = tagList(icon("heart"), " Healthy Communities & Nutrition"),
            status = "primary",
            solidHeader = TRUE,
            width = 12
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "Walkable & Bikeable Communities Acts",
              status = "primary",
              solidHeader = FALSE,
              width = NULL,
              p(em("SBN 24, SBN 528, SBN 883"))
            )
          ),
          column(
            width = 4,
            box(
              title = "Healthy Food Environment",
              status = "primary",
              solidHeader = FALSE,
              width = NULL,
              p("Instituting a Legal Framework for a Healthy Food Environment")
            )
          ),
          column(
            width = 4,
            box(
              title = "Adolescent Pregnancy Prevention",
              status = "primary",
              solidHeader = FALSE,
              width = NULL,
              p("National Policy in Preventing Adolescent Pregnancy"),
              p(em("House Bill 3988"))
            )
          )
        ),
        
        # Maternal & Child Health
        fluidRow(
          box(
            title = tagList(icon("baby"), " Maternal & Child Health"),
            status = "info",
            solidHeader = TRUE,
            width = 12
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            box(
              title = "Breast Milk Banking Act",
              status = "info",
              solidHeader = FALSE,
              width = NULL,
              p(em("Senate Bill 792"))
            )
          ),
          column(
            width = 6,
            box(
              title = "Infant-Friendly Facilities Bills",
              status = "info",
              solidHeader = FALSE,
              width = NULL,
              p(em("HB 1950, 4574, 4799, 5459, 9403, 9678, 10072"))
            )
          )
        ),
        
        # Digital Health & Data
        fluidRow(
          box(
            title = tagList(icon("database"), " Digital Health & Health Information Systems"),
            status = "success",
            solidHeader = TRUE,
            width = 12
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              title = "Konektadong Pinoy Act",
              status = "success",
              solidHeader = FALSE,
              width = NULL,
              p("Republic Act No. 12234")
            )
          ),
          column(
            width = 4,
            box(
              title = "Health Passport System Act",
              status = "success",
              solidHeader = FALSE,
              width = NULL,
              p(em("Senate Bill No. 110"))
            )
          ),
          column(
            width = 4,
            box(
              title = "Philippine Civil Registration and Vital Statistics Act",
              status = "success",
              solidHeader = FALSE,
              width = NULL,
              p(em("Senate Bill No. 2914"))
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            box(
              title = "One Electronic Medical Records Act",
              status = "success",
              solidHeader = FALSE,
              width = NULL,
              p(em("Senate Bill No. 139"))
            )
          ),
          column(
            width = 6,
            box(
              title = "Philippine Health Card Act of 2025",
              status = "success",
              solidHeader = FALSE,
              width = NULL,
              p(em("Senate Bill No. 2983"))
            )
          )
        ),
        
        # Senior Citizens & Vulnerable Populations
        fluidRow(
          box(
            title = tagList(icon("user-shield"), " Senior Citizens & Vulnerable Populations"),
            status = "warning",
            solidHeader = TRUE,
            width = 12
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            box(
              title = "Act Providing Monthly Maintenance Medication Support for Senior Citizens",
              status = "warning",
              solidHeader = FALSE,
              width = NULL,
              p(em("SBN 164"))
            )
          ),
          column(
            width = 6,
            box(
              title = "Legislative initiatives related to PRSEAH",
              status = "warning",
              solidHeader = FALSE,
              width = NULL,
              p(" - amendments to RA 11313 (Safe Spaces Act)"),
              p(em("House Bills: 768, 1047, 3118, 4119"))
            )
          )
        )
        
      ),
      
      tabItem(
        tabName = "glass_amr_2025",
        fluidRow(
          box(
            title = "2025 WHO Global Antimicrobial Resistance and Use Surveillance System (GLASS) report",
            status = "primary", solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("Full Report", 
                       p("Download the full GLASS-AMR 2025 report (published 13 October 2025):"),
                       tags$a(href = "https://iris.who.int/items/13c8bb1a-6923-480f-adc6-ba9f9429730c", 
                              "Download Full Report (PDF)", 
                              target = "_blank", 
                              class = "btn btn-primary")),
              tabPanel("Summary", 
                       p("Download the summary of the GLASS-AMR 2025 report:"),
                       tags$a(href = "https://iris.who.int/handle/10665/383069", 
                              "Download Summary (PDF)", 
                              target = "_blank", 
                              class = "btn btn-primary"))
            )
          )
        )
      )
    )
  )
)







# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive value to store the data - IMPROVED data cleaning
  data <- reactive({
    # Replace with your CSV file path
    csv_file <- "MEPortal/SDGs and UHC data summary.csv"
    
    # Check if file exists
    if (!file.exists(csv_file)) {
      showNotification("CSV file not found. Please ensure 'SDGs and UHC data summary.csv' is in the working directory.", 
                       type = "error", duration = 10)
      return(data.frame())
    }
    
    tryCatch({
      # Read and clean the data
      df <- read_csv(csv_file, locale = locale(encoding = "UTF-8"))
      
      # Clean column names
      colnames(df) <- gsub("[^A-Za-z0-9_]", ".", colnames(df))
      colnames(df) <- gsub("\\.+", ".", colnames(df))
      colnames(df) <- gsub("^\\.|\\.$", "", colnames(df))
      
      # Convert Year to numeric if it's not already
      if ("Year" %in% colnames(df)) {
        df$Year <- as.numeric(as.character(df$Year))
      }
      
      # Clean up the Latest.data columns - attempt to convert to numeric where possible
      data_cols <- c("Latest.data1", "Latest.data2", "Latest.data3", "Latest.data4")
      for (col in data_cols) {
        if (col %in% colnames(df)) {
          # First, clean the data by removing any non-numeric characters except decimal points and minus signs
          df[[col]] <- gsub("[^0-9.-]", "", as.character(df[[col]]))
          # Replace empty strings with NA
          df[[col]][df[[col]] == "" | df[[col]] == "NA"] <- NA
        }
      }
      
      # Handle missing values
      df[df == "" | df == "NA"] <- NA
      
      # Ensure is_UHC is logical
      if ("is_UHC" %in% colnames(df)) {
        df$is_UHC <- as.logical(df$is_UHC)
      }
      
      return(df)
      
    }, error = function(e) {
      showNotification(paste("Error reading CSV file:", e$message), 
                       type = "error", duration = 10)
      return(data.frame())
    })
  })
  
  # Update filter choices
  observe({
    df <- data()
    if (nrow(df) > 0) {
      # Update programme choices
      programmes <- sort(unique(df$Programme.Disease.Programme[!is.na(df$Programme.Disease.Programme)]))
      updateSelectInput(session, "filter_programme", choices = programmes)
      updateSelectInput(session, "selected_programme", choices = programmes, selected = programmes[1])
      
      # Update SDG choices
      sdg_targets <- sort(unique(df$SDG_Target[!is.na(df$SDG_Target)]))
      updateSelectInput(session, "filter_sdg", choices = sdg_targets)
      updateSelectInput(session, "sdg_target", choices = sdg_targets, selected = sdg_targets[1])
      
      # Update year choices
      years <- sort(unique(df$Year[!is.na(df$Year)]), decreasing = TRUE)
      updateSelectInput(session, "filter_year", choices = years)
    }
  })
  
  # Filtered data for explorer
  filtered_data <- reactive({
    df <- data()
    if (nrow(df) == 0) return(df)
    
    # Apply filters
    if (!is.null(input$filter_programme) && length(input$filter_programme) > 0) {
      df <- df[df$Programme.Disease.Programme %in% input$filter_programme, ]
    }
    
    if (!is.null(input$filter_sdg) && length(input$filter_sdg) > 0) {
      df <- df[df$SDG_Target %in% input$filter_sdg, ]
    }
    
    if (!is.null(input$filter_year) && length(input$filter_year) > 0) {
      df <- df[df$Year %in% input$filter_year, ]
    }
    
    if (input$filter_uhc) {
      df <- df[df$is_UHC == TRUE | !is.na(df$Latest.data4), ]
    }
    
    return(df)
  })
  
  # Value boxes
  output$total_indicators <- renderValueBox({
    valueBox(
      value = nrow(data()),
      subtitle = "Total Indicators",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$uhc_indicators <- renderValueBox({
    df <- data()
    uhc_count <- sum(df$is_UHC == TRUE, na.rm = TRUE)
    valueBox(
      value = uhc_count,
      subtitle = "UHC Indicators",
      icon = icon("heart"),
      color = "red"
    )
  })
  
  # Charts - Source distribution
  output$source_distribution <- renderPlotly({
    df <- data()
    if (nrow(df) == 0) return(NULL)
    
    source_counts <- data.frame(
      Source = c("FHSIS", "PHS", "GHO", "UHC/NOH"),
      Count = c(
        sum(!is.na(df$Latest.data2)),
        sum(!is.na(df$Latest.data3)),
        sum(!is.na(df$Latest.data1)),
        sum(!is.na(df$Latest.data4))
      )
    )
    
    p <- plot_ly(source_counts, x = ~Source, y = ~Count, type = 'bar',
                 marker = list(color = c('#e74c3c', '#2ecc71', '#3498db', '#f39c12'))) %>%
      layout(title = "Number of Indicators per Source",
             xaxis = list(title = "Data Source"),
             yaxis = list(title = "Number of Indicators"))
    p
  })
  
  # Programme distribution chart
  output$programme_distribution <- renderPlotly({
    df <- data()
    if (nrow(df) == 0) return(NULL)
    
    prog_counts <- df %>%
      count(Programme.Disease.Programme) %>%
      arrange(desc(n)) %>%
      head(10)
    
    p <- plot_ly(prog_counts, x = ~Programme.Disease.Programme, y = ~n, 
                 type = 'bar',
                 marker = list(color = '#9b59b6')) %>%
      layout(title = "Top 10 Programmes by Indicator Count",
             xaxis = list(title = "Programme"),
             yaxis = list(title = "Number of Indicators"),
             margin = list(b = 150))
    p
  })
  
  # Tables
  output$main_table <- DT::renderDataTable({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    # Remove is_UHC column from display
    display_df <- df %>% select(-is_UHC)
    
    # Rename Latest data columns
    names(display_df)[names(display_df) == "Latest.data1"] <- "Latest data (GHO)"
    names(display_df)[names(display_df) == "Latest.data2"] <- "Latest data (FHSIS)"
    names(display_df)[names(display_df) == "Latest.data3"] <- "Latest data (PHS)"
    names(display_df)[names(display_df) == "Latest.data4"] <- "Latest data (UHC/NOH)"
    
    DT::datatable(display_df, 
                  options = list(
                    scrollX = TRUE,
                    pageLength = 15,
                    searchHighlight = TRUE
                  ),
                  filter = 'top',
                  rownames = FALSE) %>%
      formatStyle(columns = colnames(display_df), fontSize = '12px')
  })
  
  output$summary_table <- DT::renderDataTable({
    df <- data()
    if (nrow(df) == 0) return(NULL)
    
    summary_df <- df %>%
      select(Programme.Disease.Programme, Indicator, Year, Latest.data1, Latest.data2, Latest.data3, Latest.data4) %>%
      head(10)
    
    # Rename Latest data columns
    colnames(summary_df)[colnames(summary_df) == "Latest.data1"] <- "GHO"
    colnames(summary_df)[colnames(summary_df) == "Latest.data2"] <- "FHSIS"
    colnames(summary_df)[colnames(summary_df) == "Latest.data3"] <- "PHS"
    colnames(summary_df)[colnames(summary_df) == "Latest.data4"] <- "UHC/NOH"
    
    DT::datatable(summary_df, 
                  options = list(
                    pageLength = 5,
                    dom = 't'
                  ),
                  rownames = FALSE)
  })
  
  # Programme-specific outputs - FIXED data type handling
  output$programme_chart <- renderPlotly({
    df <- data()
    if (nrow(df) == 0 || is.null(input$selected_programme)) return(NULL)
    
    prog_data <- df[df$Programme.Disease.Programme == input$selected_programme, ]
    data_col <- input$data_source
    
    if (nrow(prog_data) == 0) return(NULL)
    
    # Ensure data column exists and filter out NAs
    if (!data_col %in% colnames(prog_data)) {
      return(plot_ly() %>% add_annotations(text = "Selected data source not available", 
                                           showarrow = FALSE))
    }
    
    prog_data_clean <- prog_data[!is.na(prog_data[[data_col]]) & prog_data[[data_col]] != "", ]
    
    if (nrow(prog_data_clean) == 0) {
      return(
        plot_ly() %>% 
          add_annotations(
            text = "No data available for selected source", 
            xref = "paper", yref = "paper", 
            x = 0.5, y = 0.5,  # center
            showarrow = FALSE, font = list(size = 14)
          ) %>%
          layout(
            xaxis = list(showticklabels = FALSE, showline = FALSE, zeroline = FALSE, showgrid = FALSE),
            yaxis = list(showticklabels = FALSE, showline = FALSE, zeroline = FALSE, showgrid = FALSE)
          )
      )
    }
    
    
    # FIXED: Convert to numeric and handle non-numeric values
    tryCatch({
      # Convert the data column to numeric
      numeric_data <- as.numeric(as.character(prog_data_clean[[data_col]]))
      
      # Remove rows where conversion failed (resulted in NA)
      valid_indices <- !is.na(numeric_data)
      
      if (sum(valid_indices) == 0) {
        return(plot_ly() %>% add_annotations(text = "No numeric data available for selected source", 
                                             showarrow = FALSE))
      }
      
      # Filter the dataframe to only include rows with valid numeric data
      prog_data_clean <- prog_data_clean[valid_indices, ]
      numeric_data <- numeric_data[valid_indices]
      
      # Add the numeric data back to the dataframe for plotting
      prog_data_clean$numeric_value <- numeric_data
      
      # Calculate y-axis range starting from 0
      max_val <- max(numeric_data, na.rm = TRUE)
      min_val <- min(numeric_data, na.rm = TRUE)
      
      # Handle edge cases
      if (is.infinite(max_val) || is.infinite(min_val) || is.na(max_val) || is.na(min_val)) {
        return(plot_ly() %>% add_annotations(text = "Invalid data values", 
                                             showarrow = FALSE))
      }
      
      y_range <- c(0, max_val * 1.1)
      
      p <- plot_ly(prog_data_clean, x = ~Year, y = ~numeric_value,
                   text = ~Indicator, type = 'scatter', mode = 'markers+lines') %>%
        layout(title = paste("Trend for", input$selected_programme),
               xaxis = list(title = "Year", 
                            dtick = 1,
                            tick0 = min(prog_data_clean$Year, na.rm = TRUE)),
               yaxis = list(title = "Value", range = y_range))
      return(p)
      
    }, error = function(e) {
      return(plot_ly() %>% add_annotations(text = paste("Error processing data:", e$message), 
                                           showarrow = FALSE))
    })
  })
  
  output$programme_table <- DT::renderDataTable({
    df <- data()
    if (nrow(df) == 0 || is.null(input$selected_programme)) return(NULL)
    
    prog_data <- df[df$Programme.Disease.Programme == input$selected_programme, ]
    
    # Remove is_UHC column from display
    display_prog_data <- prog_data %>% select(-is_UHC)
    
    # Rename Latest data columns to match Data Explorer
    names(display_prog_data)[names(display_prog_data) == "Latest.data1"] <- "Latest data (GHO)"
    names(display_prog_data)[names(display_prog_data) == "Latest.data2"] <- "Latest data (FHSIS)"
    names(display_prog_data)[names(display_prog_data) == "Latest.data3"] <- "Latest data (PHS)"
    names(display_prog_data)[names(display_prog_data) == "Latest.data4"] <- "Latest data (UHC/NOH)"
    
    DT::datatable(display_prog_data, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # SDG outputs - FIXED data type handling
  output$sdg_chart <- renderPlotly({
    df <- data()
    if (nrow(df) == 0 || is.null(input$sdg_target)) return(NULL)
    
    sdg_data <- df[df$SDG_Target == input$sdg_target, ]
    
    if (nrow(sdg_data) == 0) return(NULL)
    
    # Remove rows with NA values for Latest.data1
    sdg_data_clean <- sdg_data[!is.na(sdg_data$Latest.data1) & sdg_data$Latest.data1 != "", ]
    
    if (nrow(sdg_data_clean) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
    }
    
    tryCatch({
      # Convert to numeric and handle non-numeric values
      numeric_data <- as.numeric(as.character(sdg_data_clean$Latest.data1))
      valid_indices <- !is.na(numeric_data)
      
      if (sum(valid_indices) == 0) {
        return(plot_ly() %>% add_annotations(text = "No numeric data available", 
                                             showarrow = FALSE))
      }
      
      sdg_data_clean <- sdg_data_clean[valid_indices, ]
      sdg_data_clean$numeric_value <- numeric_data[valid_indices]
      
      max_val <- max(sdg_data_clean$numeric_value, na.rm = TRUE)
      y_range <- c(0, max_val * 1.1)
      
      p <- plot_ly(sdg_data_clean, x = ~Indicator, y = ~numeric_value, type = 'bar') %>%
        layout(title = paste("SDG Target:", input$sdg_target),
               xaxis = list(title = "Indicator"),
               yaxis = list(title = "Value", range = y_range),
               margin = list(b = 150))
      return(p)
      
    }, error = function(e) {
      return(plot_ly() %>% add_annotations(text = paste("Error processing data:", e$message), 
                                           showarrow = FALSE))
    })
  })
  
  output$sdg_table <- DT::renderDataTable({
    df <- data()
    if (nrow(df) == 0 || is.null(input$sdg_target)) return(NULL)
    
    sdg_data <- df[df$SDG_Target == input$sdg_target, ]
    
    # Remove is_UHC column from display
    display_sdg_data <- sdg_data %>% select(-is_UHC)
    
    # Rename Latest data columns
    names(display_sdg_data)[names(display_sdg_data) == "Latest.data1"] <- "Latest data (GHO)"
    names(display_sdg_data)[names(display_sdg_data) == "Latest.data2"] <- "Latest data (FHSIS)"
    names(display_sdg_data)[names(display_sdg_data) == "Latest.data3"] <- "Latest data (PHS)"
    names(display_sdg_data)[names(display_sdg_data) == "Latest.data4"] <- "Latest data (UHC/NOH)"
    
    DT::datatable(display_sdg_data, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$sdg_summary <- renderText({
    df <- data()
    if (nrow(df) == 0 || is.null(input$sdg_target)) return("No data selected")
    
    sdg_data <- df[df$SDG_Target == input$sdg_target, ]
    paste("Total Indicators:", nrow(sdg_data))
  })
  
  # UHC outputs - FIXED data type handling
  output$uhc_chart <- renderPlotly({
    df <- data()
    if (nrow(df) == 0) return(NULL)
    
    uhc_data <- df[df$is_UHC == TRUE, ]
    
    if (nrow(uhc_data) == 0) return(NULL)
    
    # Remove rows with NA values for Latest.data4
    uhc_data_clean <- uhc_data[!is.na(uhc_data$Latest.data4) & uhc_data$Latest.data4 != "", ]
    
    if (nrow(uhc_data_clean) == 0) {
      return(plot_ly() %>% add_annotations(text = "No UHC data available", showarrow = FALSE))
    }
    
    tryCatch({
      # Convert to numeric and handle non-numeric values
      numeric_data <- as.numeric(as.character(uhc_data_clean$Latest.data4))
      valid_indices <- !is.na(numeric_data)
      
      if (sum(valid_indices) == 0) {
        return(plot_ly() %>% add_annotations(text = "No numeric UHC data available", 
                                             showarrow = FALSE))
      }
      
      uhc_data_clean <- uhc_data_clean[valid_indices, ]
      uhc_data_clean$numeric_value <- numeric_data[valid_indices]
      
      # Calculate y-axis range starting from 0
      max_val <- max(uhc_data_clean$numeric_value, na.rm = TRUE)
      y_range <- c(0, max_val * 1.1)
      
      # ✅ Wrap labels for readability (Plotly needs <br> instead of \n)
      uhc_data_clean$wrapped_indicator <- str_wrap(uhc_data_clean$UHC.Indicator, width = 30)
      uhc_data_clean$wrapped_indicator <- gsub("\n", "<br>", uhc_data_clean$wrapped_indicator)
      
      # Plot
      p <- plot_ly(uhc_data_clean, x = ~wrapped_indicator, y = ~numeric_value, type = 'bar',
                   marker = list(color = '#2ecc71')) %>%
        layout(title = "UHC Indicators",
               xaxis = list(title = "UHC Indicator", 
                            tickangle = -45,  # if still crowded, set tickangle = 45 or 90
                            tickmode = "array",
                            tickvals = uhc_data_clean$wrapped_indicator,
                            ticktext = uhc_data_clean$wrapped_indicator),
               yaxis = list(title = "Value", range = y_range,
                            tickfont = list(size = 7)),
               margin = list(b = 150, l = 50, r = 50, t = 80))
      return(p)
      
    }, error = function(e) {
      return(plot_ly() %>% add_annotations(text = paste("Error processing data:", e$message), 
                                           showarrow = FALSE))
    })
  })
  
  output$uhc_table <- DT::renderDataTable({
    df <- data()
    if (nrow(df) == 0) return(NULL)
    
    uhc_data <- df[df$is_UHC == TRUE, ]
    
    # Remove is_UHC column from display since it's redundant in this context
    display_uhc_data <- uhc_data %>% select(-is_UHC)
    
    # Rename Latest data columns
    names(display_uhc_data)[names(display_uhc_data) == "Latest.data1"] <- "Latest data (GHO)"
    names(display_uhc_data)[names(display_uhc_data) == "Latest.data2"] <- "Latest data (FHSIS)"
    names(display_uhc_data)[names(display_uhc_data) == "Latest.data3"] <- "Latest data (PHS)"
    names(display_uhc_data)[names(display_uhc_data) == "Latest.data4"] <- "Latest data (UHC/NOH)"
    
    DT::datatable(display_uhc_data, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "filter_programme", selected = character(0))
    updateSelectInput(session, "filter_sdg", selected = character(0))
    updateSelectInput(session, "filter_year", selected = character(0))
    updateCheckboxInput(session, "filter_uhc", value = FALSE)
  })
  
  
  # ---- GATS Downloads ----
  output$download_zamboanga <- downloadHandler(
    filename = function() {"ZAMBOANGA_GATS2021_Factsheet_29MAR2023.pdf"},
    content = function(file) {
      file.copy("GATS/ZAMBOANGA_GATS2021_Factsheet_29MAR2023.pdf", file)
    }
  )
  
  output$download_quezon <- downloadHandler(
    filename = function() {"QUEZON_GATS2021_Factsheet_29MAR2023.pdf"},
    content = function(file) {
      file.copy("GATS/QUEZON_GATS2021_Factsheet_29MAR2023.pdf", file)
    }
  )
  
  output$download_philcomp <- downloadHandler(
    filename = function() {"PHL_GATS2021_FS-Comparison_29NOV2022.pdf"},
    content = function(file) {
      file.copy("GATS/PHL_GATS2021_FS-Comparison_29NOV2022.pdf", file)
    }
  )
  
  output$download_philfacts <- downloadHandler(
    filename = function() {"PHL_GATS2021_Factsheet_13FEB2023.pdf"},
    content = function(file) {
      file.copy("GATS/PHL_GATS2021_Factsheet_13FEB2023.pdf", file)
    }
  )
  
  output$download_gensan <- downloadHandler(
    filename = function() {"GENERALSANTOS_GATS2021_Factsheet_29MAR2023.pdf"},
    content = function(file) {
      file.copy("GATS/GENERALSANTOS_GATS2021_Factsheet_29MAR2023.pdf", file)
    }
  )
  
  output$download_cebu <- downloadHandler(
    filename = function() {"CEBU_GATS2021_Factsheet_27FEB2023.pdf"},
    content = function(file) {
      file.copy("GATS/CEBU_GATS2021_Factsheet_27FEB2023.pdf", file)
    }
  )
  
  output$download_baguio <- downloadHandler(
    filename = function() {"BAGUIO_GATS2021_Factsheet_16FEB2023.pdf"},
    content = function(file) {
      file.copy("GATS/BAGUIO_GATS2021_Factsheet_16FEB2023.pdf", file)
    }
  )
  
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)






# 2. portal with the map (map not appearing)--------------------------------------------------------------


shapefile <- st_read("MEPortal/project.shp")
project <- shapefile %>% 
  rename("Province" = "ADM2_EN",
         "Region" = "ADM1_EN") %>% 
  select(Province, Region, TEAM, PROGRAM, YEARIMP, geometry)

View(project)



# ui ----------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Health Indicators Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table")),
      menuItem("Programme Analysis", tabName = "programme", icon = icon("chart-bar")),
      menuItem("SDG Indicators", tabName = "sdg", icon = icon("globe")),
      menuItem("UHC Indicators", tabName = "uhc", icon = icon("heart")),
      menuItem("Project Monitoring", tabName = "project", icon = icon("map-marked-alt")),
      menuItem("Global Adult Tobacco Survey (GATS)", tabName = "gats", icon = icon("file-pdf"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 8px;
        }
        .box-header {
          font-weight: bold;
        }
        #project_map {
          height: 600px !important;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_indicators"),
          valueBoxOutput("uhc_indicators")
        ),
        fluidRow(
          box(
            title = "Legend", status = "warning", solidHeader = TRUE,
            width = 12,
            div(class = "legend-box",
                div(class = "legend-item", strong("FHSIS"), " – Field Health Services Information System"),
                div(class = "legend-item", strong("PHS"), " – Philippine Health Statistics"),
                div(class = "legend-item", strong("NOH"), " – National Objectives for Health"),
                div(class = "legend-item", strong("UHC"), " – Universal Health Care")
            )
          )
        ),
        fluidRow(
          box(
            title = "Number of Indicators per Source", status = "primary", solidHeader = TRUE,
            width = 6, height = 400,
            withSpinner(plotlyOutput("source_distribution"))
          ),
          box(
            title = "Number of Indicators per Programme", status = "info", solidHeader = TRUE,
            width = 6, height = 400,
            withSpinner(plotlyOutput("programme_distribution"))
          )
        ),
        fluidRow(
          box(
            title = "Recent Data Summary", status = "success", solidHeader = TRUE,
            width = 12,
            withSpinner(DT::dataTableOutput("summary_table"))
          )
        )
      ),
      
      # Data Explorer Tab
      tabItem(
        tabName = "explorer",
        fluidRow(
          box(
            title = "Filters", status = "primary", solidHeader = TRUE,
            width = 3,
            selectInput("filter_programme", "Programme/Disease:",
                        choices = NULL, multiple = TRUE),
            selectInput("filter_sdg", "SDG Target:",
                        choices = NULL, multiple = TRUE),
            selectInput("filter_year", "Year:",
                        choices = NULL, multiple = TRUE),
            checkboxInput("filter_uhc", "UHC Indicators Only", FALSE),
            actionButton("reset_filters", "Reset Filters", class = "btn-warning")
          ),
          box(
            title = "Data Table", status = "primary", solidHeader = TRUE,
            width = 9,
            withSpinner(DT::dataTableOutput("main_table"))
          )
        )
      ),
      
      # Programme Analysis Tab
      tabItem(
        tabName = "programme",
        fluidRow(
          box(
            title = "Programme Selection", status = "primary", solidHeader = TRUE,
            width = 3,
            selectInput("selected_programme", "Select Programme:", choices = NULL),
            radioButtons("data_source", "Data Source:",
                         choices = list(
                           "GHO" = "Latest.data1",
                           "FHSIS" = "Latest.data2", 
                           "PHS" = "Latest.data3",
                           "UHC/NOH" = "Latest.data4"
                         ))
          ),
          box(
            title = "Programme Indicators", status = "info", solidHeader = TRUE,
            width = 9,
            withSpinner(plotlyOutput("programme_chart")),
            br(),
            withSpinner(DT::dataTableOutput("programme_table"))
          )
        )
      ),
      
      # SDG Indicators Tab
      tabItem(
        tabName = "sdg",
        fluidRow(
          box(
            title = "SDG Target Analysis", status = "primary", solidHeader = TRUE,
            width = 4,
            selectInput("sdg_target", "SDG Target:", choices = NULL),
            br(),
            h4("SDG Target Summary:"),
            verbatimTextOutput("sdg_summary")
          ),
          box(
            title = "SDG Indicators Overview", status = "info", solidHeader = TRUE,
            width = 8,
            withSpinner(plotlyOutput("sdg_chart")),
            br(),
            withSpinner(DT::dataTableOutput("sdg_table"))
          )
        )
      ),
      
      # UHC Indicators Tab
      tabItem(
        tabName = "uhc",
        fluidRow(
          box(
            title = "UHC Indicators Analysis", status = "success", solidHeader = TRUE,
            width = 12,
            withSpinner(plotlyOutput("uhc_chart")),
            br(),
            withSpinner(DT::dataTableOutput("uhc_table"))
          )
        )
      ),
      
      # Project Monitoring Tab - NEW
      tabItem(
        tabName = "project",
        fluidRow(
          box(
            title = "Project Filters", status = "primary", solidHeader = TRUE,
            width = 3,
            selectInput("project_region", "Filter by Region:",
                        choices = NULL, multiple = TRUE),
            selectInput("project_team", "Filter by Team:",
                        choices = NULL, multiple = TRUE),
            selectInput("project_program", "Filter by Program:",
                        choices = NULL, multiple = TRUE),
            selectInput("project_year", "Filter by Implementation Year:",
                        choices = NULL, multiple = TRUE),
            actionButton("reset_project_filters", "Reset Filters", class = "btn-warning"),
            br(), br(),
            h4("Project Summary:"),
            verbatimTextOutput("project_summary")
          ),
          box(
            title = "Project Locations Map", status = "info", solidHeader = TRUE,
            width = 9,
            withSpinner(leafletOutput("project_map", height = "600px"))
          )
        ),
        fluidRow(
          box(
            title = "Project Details", status = "success", solidHeader = TRUE,
            width = 12,
            withSpinner(DT::dataTableOutput("project_table"))
          )
        )
      ),
      
      # GATS Tab
      tabItem(
        tabName = "gats",
        fluidRow(
          box(
            title = "Global Adult Tobacco Survey (GATS) Factsheets",
            status = "danger", solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("Zamboanga", 
                       p("Download the factsheet:"),
                       downloadButton("download_zamboanga", "Download PDF")),
              tabPanel("Quezon", 
                       p("Download the factsheet:"),
                       downloadButton("download_quezon", "Download PDF")),
              tabPanel("Philippines Comparison", 
                       p("Download the factsheet:"),
                       downloadButton("download_philcomp", "Download PDF")),
              tabPanel("Philippines Factsheet", 
                       p("Download the factsheet:"),
                       downloadButton("download_philfacts", "Download PDF")),
              tabPanel("General Santos", 
                       p("Download the factsheet:"),
                       downloadButton("download_gensan", "Download PDF")),
              tabPanel("Cebu", 
                       p("Download the factsheet:"),
                       downloadButton("download_cebu", "Download PDF")),
              tabPanel("Baguio", 
                       p("Download the factsheet:"),
                       downloadButton("download_baguio", "Download PDF"))
            )
          )
        )
      )
    )
  )
)


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive value to store the health indicators data - IMPROVED data cleaning
  data <- reactive({
    # Replace with your CSV file path
    csv_file <- "MEPortal/SDGs and UHC data summary.csv"
    
    # Check if file exists
    if (!file.exists(csv_file)) {
      showNotification("CSV file not found. Please ensure 'SDGs and UHC data summary.csv' is in the working directory.", 
                       type = "error", duration = 10)
      return(data.frame())
    }
    
    tryCatch({
      # Read and clean the data
      df <- read_csv(csv_file, locale = locale(encoding = "UTF-8"))
      
      # Clean column names
      colnames(df) <- gsub("[^A-Za-z0-9_]", ".", colnames(df))
      colnames(df) <- gsub("\\.+", ".", colnames(df))
      colnames(df) <- gsub("^\\.|\\.$", "", colnames(df))
      
      # Convert Year to numeric if it's not already
      if ("Year" %in% colnames(df)) {
        df$Year <- as.numeric(as.character(df$Year))
      }
      
      # Clean up the Latest.data columns - attempt to convert to numeric where possible
      data_cols <- c("Latest.data1", "Latest.data2", "Latest.data3", "Latest.data4")
      for (col in data_cols) {
        if (col %in% colnames(df)) {
          # First, clean the data by removing any non-numeric characters except decimal points and minus signs
          df[[col]] <- gsub("[^0-9.-]", "", as.character(df[[col]]))
          # Replace empty strings with NA
          df[[col]][df[[col]] == "" | df[[col]] == "NA"] <- NA
        }
      }
      
      # Handle missing values
      df[df == "" | df == "NA"] <- NA
      
      # Ensure is_UHC is logical
      if ("is_UHC" %in% colnames(df)) {
        df$is_UHC <- as.logical(df$is_UHC)
      }
      
      return(df)
      
    }, error = function(e) {
      showNotification(paste("Error reading CSV file:", e$message), 
                       type = "error", duration = 10)
      return(data.frame())
    })
  })
  
  # Reactive value to store the project data - NEW
  project_data <- reactive({
    shp_file <- "project.shp"
    
    # Check if shapefile exists
    if (!file.exists(shp_file)) {
      showNotification("Shapefile not found. Please ensure 'project.shp' and associated files are in the working directory.", 
                       type = "error", duration = 10)
      return(NULL)
    }
    
    tryCatch({
      # Read and process the shapefile
      shapefile <- st_read(shp_file, quiet = TRUE)
      project <- shapefile %>% 
        rename("Province" = "ADM2_EN",
               "Region" = "ADM1_EN") %>% 
        select(Province, Region, TEAM, PROGRAM, YEARIMP, geometry)
      
      return(project)
      
    }, error = function(e) {
      showNotification(paste("Error reading shapefile:", e$message), 
                       type = "error", duration = 10)
      return(NULL)
    })
  })
  
  # Update filter choices for health indicators
  observe({
    df <- data()
    if (nrow(df) > 0) {
      # Update programme choices
      programmes <- sort(unique(df$Programme.Disease.Programme[!is.na(df$Programme.Disease.Programme)]))
      updateSelectInput(session, "filter_programme", choices = programmes)
      updateSelectInput(session, "selected_programme", choices = programmes, selected = programmes[1])
      
      # Update SDG choices
      sdg_targets <- sort(unique(df$SDG_Target[!is.na(df$SDG_Target)]))
      updateSelectInput(session, "filter_sdg", choices = sdg_targets)
      updateSelectInput(session, "sdg_target", choices = sdg_targets, selected = sdg_targets[1])
      
      # Update year choices
      years <- sort(unique(df$Year[!is.na(df$Year)]), decreasing = TRUE)
      updateSelectInput(session, "filter_year", choices = years)
    }
  })
  
  # Update filter choices for project monitoring - NEW
  observe({
    project <- project_data()
    if (!is.null(project) && nrow(project) > 0) {
      # Update region choices
      regions <- sort(unique(project$Region[!is.na(project$Region)]))
      updateSelectInput(session, "project_region", choices = regions)
      
      # Update team choices
      teams <- sort(unique(project$TEAM[!is.na(project$TEAM)]))
      updateSelectInput(session, "project_team", choices = teams)
      
      # Update program choices
      programs <- sort(unique(project$PROGRAM[!is.na(project$PROGRAM)]))
      updateSelectInput(session, "project_program", choices = programs)
      
      # Update year choices
      years <- sort(unique(project$YEARIMP[!is.na(project$YEARIMP)]), decreasing = TRUE)
      updateSelectInput(session, "project_year", choices = years)
    }
  })
  
  # Filtered data for explorer
  filtered_data <- reactive({
    df <- data()
    if (nrow(df) == 0) return(df)
    
    # Apply filters
    if (!is.null(input$filter_programme) && length(input$filter_programme) > 0) {
      df <- df[df$Programme.Disease.Programme %in% input$filter_programme, ]
    }
    
    if (!is.null(input$filter_sdg) && length(input$filter_sdg) > 0) {
      df <- df[df$SDG_Target %in% input$filter_sdg, ]
    }
    
    if (!is.null(input$filter_year) && length(input$filter_year) > 0) {
      df <- df[df$Year %in% input$filter_year, ]
    }
    
    if (input$filter_uhc) {
      df <- df[df$is_UHC == TRUE | !is.na(df$Latest.data4), ]
    }
    
    return(df)
  })
  
  # Filtered project data - NEW
  filtered_project_data <- reactive({
    project <- project_data()
    if (is.null(project) || nrow(project) == 0) return(project)
    
    # Apply filters
    if (!is.null(input$project_region) && length(input$project_region) > 0) {
      project <- project[project$Region %in% input$project_region, ]
    }
    
    if (!is.null(input$project_team) && length(input$project_team) > 0) {
      project <- project[project$TEAM %in% input$project_team, ]
    }
    
    if (!is.null(input$project_program) && length(input$project_program) > 0) {
      project <- project[project$PROGRAM %in% input$project_program, ]
    }
    
    if (!is.null(input$project_year) && length(input$project_year) > 0) {
      project <- project[project$YEARIMP %in% input$project_year, ]
    }
    
    return(project)
  })
  
  # Value boxes
  output$total_indicators <- renderValueBox({
    valueBox(
      value = nrow(data()),
      subtitle = "Total Indicators",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$uhc_indicators <- renderValueBox({
    df <- data()
    uhc_count <- sum(df$is_UHC == TRUE, na.rm = TRUE)
    valueBox(
      value = uhc_count,
      subtitle = "UHC Indicators",
      icon = icon("heart"),
      color = "red"
    )
  })
  
  # Charts - Source distribution
  output$source_distribution <- renderPlotly({
    df <- data()
    if (nrow(df) == 0) return(NULL)
    
    source_counts <- data.frame(
      Source = c("FHSIS", "PHS", "GHO", "UHC/NOH"),
      Count = c(
        sum(!is.na(df$Latest.data2)),
        sum(!is.na(df$Latest.data3)),
        sum(!is.na(df$Latest.data1)),
        sum(!is.na(df$Latest.data4))
      )
    )
    
    p <- plot_ly(source_counts, x = ~Source, y = ~Count, type = 'bar',
                 marker = list(color = c('#e74c3c', '#2ecc71', '#3498db', '#f39c12'))) %>%
      layout(title = "Number of Indicators per Source",
             xaxis = list(title = "Data Source"),
             yaxis = list(title = "Number of Indicators"))
    p
  })
  
  # Programme distribution chart
  output$programme_distribution <- renderPlotly({
    df <- data()
    if (nrow(df) == 0) return(NULL)
    
    prog_counts <- df %>%
      count(Programme.Disease.Programme) %>%
      arrange(desc(n)) %>%
      head(10)
    
    p <- plot_ly(prog_counts, x = ~Programme.Disease.Programme, y = ~n, 
                 type = 'bar',
                 marker = list(color = '#9b59b6')) %>%
      layout(title = "Top 10 Programmes by Indicator Count",
             xaxis = list(title = "Programme"),
             yaxis = list(title = "Number of Indicators"),
             margin = list(b = 150))
    p
  })
  
  # Tables
  output$main_table <- DT::renderDataTable({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    # Remove is_UHC column from display
    display_df <- df %>% select(-is_UHC)
    
    # Rename Latest data columns
    names(display_df)[names(display_df) == "Latest.data1"] <- "Latest data (GHO)"
    names(display_df)[names(display_df) == "Latest.data2"] <- "Latest data (FHSIS)"
    names(display_df)[names(display_df) == "Latest.data3"] <- "Latest data (PHS)"
    names(display_df)[names(display_df) == "Latest.data4"] <- "Latest data (UHC/NOH)"
    
    DT::datatable(display_df, 
                  options = list(
                    scrollX = TRUE,
                    pageLength = 15,
                    searchHighlight = TRUE
                  ),
                  filter = 'top',
                  rownames = FALSE) %>%
      formatStyle(columns = colnames(display_df), fontSize = '12px')
  })
  
  output$summary_table <- DT::renderDataTable({
    df <- data()
    if (nrow(df) == 0) return(NULL)
    
    summary_df <- df %>%
      select(Programme.Disease.Programme, Indicator, Year, Latest.data1, Latest.data2, Latest.data3, Latest.data4) %>%
      head(10)
    
    # Rename Latest data columns
    colnames(summary_df)[colnames(summary_df) == "Latest.data1"] <- "GHO"
    colnames(summary_df)[colnames(summary_df) == "Latest.data2"] <- "FHSIS"
    colnames(summary_df)[colnames(summary_df) == "Latest.data3"] <- "PHS"
    colnames(summary_df)[colnames(summary_df) == "Latest.data4"] <- "UHC/NOH"
    
    DT::datatable(summary_df, 
                  options = list(
                    pageLength = 5,
                    dom = 't'
                  ),
                  rownames = FALSE)
  })
  
  # Programme-specific outputs - FIXED data type handling
  output$programme_chart <- renderPlotly({
    df <- data()
    if (nrow(df) == 0 || is.null(input$selected_programme)) return(NULL)
    
    prog_data <- df[df$Programme.Disease.Programme == input$selected_programme, ]
    data_col <- input$data_source
    
    if (nrow(prog_data) == 0) return(NULL)
    
    # Ensure data column exists and filter out NAs
    if (!data_col %in% colnames(prog_data)) {
      return(plot_ly() %>% add_annotations(text = "Selected data source not available", 
                                           showarrow = FALSE))
    }
    
    prog_data_clean <- prog_data[!is.na(prog_data[[data_col]]) & prog_data[[data_col]] != "", ]
    
    if (nrow(prog_data_clean) == 0) {
      return(
        plot_ly() %>% 
          add_annotations(
            text = "No data available for selected source", 
            xref = "paper", yref = "paper", 
            x = 0.5, y = 0.5,  # center
            showarrow = FALSE, font = list(size = 14)
          ) %>%
          layout(
            xaxis = list(showticklabels = FALSE, showline = FALSE, zeroline = FALSE, showgrid = FALSE),
            yaxis = list(showticklabels = FALSE, showline = FALSE, zeroline = FALSE, showgrid = FALSE)
          )
      )
    }
    
    
    # FIXED: Convert to numeric and handle non-numeric values
    tryCatch({
      # Convert the data column to numeric
      numeric_data <- as.numeric(as.character(prog_data_clean[[data_col]]))
      
      # Remove rows where conversion failed (resulted in NA)
      valid_indices <- !is.na(numeric_data)
      
      if (sum(valid_indices) == 0) {
        return(plot_ly() %>% add_annotations(text = "No numeric data available for selected source", 
                                             showarrow = FALSE))
      }
      
      # Filter the dataframe to only include rows with valid numeric data
      prog_data_clean <- prog_data_clean[valid_indices, ]
      numeric_data <- numeric_data[valid_indices]
      
      # Add the numeric data back to the dataframe for plotting
      prog_data_clean$numeric_value <- numeric_data
      
      # Calculate y-axis range starting from 0
      max_val <- max(numeric_data, na.rm = TRUE)
      min_val <- min(numeric_data, na.rm = TRUE)
      
      # Handle edge cases
      if (is.infinite(max_val) || is.infinite(min_val) || is.na(max_val) || is.na(min_val)) {
        return(plot_ly() %>% add_annotations(text = "Invalid data values", 
                                             showarrow = FALSE))
      }
      
      y_range <- c(0, max_val * 1.1)
      
      p <- plot_ly(prog_data_clean, x = ~Year, y = ~numeric_value,
                   text = ~Indicator, type = 'scatter', mode = 'markers+lines') %>%
        layout(title = paste("Trend for", input$selected_programme),
               xaxis = list(title = "Year", 
                            dtick = 1,
                            tick0 = min(prog_data_clean$Year, na.rm = TRUE)),
               yaxis = list(title = "Value", range = y_range))
      return(p)
      
    }, error = function(e) {
      return(plot_ly() %>% add_annotations(text = paste("Error processing data:", e$message), 
                                           showarrow = FALSE))
    })
  })
  
  output$programme_table <- DT::renderDataTable({
    df <- data()
    if (nrow(df) == 0 || is.null(input$selected_programme)) return(NULL)
    
    prog_data <- df[df$Programme.Disease.Programme == input$selected_programme, ]
    
    # Remove is_UHC column from display
    display_prog_data <- prog_data %>% select(-is_UHC)
    
    # Rename Latest data columns to match Data Explorer
    names(display_prog_data)[names(display_prog_data) == "Latest.data1"] <- "Latest data (GHO)"
    names(display_prog_data)[names(display_prog_data) == "Latest.data2"] <- "Latest data (FHSIS)"
    names(display_prog_data)[names(display_prog_data) == "Latest.data3"] <- "Latest data (PHS)"
    names(display_prog_data)[names(display_prog_data) == "Latest.data4"] <- "Latest data (UHC/NOH)"
    
    DT::datatable(display_prog_data, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # SDG outputs - FIXED data type handling
  output$sdg_chart <- renderPlotly({
    df <- data()
    if (nrow(df) == 0 || is.null(input$sdg_target)) return(NULL)
    
    sdg_data <- df[df$SDG_Target == input$sdg_target, ]
    
    if (nrow(sdg_data) == 0) return(NULL)
    
    # Remove rows with NA values for Latest.data1
    sdg_data_clean <- sdg_data[!is.na(sdg_data$Latest.data1) & sdg_data$Latest.data1 != "", ]
    
    if (nrow(sdg_data_clean) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
    }
    
    tryCatch({
      # Convert to numeric and handle non-numeric values
      numeric_data <- as.numeric(as.character(sdg_data_clean$Latest.data1))
      valid_indices <- !is.na(numeric_data)
      
      if (sum(valid_indices) == 0) {
        return(plot_ly() %>% add_annotations(text = "No numeric data available", 
                                             showarrow = FALSE))
      }
      
      sdg_data_clean <- sdg_data_clean[valid_indices, ]
      sdg_data_clean$numeric_value <- numeric_data[valid_indices]
      
      max_val <- max(sdg_data_clean$numeric_value, na.rm = TRUE)
      y_range <- c(0, max_val * 1.1)
      
      p <- plot_ly(sdg_data_clean, x = ~Indicator, y = ~numeric_value, type = 'bar') %>%
        layout(title = paste("SDG Target:", input$sdg_target),
               xaxis = list(title = "Indicator"),
               yaxis = list(title = "Value", range = y_range),
               margin = list(b = 150))
      return(p)
      
    }, error = function(e) {
      return(plot_ly() %>% add_annotations(text = paste("Error processing data:", e$message), 
                                           showarrow = FALSE))
    })
  })
  
  output$sdg_table <- DT::renderDataTable({
    df <- data()
    if (nrow(df) == 0 || is.null(input$sdg_target)) return(NULL)
    
    sdg_data <- df[df$SDG_Target == input$sdg_target, ]
    
    # Remove is_UHC column from display
    display_sdg_data <- sdg_data %>% select(-is_UHC)
    
    # Rename Latest data columns
    names(display_sdg_data)[names(display_sdg_data) == "Latest.data1"] <- "Latest data (GHO)"
    names(display_sdg_data)[names(display_sdg_data) == "Latest.data2"] <- "Latest data (FHSIS)"
    names(display_sdg_data)[names(display_sdg_data) == "Latest.data3"] <- "Latest data (PHS)"
    names(display_sdg_data)[names(display_sdg_data) == "Latest.data4"] <- "Latest data (UHC/NOH)"
    
    DT::datatable(display_sdg_data, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$sdg_summary <- renderText({
    df <- data()
    if (nrow(df) == 0 || is.null(input$sdg_target)) return("No data selected")
    
    sdg_data <- df[df$SDG_Target == input$sdg_target, ]
    paste("Total Indicators:", nrow(sdg_data))
  })
  
  # UHC outputs - FIXED data type handling
  output$uhc_chart <- renderPlotly({
    df <- data()
    if (nrow(df) == 0) return(NULL)
    
    uhc_data <- df[df$is_UHC == TRUE, ]
    
    if (nrow(uhc_data) == 0) return(NULL)
    
    # Remove rows with NA values for Latest.data4
    uhc_data_clean <- uhc_data[!is.na(uhc_data$Latest.data4) & uhc_data$Latest.data4 != "", ]
    
    if (nrow(uhc_data_clean) == 0) {
      return(plot_ly() %>% add_annotations(text = "No UHC data available", showarrow = FALSE))
    }
    
    tryCatch({
      # Convert to numeric and handle non-numeric values
      numeric_data <- as.numeric(as.character(uhc_data_clean$Latest.data4))
      valid_indices <- !is.na(numeric_data)
      
      if (sum(valid_indices) == 0) {
        return(plot_ly() %>% add_annotations(text = "No numeric UHC data available", 
                                             showarrow = FALSE))
      }
      
      uhc_data_clean <- uhc_data_clean[valid_indices, ]
      uhc_data_clean$numeric_value <- numeric_data[valid_indices]
      
      # Calculate y-axis range starting from 0
      max_val <- max(uhc_data_clean$numeric_value, na.rm = TRUE)
      y_range <- c(0, max_val * 1.1)
      
      # ✅ Wrap labels for readability (Plotly needs <br> instead of \n)
      uhc_data_clean$wrapped_indicator <- str_wrap(uhc_data_clean$UHC.Indicator, width = 30)
      uhc_data_clean$wrapped_indicator <- gsub("\n", "<br>", uhc_data_clean$wrapped_indicator)
      
      # Plot
      p <- plot_ly(uhc_data_clean, x = ~wrapped_indicator, y = ~numeric_value, type = 'bar',
                   marker = list(color = '#2ecc71')) %>%
        layout(title = "UHC Indicators",
               xaxis = list(title = "UHC Indicator", 
                            tickangle = -45,  # if still crowded, set tickangle = 45 or 90
                            tickmode = "array",
                            tickvals = uhc_data_clean$wrapped_indicator,
                            ticktext = uhc_data_clean$wrapped_indicator),
               yaxis = list(title = "Value", range = y_range,
                            tickfont = list(size = 7)),
               margin = list(b = 150, l = 50, r = 50, t = 80))
      return(p)
      
    }, error = function(e) {
      return(plot_ly() %>% add_annotations(text = paste("Error processing data:", e$message), 
                                           showarrow = FALSE))
    })
  })
  
  output$uhc_table <- DT::renderDataTable({
    df <- data()
    if (nrow(df) == 0) return(NULL)
    
    uhc_data <- df[df$is_UHC == TRUE, ]
    
    # Remove is_UHC column from display since it's redundant in this context
    display_uhc_data <- uhc_data %>% select(-is_UHC)
    
    # Rename Latest data columns
    names(display_uhc_data)[names(display_uhc_data) == "Latest.data1"] <- "Latest data (GHO)"
    names(display_uhc_data)[names(display_uhc_data) == "Latest.data2"] <- "Latest data (FHSIS)"
    names(display_uhc_data)[names(display_uhc_data) == "Latest.data3"] <- "Latest data (PHS)"
    names(display_uhc_data)[names(display_uhc_data) == "Latest.data4"] <- "Latest data (UHC/NOH)"
    
    DT::datatable(display_uhc_data, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # PROJECT MONITORING OUTPUTS - NEW
  
  # Project map
  output$project_map <- renderLeaflet({
    project <- filtered_project_data()
    if (is.null(project) || nrow(project) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = 121.7740, lat = 12.8797, zoom = 6) %>%
               addMarkers(lng = 121.7740, lat = 12.8797, popup = "No project data available"))
    }
    
    tryCatch({
      # Transform to WGS84 if needed
      if (st_crs(project) != st_crs(4326)) {
        project <- st_transform(project, 4326)
      }
      
      # Create color palette based on PROGRAM
      programs <- unique(project$PROGRAM[!is.na(project$PROGRAM)])
      if (length(programs) > 0) {
        color_palette <- colorFactor(
          palette = rainbow(length(programs)),
          domain = programs
        )
        
        # Create popup text
        project$popup_text <- paste0(
          "<b>Province:</b> ", project$Province, "<br>",
          "<b>Region:</b> ", project$Region, "<br>",
          "<b>Team:</b> ", ifelse(is.na(project$TEAM), "N/A", project$TEAM), "<br>",
          "<b>Program:</b> ", ifelse(is.na(project$PROGRAM), "N/A", project$PROGRAM), "<br>",
          "<b>Implementation Year:</b> ", ifelse(is.na(project$YEARIMP), "N/A", project$YEARIMP)
        )
        
        # Get colors for each polygon
        colors <- ifelse(is.na(project$PROGRAM), "#808080", color_palette(project$PROGRAM))
        
        leaflet(project) %>%
          addTiles() %>%
          addPolygons(
            fillColor = colors,
            weight = 1,
            opacity = 1,
            color = "black",
            dashArray = "1",
            fillOpacity = 0.7,
            popup = ~popup_text,
            highlight = highlightOptions(
              weight = 3,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.9,
              bringToFront = TRUE
            )
          ) %>%
          addLegend(
            pal = color_palette,
            values = programs,
            opacity = 0.7,
            title = "Programs",
            position = "bottomright"
          ) %>%
          fitBounds(
            lng1 = st_bbox(project)[1], lat1 = st_bbox(project)[2],
            lng2 = st_bbox(project)[3], lat2 = st_bbox(project)[4]
          )
      } else {
        # Fallback map without color coding
        leaflet(project) %>%
          addTiles() %>%
          addPolygons(
            fillColor = "#808080",
            weight = 1,
            opacity = 1,
            color = "black",
            dashArray = "1",
            fillOpacity = 0.7,
            popup = ~paste0(
              "<b>Province:</b> ", Province, "<br>",
              "<b>Region:</b> ", Region, "<br>",
              "<b>Team:</b> ", ifelse(is.na(TEAM), "N/A", TEAM), "<br>",
              "<b>Program:</b> ", ifelse(is.na(PROGRAM), "N/A", PROGRAM), "<br>",
              "<b>Implementation Year:</b> ", ifelse(is.na(YEARIMP), "N/A", YEARIMP)
            )
          ) %>%
          fitBounds(
            lng1 = st_bbox(project)[1], lat1 = st_bbox(project)[2],
            lng2 = st_bbox(project)[3], lat2 = st_bbox(project)[4]
          )
      }
      
    }, error = function(e) {
      leaflet() %>% 
        addTiles() %>% 
        setView(lng = 121.7740, lat = 12.8797, zoom = 6) %>%
        addMarkers(lng = 121.7740, lat = 12.8797, 
                   popup = paste("Error loading map:", e$message))
    })
  })
  
  # Project table
  output$project_table <- DT::renderDataTable({
    project <- filtered_project_data()
    if (is.null(project) || nrow(project) == 0) return(NULL)
    
    # Convert to regular dataframe and remove geometry for table display
    project_df <- project %>% 
      st_drop_geometry() %>%
      as.data.frame()
    
    DT::datatable(project_df,
                  options = list(
                    scrollX = TRUE,
                    pageLength = 15,
                    searchHighlight = TRUE
                  ),
                  filter = 'top',
                  rownames = FALSE) %>%
      formatStyle(columns = colnames(project_df), fontSize = '12px')
  })
  
  # Project summary
  output$project_summary <- renderText({
    project <- filtered_project_data()
    if (is.null(project) || nrow(project) == 0) return("No project data available")
    
    total_provinces <- nrow(project)
    total_regions <- length(unique(project$Region[!is.na(project$Region)]))
    total_teams <- length(unique(project$TEAM[!is.na(project$TEAM)]))
    total_programs <- length(unique(project$PROGRAM[!is.na(project$PROGRAM)]))
    
    paste(
      "Total Provinces:", total_provinces, "\n",
      "Total Regions:", total_regions, "\n",
      "Total Teams:", total_teams, "\n",
      "Total Programs:", total_programs
    )
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "filter_programme", selected = character(0))
    updateSelectInput(session, "filter_sdg", selected = character(0))
    updateSelectInput(session, "filter_year", selected = character(0))
    updateCheckboxInput(session, "filter_uhc", value = FALSE)
  })
  
  # Reset project filters - NEW
  observeEvent(input$reset_project_filters, {
    updateSelectInput(session, "project_region", selected = character(0))
    updateSelectInput(session, "project_team", selected = character(0))
    updateSelectInput(session, "project_program", selected = character(0))
    updateSelectInput(session, "project_year", selected = character(0))
  })
  
  # ---- GATS Downloads ----
  output$download_zamboanga <- downloadHandler(
    filename = function() {"ZAMBOANGA_GATS2021_Factsheet_29MAR2023.pdf"},
    content = function(file) {
      file.copy("GATS/ZAMBOANGA_GATS2021_Factsheet_29MAR2023.pdf", file)
    }
  )
  
  output$download_quezon <- downloadHandler(
    filename = function() {"QUEZON_GATS2021_Factsheet_29MAR2023.pdf"},
    content = function(file) {
      file.copy("GATS/QUEZON_GATS2021_Factsheet_29MAR2023.pdf", file)
    }
  )
  
  output$download_philcomp <- downloadHandler(
    filename = function() {"PHL_GATS2021_FS-Comparison_29NOV2022.pdf"},
    content = function(file) {
      file.copy("GATS/PHL_GATS2021_FS-Comparison_29NOV2022.pdf", file)
    }
  )
  
  output$download_philfacts <- downloadHandler(
    filename = function() {"PHL_GATS2021_Factsheet_13FEB2023.pdf"},
    content = function(file) {
      file.copy("GATS/PHL_GATS2021_Factsheet_13FEB2023.pdf", file)
    }
  )
  
  output$download_gensan <- downloadHandler(
    filename = function() {"GENERALSANTOS_GATS2021_Factsheet_29MAR2023.pdf"},
    content = function(file) {
      file.copy("GATS/GENERALSANTOS_GATS2021_Factsheet_29MAR2023.pdf", file)
    }
  )
  
  output$download_cebu <- downloadHandler(
    filename = function() {"CEBU_GATS2021_Factsheet_27FEB2023.pdf"},
    content = function(file) {
      file.copy("GATS/CEBU_GATS2021_Factsheet_27FEB2023.pdf", file)
    }
  )
  
  output$download_baguio <- downloadHandler(
    filename = function() {"BAGUIO_GATS2021_Factsheet_16FEB2023.pdf"},
    content = function(file) {
      file.copy("GATS/BAGUIO_GATS2021_Factsheet_16FEB2023.pdf", file)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)




# add health related legislations -----------------------------------------






