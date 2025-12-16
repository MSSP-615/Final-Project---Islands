# Islands project

library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(plotly)
library(gt)
library(leaflet)
library(here)
library(sf)
library(tigris)

# Set tigris cache
options(tigris_use_cache = TRUE)

# Load data
ethnicity <- read_csv(here("data/clean_data/ethnicity.csv"))
hurricanes <- read_csv(here("data/clean_data/hurricanes.csv")) %>%
  mutate(landfall_date = as.Date(landfall_date))
weather <- read_csv(here("data/clean_data/usvi_clean_weather.csv"))
population <- read_csv(here("data/clean_data/wdi_pop_long.csv"))
wdi_economic <- read_csv(here("data/clean_data/wdi_economic.csv"))
tourism <- read_csv(here("data/clean_data/tourism_arrivals.csv"))

# Prepare hurricane years for merging
hurricane_years <- hurricanes %>%
  select(Year, `Hurricane name`, Category) %>%
  rename(year = Year)

# UI
ui <- page_navbar(
  title = "USVI Dashboard",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2C3E50",
    secondary = "#18BC9C",
    success = "#18BC9C",
    info = "#3498DB",
    bg = "#ECF0F1",
    fg = "#2C3E50"
  ),
  
  # Overview Tab
  nav_panel(
    title = "Overview",
    icon = bs_icon("house-door"),
    
    div(
      style = "
    padding: 30px 20px;
    background: linear-gradient(135deg, #2C3E50, #18BC9C);
    color: white;
    border-radius: 12px;
    text-align: center;
    ",
      h1("Learn about the U.S. Virgin Islands!"),
      h4("An interactive dashboard exploring people, climate, tourism, and hurricanes")
    ),
    
    div(
      style = "padding: 20px;",
      
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        
        value_box(
          title = "Total Population",
          value = "87,146",
          showcase = bs_icon("people"),
          theme = "primary",
          p("2020 Census")
        ),
        
        value_box(
          title = "Total Area",
          value = "133 mi²",
          showcase = bs_icon("geo-alt"),
          theme = "success",
          p("346 km²")
        ),
        
        value_box(
          title = "Hurricanes",
          value = "13",
          showcase = bs_icon("cloud-lightning"),
          theme = "warning",
          p("1989-2019")
        ),
        
        value_box(
          title = "Annual Tourism",
          value = "2-2.5M",
          showcase = bs_icon("airplane"),
          theme = "info",
          p("Pre-2017 average")
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          full_screen = TRUE,
          card_header("Map of the US Virgin Islands"),
          card_body(
            leafletOutput("overview_map", height = "400px"),
            div(style = "margin-top: 10px;",
                selectInput(
                  "mapStyle",
                  "Map type",
                  choices = c(
                    "Street Map" = "OpenStreetMap",
                    "Satellite" = "Esri.WorldImagery",
                    "Topographic" = "Esri.WorldTopoMap"
                  ),
                  selected = "OpenStreetMap",
                  width = "250px"
                )
            )
          )
        ),
        
        card(
          card_header("About the USVI"),
          p("The United States Virgin Islands (USVI) is an unincorporated territory situated in the northeastern 
            Caribbean Sea, approximately 40 miles east of Puerto Rico."),
          tags$ul(
            tags$li(strong("Official Language:"), " English"),
            tags$li(strong("Currency:"), " US Dollar (USD)")
          ),
          p("The territory comprises three main islands along with approximately 50 smaller islets and cays. 
            The three main islands are:"),
          tags$ul(
            tags$li(strong("St. Croix"), "- The largest island"),
            tags$li(strong("St. Thomas"), "- Capital (Charlotte Amalie)"),
            tags$li(strong("St. John"), "- Known for national parks")
          ),
          p("The islands feature a subtropical climate with temperatures averaging around 78°F, moderated by 
            steady northeasterly trade winds, and experience distinct dry and wet seasons with annual rainfall 
            averaging 45 inches. Since the United States acquired the territory, it has since developed a 
            diverse economy centered on tourism, with approximately 2.5 million visitors annually. The USVI's 
            geographic location in the Atlantic hurricane belt, combined with its tropical climate and island 
            topography, makes it particularly vulnerable to extreme weather events, while its small land area 
            and limited freshwater resources present ongoing environmental and infrastructure challenges. 
            Understanding the interplay between the territory's climate patterns and demographic trends is 
            essential for sustainable development planning and disaster preparedness in this unique 
            Caribbean archipelago.")
        )
      ), 
      
      layout_columns(
        col_widths = 12, 
        
        card(
          full_screen = TRUE,
          card_header("USVI Photo Carousel"),
          card_body(
            style = "padding: 0;",
            
            tags$div(
              id = "usviCarousel",
              class = "carousel slide",
              `data-bs-ride` = "carousel",
              `data-bs-interval` = "5000",
              
              # --- Dots (one per image) ---
              tags$div(
                class = "carousel-indicators",
                tags$button(`data-bs-target` = "#usviCarousel", `data-bs-slide-to` = "0", 
                            class = "active", `aria-current` = "true"),
                tags$button(`data-bs-target` = "#usviCarousel", `data-bs-slide-to` = "1"),
                tags$button(`data-bs-target` = "#usviCarousel", `data-bs-slide-to` = "2"),
                tags$button(`data-bs-target` = "#usviCarousel", `data-bs-slide-to` = "3"),
                tags$button(`data-bs-target` = "#usviCarousel", `data-bs-slide-to` = "4"),
                tags$button(`data-bs-target` = "#usviCarousel", `data-bs-slide-to` = "5"),
                tags$button(`data-bs-target` = "#usviCarousel", `data-bs-slide-to` = "6"),
                tags$button(`data-bs-target` = "#usviCarousel", `data-bs-slide-to` = "7"),
                tags$button(`data-bs-target` = "#usviCarousel", `data-bs-slide-to` = "8"),
                tags$button(`data-bs-target` = "#usviCarousel", `data-bs-slide-to` = "9")
              ),
              
              # --- Slides ---
              tags$div(
                class = "carousel-inner",
                
                tags$div(
                  class = "carousel-item active",
                  tags$img(
                    src = "Birds%20Eye%20-%20St.%20John.webp",
                    class = "d-block w-100",
                    style = "height: 60vh; object-fit: cover; border-radius: 0;"
                  )
                ),
                
                tags$div(class = "carousel-item",
                         tags$img(src = "Cane%20Bay%20Beach%20-%20St.%20Croix.jpg",
                                  class = "d-block w-100",
                                  style = "height: 60vh; object-fit: cover; border-radius: 0;")),
                
                tags$div(class = "carousel-item",
                         tags$img(src = "Charlotte%20Amalie-St.%20Thomas.jpg",
                                  class = "d-block w-100",
                                  style = "height: 60vh; object-fit: cover; border-radius: 0;")),
                
                tags$div(class = "carousel-item",
                         tags$img(src = "Harbor%20-%20St.%20John.jpg",
                                  class = "d-block w-100",
                                  style = "height: 60vh; object-fit: cover; border-radius: 0;")),
                
                tags$div(class = "carousel-item",
                         tags$img(src = "Lindquist%20Beach%20-%20St.%20Thomas.webp",
                                  class = "d-block w-100",
                                  style = "height: 60vh; object-fit: cover; border-radius: 0;")),
                
                tags$div(class = "carousel-item",
                         tags$img(src = "Scuba%20-%20St.%20Croix.jpg",
                                  class = "d-block w-100",
                                  style = "height: 60vh; object-fit: cover; border-radius: 0;")),
                
                tags$div(class = "carousel-item",
                         tags$img(src = "St.-Croix.jpg",
                                  class = "d-block w-100",
                                  style = "height: 60vh; object-fit: cover; border-radius: 0;")),
                
                tags$div(class = "carousel-item",
                         tags$img(src = "Sunset%20-%20St.%20Thomas.jpg",
                                  class = "d-block w-100",
                                  style = "height: 60vh; object-fit: cover; border-radius: 0;")),
                
                tags$div(class = "carousel-item",
                         tags$img(src = "xst-thomas.jpeg",
                                  class = "d-block w-100",
                                  style = "height: 60vh; object-fit: cover; border-radius: 0;")),
                
                tags$div(class = "carousel-item",
                         tags$img(src = "xst-thomas2.jpeg",
                                  class = "d-block w-100",
                                  style = "height: 60vh; object-fit: cover; border-radius: 0;"))
              ),
              
              # --- Prev/Next buttons ---
              tags$button(
                class = "carousel-control-prev",
                type = "button",
                `data-bs-target` = "#usviCarousel",
                `data-bs-slide` = "prev",
                tags$span(class = "carousel-control-prev-icon", `aria-hidden` = "true"),
                tags$span(class = "visually-hidden", "Previous")
              ),
              tags$button(
                class = "carousel-control-next",
                type = "button",
                `data-bs-target` = "#usviCarousel",
                `data-bs-slide` = "next",
                tags$span(class = "carousel-control-next-icon", `aria-hidden` = "true"),
                tags$span(class = "visually-hidden", "Next")
              )
            )
          ) 
        )
      )
    )
  ),
  
  # Demographics Tab
  nav_panel(
    title = "Demographics",
    icon = bs_icon("people"),
    
    div(
      style = "padding: 20px;",
      
      # ---- TOP: Table (left) + Blurb (right) ----
      layout_columns(
        col_widths = c(7, 5),
        
        card(
          full_screen = TRUE,
          card_header("Detailed Demographics Table"),
          card_body(
            gt_output("demo_table")
          )
        ),
        
        card(
          card_header("Population & Demographic Findings"),
          card_body(
            p("The U.S. Virgin Islands has a diverse population shaped by migration, economic shifts, and hurricane impacts."),
            tags$ul(
              tags$li(strong("Population:"), "87,146 (2020 Census)"),
              tags$li(strong("Key story:"), "population change over time and shifts in age structure")
            ),
            p("Black or African American residents represent the overwhelming majority (71.4%). White residents comprise 13.3% of 
            the population. Two or More Races accounts for 7.5%, and other racial categories each represent less than 7% of the 
            population. This diversity reflects the USVI's historical and cultural ties to the Caribbean region.
            Overall, the population increased until about 2000 and has been declining since 2010. The elderly 
            population is increasing as the youth population declines over time.")
          )
        )
      ),
      
      div(style = "height: 15px;"),
      
      # ---- BOTTOM: Full-width interactive plot + control ----
      card(
        full_screen = TRUE,
        card_header(
          "Demographic Analysis",
          div(
            style = "float: right; width: 320px;",
            selectInput(
              "demo_viz_type",
              NULL,
              choices = c(
                "Ethnicity Distribution"        = "ethnicity",
                "Population Over Time"          = "pop_time",
                "Population Growth Rate"        = "pop_growth",
                "Age Structure"                 = "age_structure"
              ),
              selected = "ethnicity"
            )
          )
        ),
        card_body(
          uiOutput("demo_plot_ui")
        )
      )
    )
  ),
  
  # Tourism Tab
  nav_panel(
    title = "Tourism",
    icon = bs_icon("airplane"),
    
    div(
      style = "padding: 20px;",
      
      card(
        class = "mb-3",
        card_header("Tourism in the U.S. Virgin Islands"),
        card_body(
          p("Tourism is the cornerstone of the U.S. Virgin Islands economy, driving employment, infrastructure development, 
            and public revenue across the territory. Each year, the USVI welcomes millions of visitors through a combination 
            of air travel and cruise ship tourism."),
          p("St. Thomas and St. John serve as major cruise destinations, while St. Croix attracts longer-stay visitors seeking 
            cultural, historical, and ecological experiences. These distinct tourism patterns shape island-level trends in 
            visitor arrivals."),
          p("However, tourism in the USVI is highly sensitive to external shocks, particularly extreme weather events. Hurricanes, most 
            notably Hurricanes Irma and Maria in 2017, caused sharp declines in visitor arrivals, followed by gradual recovery 
            over subsequent years. Differences in recovery trajectories between air and cruise tourism highlight the varying 
            resilience of tourism sectors and underscore the importance of disaster preparedness and sustainable tourism planning.")
        )
      ),
      
      layout_sidebar(
        sidebar = sidebar(
          title = "Tourism Controls",
          radioButtons(
            "tourism_type",
            "Arrival Type:",
            choices = c("Both" = "both", "Air" = "air", "Cruise" = "cruise"),
            selected = "both"
          ),
          sliderInput(
            "tourism_years",
            "Year Range:",
            min = 1995,
            max = 2024,
            value = c(1995, 2024),
            step = 1,
            sep = ""
          ),
          checkboxInput(
            "show_hurricanes",
            "Highlight Hurricane Years",
            value = FALSE
          ),
          hr(),
          downloadButton("download_tourism", "Download Data")
        ),
        
        navset_card_tab(
          full_screen = TRUE,
          
          nav_panel(
            "Trend Over Time",
            plotlyOutput("tourism_trend", height = "600px")
          ),
          
          nav_panel(
            "By Island",
            plotlyOutput("tourism_island", height = "600px")
          ),
          
          nav_panel(
            "2017 Impact",
            plotlyOutput("tourism_2017", height = "600px")
          ),
          
          nav_panel(
            "Recovery Analysis",
            gt_output("tourism_recovery_table")
          )
        )
      )
    )
  ),
  
  # Weather Tab
  nav_panel(
    title = "Weather & Climate",
    icon = bs_icon("thermometer-half"),
    
    div(
      style = "padding: 20px;",
      
      card(
        class = "mb-3",
        card_header("Weather & Climate in the U.S. Virgin Islands"),
        card_body(
          p("The U.S. Virgin Islands experience a tropical maritime climate characterized by warm temperatures year-round, 
            steady trade winds, and distinct wet and dry seasons. Average temperatures vary little across the year, while 
            precipitation patterns show stronger seasonal and interannual variability."),
          p("Rainfall is influenced by large-scale climate processes as well as local island geography, resulting in 
            differences across islands and between years. Periods of elevated precipitation often coincide with active 
            hurricane seasons, highlighting the close relationship between climate variability and extreme weather."),
          p("Understanding long-term climate trends and island-level differences is essential for water resource management, 
            infrastructure planning, and resilience to climate-related hazards. The visualizations below explore temperature 
            and precipitation patterns over time, across seasons, and by island, with hurricane years highlighted where relevant.")
        )
      ),
      
      layout_sidebar(
        sidebar = sidebar(
          title = "Weather Controls",
          selectInput(
            "weather_metric",
            "Metric:",
            choices = c("Temperature" = "temp", "Precipitation" = "precip")
          ),
          sliderInput(
            "weather_years",
            "Year Range:",
            min = 1975,
            max = 2025,
            value = c(1975, 2025),
            step = 1,
            sep = ""
          ),
          checkboxInput(
            "show_hurricane_weather",
            "Highlight Hurricane Years",
            value = TRUE
          )
        ),
        
        navset_card_tab(
          full_screen = TRUE,
          
          nav_panel(
            "Annual Trends",
            plotlyOutput("weather_annual", height = "600px")
          ),
          
          nav_panel(
            "Monthly Patterns",
            plotlyOutput("weather_monthly", height = "600px")
          ),
          
          nav_panel(
            "By Island",
            plotlyOutput("weather_island", height = "600px")
          )
        )
      )
    )
  ),
  
  # Hurricanes Tab
  nav_panel(
    title = "Hurricanes",
    icon = bs_icon("cloud-lightning"),
    
    div(
      style = "padding: 20px;",
      
      card(
        class = "mb-3",
        card_header("Hurricanes and Extreme Weather in the U.S. Virgin Islands"),
        card_body(
          p("The U.S. Virgin Islands are located within the Atlantic hurricane belt and are periodically impacted 
            by tropical storms and hurricanes of varying intensity. These events pose significant risks to infrastructure, 
            ecosystems, and the local economy, particularly tourism and public services."),
          p("While many storms pass near the territory without direct landfall, even distant hurricanes can produce damaging 
            winds, storm surge, and heavy rainfall. Major hurricanes, such as Hurricanes Hugo (1989), Marilyn (1995), and Irma 
            and Maria (2017), have caused widespread disruption and long-term recovery challenges."),
          p("The visualizations in this section examine hurricane activity affecting the USVI across time, including changes 
            in frequency, intensity, and seasonal patterns. Understanding historical hurricane trends provides critical context 
            for disaster preparedness, risk mitigation, and climate resilience planning.")
        )
      ), 
      
      layout_sidebar(
        sidebar = sidebar(
          title = "Hurricane Filters",
          sliderInput(
            "hurricane_years",
            "Year Range:",
            min = 1989,
            max = 2019,
            value = c(1989, 2019),
            step = 1,
            sep = ""
          ),
          checkboxGroupInput(
            "hurricane_categories",
            "Categories:",
            choices = 1:5,
            selected = 1:5
          ),
          hr(),
          p(strong("Total Hurricanes:"), textOutput("n_hurricanes", inline = TRUE))
        ),
        
        navset_card_tab(
          full_screen = TRUE,
          
          nav_panel(
            "Timeline",
            plotlyOutput("hurricane_timeline", height = "600px")
          ),
          
          nav_panel(
            "By Decade",
            plotlyOutput("hurricane_decade", height = "600px")
          ),
          
          nav_panel(
            "Category Distribution",
            plotlyOutput("hurricane_categories", height = "600px")
          ),
          
          nav_panel(
            "Table",
            gt_output("hurricane_table")
          )
        )
      )
    )
  ),
  
  # Data Explorer Tab
  nav_panel(
    title = "Data Explorer",
    icon = bs_icon("table"),
    
    div(
      style = "padding: 20px;",
      
      layout_sidebar(
        sidebar = sidebar(
          title = "Dataset Selection",
          radioButtons(
            "dataset_choice",
            "Choose Dataset:",
            choices = c(
              "Tourism Arrivals" = "tourism",
              "Hurricanes" = "hurricanes",
              "Weather Data" = "weather",
              "Demographics" = "demographics"
            )
          ),
          hr(),
          downloadButton("download_dataset", "Download Selected Data")
        ),
        
        card(
          full_screen = TRUE,
          card_header("Data Table"),
          DT::dataTableOutput("data_table")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  #Interactive Map
  output$overview_map <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) %>%
      leaflet::addProviderTiles(input$mapStyle) %>%
      leaflet::setView(lng = -64.8, lat = 18.1, zoom = 8) %>%
      leaflet::addMarkers(
        lng = c(-64.9, -64.86, -64.75),
        lat = c(18.33, 17.74, 18.33),
        popup = c(
          "<strong>St. Thomas</strong><br>Capital: Charlotte Amalie",
          "<strong>St. Croix</strong><br>Largest island",
          "<strong>St. John</strong><br>National Park"
        ),
        label = c("St. Thomas", "St. Croix", "St. John")
      )
  })
  
  observeEvent(input$mapStyle, {
    leaflet::leafletProxy("overview_map") %>%
      leaflet::clearTiles() %>%
      leaflet::addProviderTiles(input$mapStyle)
  })
  
  
  # Demographics
  output$demo_plot_ui <- renderUI({
    if (input$demo_viz_type == "age_structure") {
      plotOutput("demo_plot_static", height = "600px")
    } else {
      plotlyOutput("demo_plotly", height = "600px")
    }
  })
  
  output$demo_plotly <- renderPlotly({
    req(input$demo_viz_type)
    
    # ---- Ethnicity (unchanged) ----
    if (input$demo_viz_type == "ethnicity") {
      
      p <- ethnicity %>%
        filter(category != "Total Population") %>%
        mutate(category = forcats::fct_reorder(category, population)) %>%
        ggplot(aes(
          x = category, y = population, fill = category,
          text = paste0(category, "<br>",
                        scales::comma(population), " (", percent, "%)")
        )) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_brewer(palette = "Set3") +
        labs(title = "USVI Population by Race/Ethnicity (2020)",
             x = NULL, y = "Population") +
        theme_minimal()
      
      return(ggplotly(p, tooltip = "text"))
    }
    
    # ---- Total population over time (unchanged) ----
    if (input$demo_viz_type == "pop_time") {
      
      p <- population %>%
        filter(`Series.Name` == "Population, total") %>%
        mutate(year = as.integer(year),
               value = as.numeric(value)) %>%
        arrange(year) %>%
        ggplot(aes(
          x = year, y = value, group = 1,
          text = paste0("Year: ", year, "<br>",
                        "Population: ", scales::comma(round(value)))
        )) +
        geom_line(color = "steelblue", linewidth = 1.2) +
        geom_point(color = "steelblue", size = 2) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "USVI Total Population Over Time",
             x = "Year", y = "Population") +
        theme_minimal()
      
      return(ggplotly(p, tooltip = "text"))
    }
    
    # ---- NEW: Population growth rate ----
    if (input$demo_viz_type == "pop_growth") {
      
      p <- population %>%
        filter(`Series.Name` == "Population growth (annual %)") %>%
        mutate(year = as.integer(year),
               value = as.numeric(value)) %>%
        arrange(year) %>%
        ggplot(aes(
          x = year, y = value, group = 1,
          text = paste0("Year: ", year, "<br>",
                        "Growth rate: ", round(value, 2), "%")
        )) +
        geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
        geom_line(color = "darkgreen", linewidth = 1.2) +
        geom_point(color = "darkgreen", size = 2) +
        labs(title = "USVI Annual Population Growth Rate",
             x = "Year", y = "Growth Rate (%)") +
        theme_minimal()
      
      return(ggplotly(p, tooltip = "text"))
    }
    
    plotly::plotly_empty()
    
  })
  
  
  # static Age structure graph
  output$demo_plot_static <- renderPlot({
    req(input$demo_viz_type == "age_structure")
    
    population %>%
      filter(`Series.Name` %in% c(
        "Population ages 0-14 (% of total population)",
        "Population ages 15-64 (% of total population)",
        "Population ages 65 and above (% of total population)"
      )) %>%
      mutate(
        year = as.integer(year),
        value = as.numeric(value),
        age_group = case_when(
          `Series.Name` == "Population ages 0-14 (% of total population)" ~ "Ages 0–14",
          `Series.Name` == "Population ages 15-64 (% of total population)" ~ "Ages 15–64",
          `Series.Name` == "Population ages 65 and above (% of total population)" ~ "Ages 65+"
        )
      ) %>%
      ggplot(aes(x = year, y = value, fill = age_group)) +
      geom_area(alpha = 0.7) +
      scale_fill_brewer(palette = "Set2", name = "Age Group") +
      labs(title = "USVI Population Age Structure Over Time",
           x = "Year", y = "Percent of Total Population") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Demographics Table
  output$demo_table <- render_gt({
    ethnicity %>%
      arrange(desc(population)) %>%
      mutate(
        population = scales::comma(population),
        percent = paste0(percent, "%")
      ) %>%
      gt() %>%
      tab_header(title = "USVI Demographics (2020 Census)") %>%
      cols_label(
        category = "Race/Ethnicity",
        population = "Population",
        percent = "Percent"
      )
  })
  
  # Filtered hurricanes
  filtered_hurricanes <- reactive({
    hurricanes %>%
      filter(
        Year >= input$hurricane_years[1],
        Year <= input$hurricane_years[2],
        Category %in% as.numeric(input$hurricane_categories)
      )
  })
  
  output$n_hurricanes <- renderText({
    nrow(filtered_hurricanes())
  })
  
  # Hurricane Timeline
  output$hurricane_timeline <- renderPlotly({
    p <- filtered_hurricanes() %>%
      ggplot(aes(x = landfall_date, y = Category, color = factor(Category),
                 text = paste0(`Hurricane name`, "<br>",
                               "Date: ", format(landfall_date, "%b %d, %Y"), "<br>",
                               "Category: ", Category))) +
      geom_point(size = 4, position = position_jitter(width = 0, height = 0.15)) +
      scale_color_manual(
        values = c("1" = "#fec44f", "2" = "#fe9929", "3" = "#ec7014", 
                   "4" = "#cc4c02", "5" = "#8c2d04"),
        name = "Category"
      ) +
      scale_y_continuous(breaks = 1:5) +
      labs(title = "Hurricane Timeline",
           x = "Landfall Date", y = "Saffir-Simpson Category") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Hurricane by Decade
  output$hurricane_decade <- renderPlotly({
    p <- filtered_hurricanes() %>%
      mutate(decade = paste0(floor(Year / 10) * 10, "s")) %>%
      count(decade, Category) %>%
      ggplot(aes(x = decade, y = n, fill = factor(Category),
                 text = paste0("Decade: ", decade, "<br>",
                               "Category ", Category, ": ", n))) +
      geom_col() +
      scale_fill_manual(
        values = c("1" = "#fec44f", "2" = "#fe9929", "3" = "#ec7014", 
                   "4" = "#cc4c02", "5" = "#8c2d04"),
        name = "Category"
      ) +
      labs(title = "Hurricanes by Decade and Category",
           x = "Decade", y = "Number of Hurricanes") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Hurricane Categories
  output$hurricane_categories <- renderPlotly({
    p <- filtered_hurricanes() %>%
      count(Category) %>%
      ggplot(aes(x = factor(Category), y = n, fill = factor(Category),
                 text = paste0("Category ", Category, "<br>Count: ", n))) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(
        values = c("1" = "#fec44f", "2" = "#fe9929", "3" = "#ec7014", 
                   "4" = "#cc4c02", "5" = "#8c2d04")
      ) +
      labs(title = "Hurricane Category Distribution",
           x = "Saffir-Simpson Category", y = "Number of Hurricanes") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Hurricane Table
  output$hurricane_table <- render_gt({
    filtered_hurricanes() %>%
      arrange(desc(Category), Year) %>%
      select(Year, `Hurricane name`, Category, landfall_date) %>%
      gt() %>%
      tab_header(title = "Hurricanes Affecting USVI") %>%
      data_color(
        columns = Category,
        colors = scales::col_numeric(
          palette = c("#fec44f", "#fe9929", "#ec7014", "#cc4c02", "#8c2d04"),
          domain = c(1, 5)
        )
      )
  })
  
  # Tourism data filtered
  tourism_filtered <- reactive({
    data <- tourism %>%
      filter(year >= input$tourism_years[1],
             year <= input$tourism_years[2])
    
    if (input$tourism_type != "both") {
      data <- data %>% filter(arrival_type == input$tourism_type)
    }
    
    data
  })
  
  # Tourism Trend
  output$tourism_trend <- renderPlotly({
    data <- tourism_filtered() %>%
      group_by(year, arrival_type) %>%
      summarise(total = sum(usvi_total, na.rm = TRUE), .groups = "drop")
    
    if (input$show_hurricanes) {
      data <- data %>% left_join(hurricane_years, by = "year")
    }
    
    p <- data %>%
      ggplot(aes(x = year, y = total, color = arrival_type, group = arrival_type,
                 text = paste0("Year: ", year, "<br>",
                               "Type: ", tools::toTitleCase(arrival_type), "<br>",
                               "Arrivals: ", scales::comma(total)))) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c("air" = "#1f78b4", "cruise" = "#33a02c"),
        labels = c("Air Arrivals", "Cruise Passengers"),
        name = "Arrival Type"
      ) +
      labs(title = "Tourism Arrivals Over Time",
           x = "Year", y = "Number of Arrivals") +
      theme_minimal()
    
    if (input$show_hurricanes && nrow(hurricane_years) > 0) {
      p <- p + geom_vline(data = hurricane_years, 
                          aes(xintercept = year),
                          linetype = "dashed", alpha = 0.3, color = "red")
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # 2017 Impact
  output$tourism_2017 <- renderPlotly({
    p <- tourism %>%
      filter(year >= 2014, year <= 2024) %>%
      group_by(year, arrival_type) %>%
      summarise(total = sum(usvi_total, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        period = case_when(
          year < 2017 ~ "Before 2017",
          year == 2017 ~ "2017 (Irma & Maria)",
          year > 2017 ~ "After 2017"
        ),
        arrival_label = ifelse(arrival_type == "air", "Air Arrivals", "Cruise Passengers")
      ) %>%
      ggplot(aes(x = year, y = total, fill = period,
                 text = paste0("Year: ", year, "<br>",
                               "Type: ", arrival_label, "<br>",
                               "Arrivals: ", scales::comma(total)))) +
      geom_col() +
      facet_wrap(~arrival_label, scales = "free_y", ncol = 1) +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(
        values = c("Before 2017" = "lightblue",
                   "2017 (Irma & Maria)" = "red",
                   "After 2017" = "lightgreen")
      ) +
      labs(title = "2017 Hurricane Impact on Tourism",
           x = "Year", y = "Arrivals", fill = "Period") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Tourism by Island
  output$tourism_island <- renderPlotly({
    p <- tourism_filtered() %>%
      select(year, month, st_thomas_st_john, st_croix, arrival_type) %>%
      pivot_longer(cols = c(st_thomas_st_john, st_croix), 
                   names_to = "island", values_to = "arrivals") %>%
      filter(!is.na(arrivals)) %>%
      group_by(year, island, arrival_type) %>%
      summarise(total = sum(arrivals, na.rm = TRUE), .groups = "drop") %>%
      mutate(island = ifelse(island == "st_thomas_st_john", "St. Thomas/St. John", "St. Croix")) %>%
      ggplot(aes(x = year, y = total, color = island, group = island, 
                 text = paste0("Year: ", year, "<br>",
                               "Island: ", island, "<br>",
                               "Arrivals: ", scales::comma(total)))) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      facet_wrap(~arrival_type, scales = "free_y", ncol = 1) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_brewer(palette = "Set1", name = "Island") +
      labs(title = "Tourism Arrivals by Island",
           x = "Year", y = "Arrivals") +
      theme_minimal()
    
    # add hurricane years (only if the check box is on)
    if (input$show_hurricanes && nrow(hurricane_years) > 0) {
      p <- p + geom_vline(
        data = hurricane_years,
        aes(xintercept = year),
        linetype = "dashed",
        alpha = 0.3,
        inherit.aes = FALSE
      )
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # Tourism Recovery Table
  output$tourism_recovery_table <- render_gt({
    tourism %>%
      filter(year %in% c(2016, 2017, 2018, 2019, 2020)) %>%
      group_by(year, arrival_type) %>%
      summarise(total = sum(usvi_total, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = arrival_type, values_from = total) %>%
      mutate(
        air_pct = (air - lag(air)) / lag(air) * 100,
        cruise_pct = (cruise - lag(cruise)) / lag(cruise) * 100
      ) %>%
      gt() %>%
      tab_header(title = "Tourism Recovery: 2016-2020") %>%
      fmt_number(columns = c(air, cruise), decimals = 0) %>%
      fmt_number(columns = c(air_pct, cruise_pct), decimals = 1) %>%
      cols_label(
        year = "Year",
        air = "Air",
        cruise = "Cruise",
        air_pct = "Air % Δ",
        cruise_pct = "Cruise % Δ"
      ) %>%
      data_color(
        columns = c(air_pct, cruise_pct),
        colors = scales::col_numeric(
          palette = c("red", "white", "green"),
          domain = c(-80, 30)
        )
      )
  })
  
  # Download Tourism Data
  output$download_tourism <- downloadHandler(
    filename = function() {
      paste0("usvi_tourism_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(tourism_filtered(), file)
    }
  )
  
  # showing hurricane lines for weather data
  hurr_lines <- function(p) {
    if (isTRUE(input$show_hurricane_weather) && nrow(hurricane_years) > 0) {
      p + geom_vline(
        data = hurricane_years,
        aes(xintercept = year),
        linetype = "dashed",
        alpha = 0.3,
        inherit.aes = FALSE,
        color = "red"
      )
    } else {
      p
    }
  }
  
  # Weather Annual
  output$weather_annual <- renderPlotly({
    if (input$weather_metric == "temp") {
      p <- weather %>%
        filter(datatype %in% c("TMAX", "TMIN"),
               year >= input$weather_years[1],
               year <= input$weather_years[2]) %>%
        group_by(year, datatype) %>%
        summarise(avg_temp = mean(value_clean, na.rm = TRUE), .groups = "drop") %>%
        mutate(datatype = ifelse(datatype == "TMAX", "Maximum", "Minimum")) %>%
        ggplot(aes(
          x = year, y = avg_temp,
          color = datatype,
          group = datatype,  # ✅ connect dots
          text = paste0("Year: ", year, "<br>",
                        "Type: ", datatype, "<br>",
                        "Temp: ", round(avg_temp, 1), "°C")
        )) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Maximum" = "#d73027", "Minimum" = "#4575b4")) +
        labs(title = "Annual Average Temperatures",
             x = "Year", y = "Temperature (°C)", color = "Type") +
        theme_minimal()
      
      p <- hurr_lines(p)   # ✅ add hurricane lines
      
      ggplotly(p, tooltip = "text")
      
      
    } else {
      data <- weather %>%
        filter(datatype == "PRCP", 
               year >= input$weather_years[1],
               year <= input$weather_years[2]) %>%
        group_by(year) %>%
        summarise(total_precip = sum(value_clean, na.rm = TRUE), .groups = "drop")
      
      if (input$show_hurricane_weather) {
        data <- data %>% left_join(hurricane_years, by = "year") %>%
          mutate(hurricane_year = !is.na(Category))
      }
      
      p <- data %>%
        ggplot(aes(x = year, y = total_precip,
                   text = paste0("Year: ", year, "<br>",
                                 "Precipitation: ", scales::comma(round(total_precip)), " mm"))) +
        geom_col(aes(fill = if(input$show_hurricane_weather) hurricane_year else NULL)) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Annual Precipitation",
             x = "Year", y = "Total Precipitation (mm)") +
        theme_minimal()
      
      if (input$show_hurricane_weather) {
        p <- p + scale_fill_manual(
          values = c("TRUE" = "#d73027", "FALSE" = "#4575b4"),
          labels = c("Normal Year", "Hurricane Year"),
          name = ""
        )
      }
      
      ggplotly(p, tooltip = "text")
    }
  })
  
  # Weather Monthly
  output$weather_monthly <- renderPlotly({
    if (input$weather_metric == "temp") {
      p <- weather %>%
        filter(datatype %in% c("TMAX", "TMIN"),
               year >= input$weather_years[1],
               year <= input$weather_years[2]) %>%
        group_by(month, datatype) %>%
        summarise(avg_temp = mean(value_clean, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          datatype = ifelse(datatype == "TMAX", "Maximum", "Minimum"),
          month_name = month.abb[month]
        ) %>%
        ggplot(aes(x = factor(month_name, levels = month.abb), 
                   y = avg_temp, fill = datatype)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("Maximum" = "#d73027", "Minimum" = "#4575b4")) +
        labs(title = "Average Monthly Temperatures",
             x = "Month", y = "Temperature (°C)", fill = "Type") +
        theme_minimal()
      
      ggplotly(p)
      
    } else {
      p <- weather %>%
        filter(datatype == "PRCP",
               year >= input$weather_years[1],
               year <= input$weather_years[2]) %>%
        group_by(month) %>%
        summarise(avg_precip = mean(value_clean, na.rm = TRUE), .groups = "drop") %>%
        mutate(month_name = month.abb[month]) %>%
        ggplot(aes(x = factor(month_name, levels = month.abb), y = avg_precip)) +
        geom_col(fill = "#4575b4") +
        labs(title = "Average Monthly Precipitation",
             x = "Month", y = "Average Precipitation (mm)") +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  # Weather by Island
  output$weather_island <- renderPlotly({
    if (input$weather_metric == "temp") {
      p <- weather %>%
        filter(datatype == "TMAX",
               year >= input$weather_years[1],
               year <= input$weather_years[2]) %>%
        group_by(year, island) %>%
        summarise(avg_temp = mean(value_clean, na.rm = TRUE), .groups = "drop") %>%
        ggplot(aes(x = year, y = avg_temp, color = island, group = island)) +
        geom_line(size = 1.2) +
        scale_color_brewer(palette = "Set1") +
        labs(title = "Average Temperature by Island",
             x = "Year", y = "Temperature (°C)", color = "Island") +
        theme_minimal()
      
      p <- hurr_lines(p)
      ggplotly(p)
      
      
    } else {
      p <- weather %>%
        filter(datatype == "PRCP",
               year >= input$weather_years[1],
               year <= input$weather_years[2]) %>%
        group_by(year, island) %>%
        summarise(total_precip = sum(value_clean, na.rm = TRUE), .groups = "drop") %>%
        ggplot(aes(x = year, y = total_precip, color = island, group = island)) +
        geom_line(size = 1.2) +
        scale_y_continuous(labels = scales::comma) +
        scale_color_brewer(palette = "Set1") +
        labs(title = "Annual Precipitation by Island",
             x = "Year", y = "Total Precipitation (mm)", color = "Island") +
        theme_minimal()
      
      p <- hurr_lines(p)
      ggplotly(p)
      
    }
  })
  
  # Data Explorer
  selected_dataset <- reactive({
    switch(input$dataset_choice,
           "tourism" = tourism,
           "hurricanes" = hurricanes,
           "weather" = weather %>% select(date, year, month, datatype, value_clean, island),
           "demographics" = ethnicity)
  })
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      selected_dataset(),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons'
    )
  })
  
  output$download_dataset <- downloadHandler(
    filename = function() {
      paste0("usvi_", input$dataset_choice, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(selected_dataset(), file)
    }
  )
}

# Run app
shinyApp(ui, server)