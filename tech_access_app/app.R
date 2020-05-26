# Set directory & load file names ---------------------------
dir <- "~/Documents/data_science/rshiny_tech-access/tech_access_app"
#source(file.path(dir, "configs.R"))

# Load packages ---------------------------
#source(file.path(dir, "packages.R")) 

# Load  helper functions ---------------------------
#source(file.path(dir, "helpers.R")) # helper functions for map data viz

# load packages 
library(readxl) # for reading data from Excel files 
library(tidyverse) # for data manipulation, processing & visualization
library(shiny) # shiny
library(shinydashboard) # shiny dashboard
library(forcats) # factor manipulation
library(plotly) # interactive plots 

# If running for the first time, install devtools & urbanmapr first 
#install.packages("devtools")
#devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

# set directory & file name 
#datadir <- "~/Documents/data_science/rshiny_tech-access/tech_access_app/data"
#rda_data <- "data.rda"

# Load data ---------------------------
#load(file.path(dir, "data.rda"))
#load(file.path(dir, "urbn_map_county.rda"))
#load(file.path(dir, "urbn_map_state.rda"))
load("data.rda")
load("urbn_map_county.rda")
load("urbn_map_state.rda")


# prepare data for viz
state_ratios <- data %>%
  group_by(statename) %>%
  summarize(mean_ratio = mean(ratio, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(statename != "Guam", statename != "American Samoa", statename != "Northern Mariana Isl") %>%
  mutate(statename = fct_reorder(statename, mean_ratio)) 


# Build Shiny Dashboard ---------------------------

# User interface ...................................
# Create elements with input & output 
ui <- dashboardPage(
  
  skin = "black",
  
  # Header content
  dashboardHeader(
    
  ),
  
  # Sidebar content
  dashboardSidebar(
    # Menu
    sidebarMenu(
      # 3 menu items: Summary, Map, Explorer
      # Learn: introduction to the topic, some tips on how to use the app; map of USA, demonstrating variation in coverage; highlighting effects of geography
      menuItem("Learn", tabName = "learn", icon = icon("compass")),
      # Reflect: interactive visualization for people to explore the data themselves
      menuItem("Reflect", tabName = "reflect", icon = icon("lightbulb"))
    )
  ),
  
  # Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "learn",
              fluidRow(box(width = 12, 
                           background = 'teal',
                           h1("High-Speed Internet Access in the USA"),
                           h2("This app is a simple tool to explore how high-speed internet access varies by state/province and county in the 
                              United States.")
              )
              ),
              fluidRow(box(width= 12, 
                           background = 'navy',
                           h4("Explore the map of the United States to see how high-speed internet connections vary county by county. 
                              This map is interactive: you can zoom in on specific areas and hover over the map to see each state's average 
                              ratio of residential high-speed internet connections to number of households. A ratio above 1 means that there 
                              are more high-speed connections than there are households. A ratio under 1 means that there are some households 
                              without a high-speed internet connection. Lower values mean that more households lack a high-speed internet 
                              connection.")
              )
              ),
              fluidRow(
                box(width = 12, 
                    plotlyOutput("ratiomap", width = 1200))
              ),
              fluidRow(box(width = 12,
                           background = 'yellow',
                           h4("These data are from the Federal Communications Commission (FCC) website, fcc.gov. 
                              The number of households are from the US Census (2013-2017 5-year American Community Survey). 
                              The number of residential high-speed internet connections are from the FCC form 477 as of December 31, 2017. 
                              Internet providers are required to fill out this form twice a year on where they offer Internet access service 
                              at speeds exceeding 200 kbps in at least one direction. More information and the original dataset can be found 
                              at the following URL: https://www.fcc.gov/general/form-477-county-data-internet-access-services")
              )
              )
      ),
      # Second tab content 
      tabItem(tabName = "reflect", 
              fluidPage(
                fluidRow( 
                  box(width = 12,
                      background = 'teal',
                      h2("Time to exercise your imagination..."),
                      h4("Imagine how different your life would be without high-speed internet. What would it be like to find a job or work? What would school be like for yourself or your children? How would this experience change during recent shelter-in-place orders in response to the coronavirus?")
                  )
                ),
                fluidRow(
                  box(width = 12, 
                      background = 'navy',
                      h4("Select a state to explore how counties vary in their ratios of high-speed connections to number of households. 
                         Once you select a state, you will see an interactive map appear. You can zoom in on specific areas and hover over 
                         the map to see each county's name and its ratio of residential high-speed internet connections to number of households. 
                         A ratio above 1 means that there are more high-speed connections than there are households. A ratio under 1 means that 
                         there are some households without a high-speed internet connection. Lower values mean that more households lack a 
                         high-speed internet connection.")
                  )
                ),
                # get user input for which state to plot
                fluidRow(
                  box(
                    background = 'maroon',
                    selectizeInput("select_state", 
                                   "Select a U.S. State/Territory:", 
                                   data$statename,
                                   selected = "New York")
                  ),
                  box(
                    background = 'aqua',
                    h4("For your state..."),
                    h5("Which county has the highest concentration of high-speed connections to households? Which has the lowest? 
                       Why might that be?")
                  )
                ),
                fluidRow(
                  box(
                    height = 800, 
                    plotlyOutput("stateratiomap")
                  ),
                  
                  box(
                    height = 800, 
                    DT::dataTableOutput("stateratiotable_max"))),
                fluidRow(
                  box(
                    width = 12, height = 800, background = 'yellow',
                    plotOutput("stateratiolollipop", height = 800))))
              
      )
    )
  )
  
)

# Server ...................................
# Tell the server how to assemble inputs to outputs 
server <- function(input, output) { 
  
  # interactive map with ratios by county 
  output$ratiomap <- renderPlotly({
    # define ggplot object for map of ratios by state
    gg_ratiomap <- ggplot(data = left_join(urbnmapr::get_urbn_map(map = "states", sf = TRUE), mutate(state_ratios, statename = as.character(statename)), 
                                           by = c("state_name" = "statename"))) +
      geom_sf(
        #mapping = aes(fill = ratio),
        mapping = aes(fill = mean_ratio, label = state_name),
        color = "lightskyblue", size = 0.25) +
      labs(fill = "Ratio") +
      theme_bw()
    # define ggplot object for map of ratios by county 
    #gg_ratiomap <- ggplot(data = left_join(urbnmapr::get_urbn_map(map = "counties", sf = TRUE), data, 
    #                                       by = c("county_name" = "countyname"))) +
    #  geom_sf(
    #    #mapping = aes(fill = ratio),
    #    mapping = aes(fill = ratio, label = county_name, text = state_name),
    #    color = "lightskyblue", size = 0.25) +
    #  labs(fill = "Ratio") +
    #  theme_bw()
    # show plot
    gg_ratiomap
    # create interactive map from ggplot object
    #ggplotly(gg_ratiomap)
    
    
  })
  
  # state plot 
  output$stateratiomap <- renderPlotly({
    
    ggstateratio <- ggplot(
      # join ratio data on map data & filter for selected state
      filter(left_join(urbnmapr::get_urbn_map(map = "counties", sf = TRUE), data, 
                       by = c("county_name" = "countyname")),
             state_name == input$select_state))  +
      # create map
      geom_sf(mapping = aes(fill = ratio, label = county_name),
              color = "lightskyblue", size = 0.05) +
      labs(fill = "Ratio") +
      theme_bw() 
    # make map interactive 
    plotly::ggplotly(ggstateratio, height = 750)
    
  })
  
  # create data table 
  output$stateratiotable_max <- DT::renderDataTable({
    # select columns & arrange by descending ratio 
    select(arrange(filter(data, statename == input$select_state), desc(ratio)), 
           `County Name` = countyname, 
           `# of Residential High-Speed Internet Connections` = consumer, 
           `# of Households` = hhs, 
           `Ratio of Residential High-Speed Internet Connections to Households` = ratio)
    
  }, height = 750)
  
  
  # create lollipop plot for state ratios 
  output$stateratiolollipop <- renderPlot({
    # ggplot 
    state_ratios %>%
      ggplot(aes(x=statename, y=mean_ratio)) +
      geom_segment(aes(xend=statename, yend=0), color=ifelse(state_ratios$statename == input$select_state, "orange", "grey")) +
      geom_point(color=ifelse(state_ratios$statename == input$select_state, "orange", "lightseagreen"), size=ifelse(state_ratios$statename == input$select_state, 5, 2) ) +
      
      coord_flip() +
      annotate("text", x=which(levels(state_ratios$statename) == input$select_state), y=state_ratios$mean_ratio[which(state_ratios$statename==input$select_state)]*1.02, 
               label=paste("  ", input$select_state, " is #", toString(1 + length(state_ratios$statename) - which(levels(state_ratios$statename) == input$select_state)), " \nwith mean ratio=", toString(round(state_ratios$mean_ratio[state_ratios$statename==input$select_state], digits = 2))), 
               color="orange", size=4 , angle=0, fontface="bold", hjust=0) +
      xlab("") +
      ylab("Ratio of Residential High-Speed Internet Connections to Number of Households") +
      theme(axis.text.x=element_text(size=18)) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  }, height = 750)
  
}

# Launch Dashboard ...................................
shinyApp(ui, server)


