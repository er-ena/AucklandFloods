### Load the data and packages ###
library(tidyverse)
library(sf)
library(shiny)
library(bslib)
library(leaflet)

data_for_app <- read_csv("data_for_app.csv")
auckland_suburb_polygons <- readRDS("auckland_suburb_polygons.rds")


# User Interface 

ui <- fluidPage(
  
  # App Title!
  titlePanel("Affected homes - by number"),
  
  fluidRow(
    
    #Left-hand side panel
    column(
      width = 3,
      h4("The property risk assessment outcome of Auckland homes affected by the 2023 weather events - by suburb"),
      div(style = "margin-top:20px;"),
      selectInput(
        inputId = "category_choice",
        label = "Property assessment outcome:",
        choices = c("Overview" = "overview",
                    "Category 1" = "cat1",
                    "Category 2 (2C & 2P)" = "cat2",
                    "Category 3" = "cat3"),
        selected = "overview"
      ),
      
      conditionalPanel(
        condition = "input.category_choice == 'cat1'",
        h4("Category 1"),
        p("The property has no intorelable risk to life."),
        p("However, this does not mean that the property will never be impacted by future severe weather events or that there is 'no risk'.")
      ),
      
      conditionalPanel(
        condition = "input.category_choice == 'cat2'",
        h4("Category 2C"),
        p("The property poses intorelable risk to life that will be reduced by community mitigation project."),
        h4("Category 2P"),
        p("The property poses intorelable risk to life that will be reduced by property mitigation."),
      ),
      
      conditionalPanel(
        condition = "input.category_choice == 'cat3'",
        h4("Category 3"),
        p("The property poses intorelable risk to life that cannot be reasonably mitigated."),
        p("They are eligible for a buy-out.")
      ),
      
      conditionalPanel(
        condition = "input.category_choice == 'overview'",
        p("This interactive map shows the categorisation of homes that were 
        severly affected by the Auckland Anniversary Weekend Floods and Cyclone Gabrielle in early 2023."),
        
        p("Note: This was an opt-in/voluntary scheme. Not all homes that were affected in the 2023 weather events are included in this map.")
      )
    ),
    
    
    
    #Right-hand side panel
    column(
      width = 9,
      
      # Map on the app
      leafletOutput("map", height = "600px"),
      
      #Panel for Histogram
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 60,
                    width = 330, height = 200,
                    
                    conditionalPanel(
                      condition = "input.category_choice == 'cat1'",
                      plotOutput("category_1", height = 200)
                    ),
                    
                    conditionalPanel(
                      condition = "input.category_choice == 'cat2'",
                      plotOutput("category_2", height = 200)
                    ),
                    
                    conditionalPanel(
                      condition = "input.category_choice == 'cat3'",
                      plotOutput("category_3", height = 200)
                    ),
                    
                    conditionalPanel(
                      condition = "input.category_choice == 'overview'",
                      plotOutput("overview", height = 200)
                    )
                    
      )
    )
  )
)



server <- function(input, output) {
  
  # Map creation
  output$map <- renderLeaflet({
    leaflet(data_for_app) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng=174.75, lat=-36.87, zoom=10.3) |>
      addLegend(position = "bottomright",
                title = "Total number of houses assessed",
                colors = c("red", "blue"),
                labels = c("≥ 100 houses", "< 100 houses"),)
  })
  
  
  filtered_data <- reactive({
    df <- data_for_app
    
    if (input$category_choice == "cat1") {
      df <- df[df$`Cat 1` > 0, ]
    } else if (input$category_choice == "cat2") {
      df <- df[(df$`Cat 2C` > 0 | df$`Cat 2P` > 0), ]
    } else if (input$category_choice == "cat3") {
      df <- df[df$`Cat 3` > 0, ]
    } else if (input$category_choice == "overview") {
      df 
    } 
  })
  
  observe({
    leafletProxy("map", data = filtered_data()) |>
      clearMarkers() |>
      addCircleMarkers(
        lat = ~lat, lng = ~lng,
        radius = 5,
        layerId = ~Suburb,  
        color = if(input$category_choice == "overview") {
          if_else(data_for_app$`Total Final Category` >= 100, "red", "blue")
        } else {
          ~if_else(Suburb %in% c("Epsom", "Mount Eden", "Mount Roskill", "Titirangi",
                                 "Mangere", "Henderson", "Milford", "Muriwai", "Piha"), "red", "blue")
        },
        popup = if(input$category_choice == "overview") {
          ~paste(
            "<span style='font-weight:bold; font-size:14px;'>", Suburb, "</span>",
            "<br><span style='font-weight:bold; font-size:14px;'>Total:", `Total Final Category`, "</span>",
            "<br><br>Category 1:", `Cat 1`,
            "<br>Category 2C:", `Cat 2C`,
            "<br>Category 2P:", `Cat 2P`,
            "<br>Category 3:", `Cat 3`
          )
        } else if(input$category_choice == "cat1") {
          ~paste(
            "<span style='font-weight:bold; font-size:14px;'>", Suburb, "</span>",
            "<br><span style='font-weight:bold; font-size:14px;'>Total:", `Total Final Category`, "</span>",
            "<br><br><span style='font-weight:bold;'>Category 1:", `Cat 1`, "</span>",
            "<br>Category 2C:", `Cat 2C`,
            "<br>Category 2P:", `Cat 2P`,
            "<br>Category 3:", `Cat 3`
          )
        }  else if(input$category_choice == "cat2") {
          ~paste(
            "<span style='font-weight:bold; font-size:14px;'>", Suburb, "</span>",
            "<br><span style='font-weight:bold; font-size:14px;'>Total:", `Total Final Category`, "</span>",
            "<br><br>Category 1:", `Cat 1`, 
            "<br><span style='font-weight:bold;'>Category 2C:", `Cat 2C`, "</span>",
            "<br><span style='font-weight:bold;'>Category 2P:", `Cat 2P`, "</span>",
            "<br>Category 3:", `Cat 3`
          )
        } else if(input$category_choice == "cat3") {
          ~paste(
            "<span style='font-weight:bold; font-size:14px;'>", Suburb, "</span>",
            "<br><span style='font-weight:bold; font-size:14px;'>Total:", `Total Final Category`, "</span>",
            "<br><br>Category 1:", `Cat 1`, 
            "<br>Category 2C:", `Cat 2C`, 
            "<br>Category 2P:", `Cat 2P`, 
            "<br><span style='font-weight:bold;'>Category 3:", `Cat 3`, "</span>"
          )
        }
        
      )
  })
  
  clicked_suburb <- reactive({
    req(input$map_marker_click)
    input$map_marker_click$id  
  })
  
  observe({
    req(clicked_suburb())  # only run if a marker is clicked
    
    selected_poly <- auckland_suburb_polygons[
      tolower(auckland_suburb_polygons$Suburb) == tolower(clicked_suburb()) |
        tolower(auckland_suburb_polygons$additional_name) == tolower(clicked_suburb()),
    ]
    
    if (nrow(selected_poly) == 0) return()
    
    selected_poly <- selected_poly |>
      distinct(Suburb, .keep_all = TRUE)
    
    # Polygon on to the map
    leafletProxy("map") |>
      clearGroup("selected-suburb") |>
      addPolygons(
        data = selected_poly,
        group = "selected-suburb",
        fillColor = "red",
        fillOpacity = 0.2,
        color = "darkred",
        weight = 2,
        popup = ~Suburb
      )
  })
  
  
  # To deselect? the boundary when changing the category
  observeEvent(input$category_choice, {
    leafletProxy("map") |>
      clearGroup("selected-suburb")
  })
  
  # To deselect? the boundary when clicking outside the boundary
  observeEvent(input$map_click, {
    leafletProxy("map") |>
      clearGroup("selected-suburb")
  })
  
  
  # The histogram
  # The code is from the superzip example 
  # https://github.com/rstudio/shiny-examples/blob/main/063-superzip-example/server.R
  suburbInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(data_for_app[FALSE,])
    
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(data_for_app,
           lat >= latRng[1] & lat<= latRng[2] &
             lng >= lngRng[1] & lng <= lngRng[2])
  })
  
  # Histogram for the overview page
  output$overview <- renderPlot({
    
    if (nrow(suburbInBounds()) == 0)
      return(NULL)
    
    hist(suburbInBounds()$`Total Final Category`,
         breaks = 20,
         main = "Visible suburbs",
         xlab = "Number of houses",
         xlim = range(data_for_app$`Total Final Category`),
         ylab = "Number of suburbs",
         col = 'red',
         border = 'white')
    
  })
  
  # Histogram for the category 1 page
  output$category_1 <- renderPlot({
    
    if (nrow(suburbInBounds()) == 0)
      return(NULL)
    
    hist(suburbInBounds()$`Cat 1`,
         breaks = 20,
         main = "Visible suburbs",
         xlab = "Number of houses",
         xlim = range(data_for_app$`Cat 1`),
         ylab = "Number of suburbs",
         col = 'red',
         border = 'white')
    
  })
  
  # Histogram for the category 2 page
  output$category_2 <- renderPlot({
    
    if (nrow(suburbInBounds()) == 0)
      return(NULL)
    
    hist(suburbInBounds()$`Cat 2P`,
         breaks = 20,
         main = "Visible suburbs",
         xlab = "Number of houses",
         xlim = range(data_for_app$`Cat 2P`),
         ylab = "Number of suburbs",
         col = 'red',
         border = 'white')
    
  })
  
  # Histogram for the category 3 page
  output$category_3 <- renderPlot({
    
    if (nrow(suburbInBounds()) == 0)
      return(NULL)
    
    hist(suburbInBounds()$`Cat 3`,
         breaks = 20,
         main = "Visible suburbs ",
         xlab = "Number of houses",
         xlim = range(data_for_app$`Cat 3`),
         ylab = "Number of suburbs",
         col = 'red',
         border = 'white')
    
  })
  
}


shinyApp(ui, server)
