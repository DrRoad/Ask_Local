palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

shinyServer(function(input, output, session) {
        
        library(ggplot2)
        library(weatherData)
        library(rpart)
        library(rpart.plot)
        library(caret)
        library(rattle)
        
        source("weather_helper.R")
        source("weather.R")
        
        # If no prepared locations file exists, make it.
        if (!file.exists("airports.csv")){
                airports <- download_airports()
        } else {
                airports <- read.csv("airports.csv")
        } ## !file.exists("airports.csv")
        
        airports$iso_country <- as.character(airports$iso_country)
        airports$iso_region <- as.character(airports$iso_region)
        airports$municipality <- as.character(airports$municipality)
        airports$iso_region[is.na(airports$iso_region)] <- airports$iso_country[is.na(airports$iso_region)]
        
        
        output$select_country <- renderUI({
                selectInput('country', 'Select country', 
                            c(Choose='', sort(airports$iso_country)), 
                            selectize=TRUE)
        })
        
        airports_country <- reactive({
                airports_country <- airports[!is.na(airports$iso_country),]
                airports_country <- airports_country[airports_country$iso_country == input$country,]
                airports_country
        })
        
        output$select_region <- renderUI({
                selectInput('region', 'Select region', 
                            c(Choose='', sort(airports_country()$iso_region)), 
                            selectize=TRUE)
        })
        
        airports_region <- reactive({
                airports_country()[airports_country()$iso_region == input$region,]
        })
        
        output$select_municipality <- renderUI({
                selectInput('municipality', 'Select municipality', 
                            c(Choose='', sort(airports_region()$municipality)), 
                            selectize=TRUE)
        })
        
        reac_prediction <- eventReactive(input$update, {
                location = airports_region()$ident[airports_region()$municipality == input$municipality]
                
                weather_forecast(input$date, location, ny=20, level = 0.75,
                                               predict_temp=TRUE, predict_rain=FALSE, 
                                               make_temp_graph=TRUE)
        })
        
        output$plot <- renderPlot({
                reac_prediction()$TempPlot
        })
        
})