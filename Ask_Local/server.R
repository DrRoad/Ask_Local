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
        
        output$text2 <- renderText(input$choices)
        
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
        
        observeEvent(input$update, {
                if(input$municipality %in% airports_region()$municipality ){
                location = airports_region()$ident[airports_region()$municipality == input$municipality]
                withProgress(message = 'Generating data', detail = "initializing", value = 0, {
                        if("T" %in% input$choices){predict_temp=TRUE}else{predict_temp=FALSE}
                        if("H" %in% input$choices){make_temp_graph=TRUE}else{make_temp_graph=FALSE}
                        if("P" %in% input$choices){predict_rain=TRUE}else{predict_rain=FALSE}
                        if("S" %in% input$choices){show_pairs=TRUE}else{show_pairs=FALSE}
                        if(predict_temp | make_temp_graph | predict_rain){
                                prediction <- weather_forecast(input$date, location, ny=input$num_years, 
                                                               level = 0.75,
                                                               predict_temp=predict_temp, 
                                                               predict_rain=predict_rain, 
                                                               make_temp_graph=make_temp_graph)
                                if(predict_temp){
                                        output$temp <- renderText(paste0(" Max T = ",round(prediction$MaxT[1],0),
                                                                        "F,        Min T=",round(prediction$MinT[1],0),"F"))
                                }
                                if(make_temp_graph){
                                        output$plot <- renderPlot({
                                                withProgress(message = 'Making plot', value = 0,prediction$TempPlot)
                                        })
                                        }
                                if(predict_rain){
                                        output$rain <- renderText(paste0("Will there be any precipitation? ",
                                                                        names(sort(prediction$Precipitation,decreasing = T))[1],
                                                                        " with ",max(prediction$Precipitation)*100,"% probability"))
                                }
                                if(show_pairs){
                                        output$pairs_button <- renderUI({
                                                actionButton("gen_pairs", "Show/Update pairs")
                                        })
                                }
                                if(show_pairs){
                                        observeEvent(input$gen_pairs,{
                                                library(psych)
                                                weather_data<-prediction$Data
                                                colors <- c("blue","gold","green",
                                                            "red","purple","yellow")[unclass(weather_data$Events)]
                                                output$pairs <- renderPlot({
                                                        withProgress(message = 'Making plot', value = 0,{
                                                                raw <- weather_data[,sapply(weather_data, is.numeric)]
                                                                pairs.panels(raw[,sample(3:ncol(raw),5),],bg=colors, 
                                                                             pch = 21, density=TRUE,ellipses=FALSE,rug=T,
                                                                             main=paste("Correlations and clustering for various weather events in",
                                                                                        input$municipality,"on",input$date))
                                                        })
                                                })
                                                output$table_c <- renderTable(table(data.frame(colors,weather_data$Events)))
                                        })
                                        
                                        
                                }
                                qlty <-  prediction$Quality
                                dqlty <- ifelse(qlty < 0.25,"bad",
                                                ifelse(qlty < 0.5,"acceptable",
                                                       ifelse(qlty < 0.75,"OK","good")))
                                
                                output$qual <- renderText(paste0("Data quality is ",dqlty,
                                                                 " (",qlty*100,"%)"))
                        }else{
                                output$temp <- renderText(paste0("Please, select a forecast aspect"))
                        }
                })
                }else{
                        output$temp <- renderText(paste0("Please, select a location"))
                }
        })
        
        
})