source("weather.R")
set.seed(123)

diff_df <- as.data.frame(airports[,c("ident")])
diff_df[,c("max_T","min_T","diff_max_T","diff_min_T",
           "Event_Actual","Event_Predicted","Event_Prob","Quality")] <- NA
date="2015-07-11"
sample_size = 100
### with rain
for (i in sample(1:nrow(diff_df),sample_size)){
        location=airports$ident[i]
        actual <- getWeatherForDate(location, date,opt_all_columns = T)
        Sys.sleep(1)
        predicted <- weather_forecast(date, location, ny=20, level = 0.75,
                                      predict_temp=TRUE, predict_rain=TRUE, 
                                      make_temp_graph=FALSE)
        diff_df$diff_max_T[i] <- predicted$MaxT[1] - actual$Max_TemperatureF
        diff_df$diff_min_T[i] <- predicted$MinT[1] - actual$Min_TemperatureF
        diff_df$max_T[i] <- actual$Max_TemperatureF
        diff_df$min_T[i] <- actual$Min_TemperatureF
        diff_df$Event_Predicted[i] <- names(predicted$Precipitation)[1]
        diff_df$Event_Actual[i] <- ifelse(is.na(actual$Events),"Dry",actual$Events)
        diff_df$Event_Prob[i] <- predicted$Precipitation[1]
        diff_df$Quality[i] <- predicted$Quality
        diff_df[i,]
        Sys.sleep(1)
}

sample_size = 200
### without rain
for (i in sample(1:nrow(diff_df),sample_size-sum(!is.na(diff_df$max_T)))){
        location=airports$ident[i]
        actual <- getWeatherForDate(location, date,opt_all_columns = T)
        Sys.sleep(1)
        predicted <- weather_forecast(date, location, ny=20, level = 0.75,
                                      predict_temp=TRUE, predict_rain=FALSE, 
                                      make_temp_graph=FALSE)
        if(predicted$Quality > 0.1){
                diff_df$diff_max_T[i] <- predicted$MaxT[1] - actual$Max_TemperatureF
                diff_df$diff_min_T[i] <- predicted$MinT[1] - actual$Min_TemperatureF
                diff_df$max_T[i] <- actual$Max_TemperatureF
                diff_df$min_T[i] <- actual$Min_TemperatureF
                diff_df$Quality[i] <- predicted$Quality
                diff_df[i,]
        }
        Sys.sleep(1)
}

diff_df$Event_Prob <- sapply(diff_df$Event_Prob,FUN = paste,collapse = "-")
write.csv(diff_df,"diff_df.csv")
title = paste("Prediction deviations from actual 
              temperatures for various locations (",date,")")

library(reshape2)
plot_df <- diff_df[,c(4,5,9)]
colnames(plot_df) <- c("Max T","Min T","Quality")
plot_df$Quality<-cut(plot_df$Quality, c(0.0,0.2,0.4,0.6,0.8,1.0))
png("quality.png",width=1200,height=800)
boxplot(value ~ variable*Quality,data = melt(plot_df, id.var="Quality"),
        col=(c("red","blue")),xlab="Data Quality",ylab="Deviation, F"
        )
title(title)
dev.off()

############## test2 
library(psych)
source("weather.R")
weather_data <- get_weather(the_date="2015-07-11",ny=20,location="KTUS")
weather_data$Events <- replace(weather_data$Events, 
                               grepl("rain",tolower(weather_data$Events)), "Rain")
weather_data$Events <- as.factor(replace(weather_data$Events, 
                                         weather_data$Events=="", "Dry"))
raw <- weather_data[,sapply(weather_data, is.numeric)]
colors <- c("blue","gold","green")[unclass(weather_data$Events)]
png("pairs.png",width=1600,height=1200,res = 150)
pairs.panels(raw[,c(1,3,4,12,14,15,16,19,20),],bg=colors, 
             pch = 21, density=TRUE,ellipses=FALSE,rug=T,
             main="Correlations and clustering for various weather events
             in Tucson, AZ on 2015-07-11")
dev.off()
table(data.frame(colors,weather_data$Events))