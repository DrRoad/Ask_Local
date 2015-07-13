source("weather.R")

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

