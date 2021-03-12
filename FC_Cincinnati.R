setwd("C:\\Users\\niken\\Desktop\\cincinatti")
library(dplyr)
#Reading the 3 files from Excel
#The win file has been used for only calculating number of wins in 2019
win = read.csv("win.csv")
#Importing the entire data of all Teams for 2019
df_2019 = read.csv("2019.csv")
#Importing the entire data of all Teams for 2020
df_2020 = read.csv("2020.csv")

#Selecting home games from 2019 and 2020 and creating its dataframe
df_2019 = df_2019[df_2019$Venue == "Nippert Stadium", ]
df_2020 = df_2020[df_2020$Venue == "Nippert Stadium", ]

#To aggregate number of wins for 2019
win_stats = data.frame(aggregate(win$Wins, by= list(win$Away), FUN ="sum"))

#Creating Classification Based on Match Day (Weekend or Weekday)
df_2019$Weekend = ifelse(((df_2019$Day == "Sat") | (df_2019$Day == "Sun")), 1, 0)
df_2020$Weekend = ifelse(((df_2020$Day == "Sat") | (df_2020$Day == "Sun")), 1, 0)

#Creating Classification Based on Match Time
df_2019$Hour = as.numeric(substr(as.character(df_2019$Time),1 , 2))
df_2020$Hour = as.numeric(substr(as.character(df_2020$Time),1 , 2))

#Count wins against each opponent in 2019 by joining on the Away column 
df_2019_filter = merge(df_2019, win_stats, by.x = "Away", by.y = "Group.1", all.x = TRUE)
df_2020_filter = merge(df_2020, win_stats, by.x = "Away", by.y = "Group.1", all.x = TRUE)

# Use 0 for Teams which did not play in 2019, but will play in 2020
df_2020_filter[is.na(df_2020_filter$x), ]$x = 0

#Convert Attendance to numeric data type
df_2019_filter$Attendance = as.numeric(gsub(",", "", as.character(df_2019_filter$Attendance)))

#Applying Linear Regression for forecasting 2020 Attendance
lm.fit = lm(Attendance~x+ Weekend+Hour, data =df_2019_filter)
lm.pred = predict(lm.fit, df_2020_filter)
#Creating a new column in df_2020_filter for storing attendance prediction
df_2020_filter$Attendance_linear = lm.pred

#Applying Random Forest for forecasting 2020 Attendance
library(randomForest)
model1 <- randomForest(Attendance~Weekend+Hour, data =df_2019_filter,iter=4)
rf.pred = predict(model1, df_2020_filter)
#Creating a new column in df_2020_filter for storing attendance prediction 
df_2020_filter$Attendance_random_forest = rf.pred

#Writing the output of both algortihms in CSV
write.csv(df_2020_filter, "2020_predict.csv", row.names = FALSE)
model1