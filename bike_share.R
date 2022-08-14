#installing required libraries
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("geosphere")
install.packages("skimr")
install.packages("mapview")


#lording libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(geosphere)
library(skimr)
library(mapview)





#lording data into bike_share
bike_share<- list.files(path="C:/Users/akhil/OneDrive/Desktop/Case Study/data/trip_data", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 




head(bike_share)
tail(bike_share)
colnames(bike_share)
dim(bike_share)
str(bike_share)
summary(bike_share)
skim_without_charts(bike_share)




#converting data into lower case
bike_share$ride_id <- tolower(bike_share$ride_id )
bike_share$rideable_type <- tolower(bike_share$rideable_type )
bike_share$start_station_name <- tolower(bike_share$start_station_name )
bike_share$start_station_id <- tolower(bike_share$start_station_id )
bike_share$end_station_name <- tolower(bike_share$end_station_name )
bike_share$end_station_id  <- tolower(bike_share$end_station_id  )
bike_share$member_casual <- tolower(bike_share$member_casual)


unique(bike_share$rideable_type)
unique(bike_share$member_casual)





#explicitly changing the data type
bike_share$start_date <- as.Date(bike_share$started_at, format = "%m/%d/%Y")
bike_share$start_time <- as.POSIXct(bike_share$started_at,format="%H:%M:%S")
bike_share$end_date <- as.Date(bike_share$ended_at, format = "%m/%d/%Y")
bike_share$end_time <-  as.POSIXct(bike_share$ended_at,format="%H:%M:%S")




#defining new Columns for weekday, month, total time and  distance rode
bike_share$day <- weekdays(as.Date(bike_share$start_date ))
bike_share$month <- months(as.Date(bike_share$start_date ))
bike_share$ride_length <-difftime(bike_share$end_time,bike_share$start_time,units = "secs")
bike_share$ride_length <- as.numeric(as.character(bike_share$ride_length))
bike_share$ride_distance <- distGeo(matrix(c(bike_share$start_lng, bike_share$start_lat), ncol=2), matrix (c(bike_share$end_lng, bike_share$end_lat), ncol=2))
bike_share$ride_distance <- bike_share$ride_distance/1000




#setting up theme for graphs
newtheme <- theme_light() +
  theme(plot.title = element_text(color = "#002949", face = 'bold', size =10),

        panel.border = element_rect(color = "#002949", size = 1),
        legend.position = "right",
        legend.text = element_text(colour="red", size=8, face="bold"),
        legend.title = element_text(colour="red", size=8, face="bold"),
        axis.title.x = element_text(colour = "#002949"),
        axis.title.y = element_text(colour = "#002949"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = '#002949'),
        axis.text.y = element_text(angle = 45, hjust = 1, color = '#002949'),
        axis.line = element_line(color = "#002949", size =1),
  )





#removing rows with with total time rode less than 1 second
bike_share <- bike_share %>%
  arrange(ride_length) 
bike_share_duplicate  <- bike_share[!(bike_share$ride_length <=0),]
bike_share_duplicate  <- bike_share_duplicate[!(bike_share_duplicate$start_lng == -73.79647698 | bike_share_duplicate$start_lat == 45.635034323),]


#removing rows with null values in end_lat and end_lng  
bike_share_duplicate<- bike_share_duplicate[-which(is.na(bike_share_duplicate$end_lat| bike_share_duplicate$end_lng)), ]


boxplot(bike_share_duplicate$ride_length,
        ylab = "bike_share_duplicate$ride_length")




#plotting a pie chart to check Distribution of ride over time
bike_share_pie1 <- bike_share_duplicate %>%
  mutate(ranges = cut(ride_length, c(0, 60, 300, 600, 3600, 7200, 14400, Inf))) %>% 
  group_by(ranges) %>% 
  summarize(number=n())

pie_lables1<- paste0(round(100 * bike_share_pie1$number / sum(bike_share_pie1$number), 1),"%")
pie(bike_share_pie1$number, labels =pie_lables1 , main = "Distribution of ride over time",
    col = rainbow(length(bike_share_pie1$number)))
legend("topright",c( "(0,60]", "(60,300]", "(300,600]", "(600,3.6e+03]", "(3.6e+03,7.2e+03]", "(7.2e+03,1.44e+04]", "(1.44e+04,Inf]"), cex = 0.8, fill = rainbow(length(bike_share_pie1$number)))
 


#filtering out the data  from 61 seconds on words in respect to ride_length  
bike_share_duplicate <- subset(bike_share_duplicate, ride_length >60)
bike_share_duplicate <- bike_share_duplicate %>%
  arrange(-ride_length) 

#plotting a pie chart to check Distribution of ride between 61 and 3600
bike_share_pie2 <- bike_share_duplicate %>%
  mutate(ranges = cut(ride_length, c(60, 300, 600, 3600, 7200, 14400, Inf))) %>% 
  group_by(ranges) %>% 
  summarize(number=n())


pie_lables2<- paste0(round(100 * bike_share_pie2$number / sum(bike_share_pie2$number), 1),"%")
pie(bike_share_pie2$number, labels = pie_lables2, main = "Distribution of ride over time after filter",
    col = rainbow(length(bike_share_pie2$number)))
legend("topright",c(  "(60,300]", "(300,600]", "(600,3.6e+03]", "(3.6e+03,7.2e+03]", "(7.2e+03,1.44e+04]", "(1.44e+04,Inf]"), cex = 0.8, fill = rainbow(length(bike_share_pie2$number)))




boxplot(bike_share_duplicate$ride_length,
        ylab = "bike_share1$ride_length")


skim_without_charts(bike_share_duplicate)


#creating a function to create pie chart
pie_chart <- function(data, grouping_var, title) {
   tab <- table(data[[deparse(substitute(grouping_var))]])
   pie_lables<- paste0(round(100 * tab / sum(tab), 1),"%")
    pie(tab, labels = pie_lables, main = title,
      col = rainbow(length(tab)))
  legend("topright", names(tab), cex = 0.8, fill = rainbow(length(tab)))
}


#piloting pie chart using user defined function
pie_chart(bike_share_duplicate, rideable_type, "Usage of different types of bike")
pie_chart(bike_share_duplicate, member_casual, "Membership vs Casual")
pie_chart(bike_share_duplicate, day, "Rides during weekdays")




#creating a function to create bar graph
bar_graph <- function(data, grouping_var, title,xtitle, ytitle){
  df <- table(data[[deparse(substitute(grouping_var))]])
  barplot(df,
          main=title,
          xlab=xtitle,
          ylab=ytitle,
          border="black",
          col=rainbow(7),
          
          
  )
}


#piloting bar graph using user defined function
bar_graph(bike_share_duplicate, member_casual, "Membership vs Casual", "Membership Type", "count")
bar_graph(bike_share_duplicate, rideable_type, "Different types of bike", "Bike Type", "count")




#creating a function to create group graph
group_bar <- function(data, gp1, gp2,  mtitle, xtitle, ytitlt, ltitle, n=1){
  df1 <- data_frame(data %>% 
    group_by({{gp1}}, {{gp2}}) %>%
    summarise(number_of_rides = n()))

  if(n==0){
    df1[[1]] <- ordered(df1[[1]],
                        levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  }
  
  ggplot(df1,
         aes(x = df1[[1]], 
             y = df1[[3]],
             fill = df1[[2]]))+   labs(title=mtitle, x =xtitle, y =ytitlt , fill = ltitle)+
    geom_bar(stat = "identity",
             position = "dodge") +geom_text(aes(label = df1[[3]]),position = position_dodge(width = 1),
                                            vjust = -0.5, size = 2) +newtheme 


}


#piloting group graph using user defined function
group_bar(bike_share_duplicate,rideable_type, member_casual,"Total number of rides  for each bike", "Bike Type", "Number of rides", "Membership Type")

group_bar(bike_share_duplicate,day,member_casual, "Total number of rides for each day", "Weekday", "Number of rides", "Membership Type",0)








#creating a function to create line graph
line_plot <- function(data, gp1, gp2, mtitle, xtitle, ytitlt, ltitle){
  df1 <- data %>% 
    group_by({{gp1}},{{gp2}} ) %>%
    summarise(number_of_rides = n())
 
    df1[[1]] <- factor(df1[[1]], levels= c("July", "August", 
                                 "September", "October", "November", "December", "January","February","March", "April","May","June"))
    
    data[order(df1[[1]]), ]
 
  
 ggplot(data=df1, aes(x= df1[[1]], y = df1[[3]], group =df1[[2]], color= df1[[2]] )) +
    geom_line()+
    geom_point()+
    labs(title=mtitle,
         x =xtitle, y =ytitlt , color = ltitle)+geom_text(aes(label = df1[[3]]),position = position_dodge(width = 1),
                                                          vjust = -0.5, size = 2) +newtheme 
  
}


#piloting line graph using user defined function
line_plot(bike_share_duplicate, month,member_casual,"Total number of rides", "Month", "Number of rides", "Membership Type")

line_plot(bike_share_duplicate, month, rideable_type,"Total number of rides", "Month", "Number of rides", "Membership Type")

line_plot(bike_share_duplicate, month, rideable_type,"Total number of rides", "Month", "Number of rides", "Bike Type")




#creating a function to create line graph for average distance rode
average_ride <- function(data, gp1, gp2, sumz1, mtitle, xtitle, ytitlt, ltitle, n=0){
  
  df1<- data %>%
    group_by({{gp1}}, {{gp2}})%>%
    summarise(mean({{sumz1}}))
  
  if (n==1) {
    df1[[2]] <- factor(df1[[2]], levels= c("July", "August", 
                                           "September", "October", "November", "December", "January","February","March", "April","May","June"))
    
    data[order(df1[[2]]), ]
  } else {
    df1[[2]]<- factor(df1[[2]], levels= c("Sunday", "Monday", 
                                          "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    data[order(df1[[2]]), ]
    
  }
  
ggplot(data=df1, aes(x= df1[[2]], y = df1[[3]], group =df1[[1]], color= df1[[1]] )) +
    geom_line()+
    geom_point()+
    labs(title=mtitle,
         x =xtitle, y =ytitlt , color = ltitle) +geom_text(aes(label = as.integer(df1[[3]])),position = position_dodge(width = 1),
                                                           vjust = -0.5, size = 2) +newtheme 
}


#piloting line graph using user defined function for average distance rode
average_ride(bike_share_duplicate,member_casual, day, ride_length, "Average time driven during weekdays by different member type", "Weekday", "Average time", "Membership Type",0)

average_ride(bike_share_duplicate,rideable_type, day, ride_length, "Average time driven during weekdays by different bike type", "Weekday", "Average time", "Bike Type",0)

average_ride(bike_share_duplicate,member_casual, month, ride_distance, "Average distance driven during weekdays by different member type", "Month", "Average time", "Membership Type",1)

average_ride(bike_share_duplicate,rideable_type, month, ride_distance, "Average distance driven during a year by different bike type", "Month", "Average time", "Bike Type",1)




average_ride1 <- function(data, gp1, gp2,gp3, sumz1, mtitle, xtitle, ytitlt, ltitle){
  
  df1<- data %>%
    group_by({{gp1}}, {{gp2}},{{gp3}})%>%
    summarise(mean({{sumz1}}))
  
  df1[[3]] <- factor(df1[[3]], levels= c("July", "August", 
                                         "September", "October", "November", "December", "January","February","March", "April","May","June"))
  data[order(df1[[3]]), ]
  
ggplot(data=df1, aes(x= df1[[3]], y = df1[[4]], group =df1[[2]], color= df1[[2]] )) +
    geom_line()+
    geom_point()+
    facet_wrap(vars(df1[[1]]))+
    labs(title=mtitle,
         x =xtitle, y =ytitlt , color = ltitle)+geom_text(aes(label = as.integer(df1[[4]])),position = position_dodge(width = 1),
                                                          vjust = -0.5, size = 2) +newtheme 


}

average_ride1(bike_share_duplicate,member_casual,rideable_type,month, ride_length, "Average bike rental duration for members and casual users ", "Month", "Average time", "Bike Type")




total_ride <- function(data, gp1, gp2,gp3, mtitle, xtitle, ytitlt, ltitle){
  
  df1<- data %>%
    group_by({{gp1}}, {{gp2}},{{gp3}})%>%
    summarise(number_of_rides = n())
  
  df1[[3]] <- factor(df1[[3]], levels= c("July", "August", 
                                         "September", "October", "November", "December", "January","February","March", "April","May","June"))
  data[order(df1[[3]]), ]
  
  ggplot(data=df1, aes(x= df1[[3]], y = df1[[4]], group =df1[[2]], color= df1[[2]] )) +
    geom_line()+
    geom_point()+
    facet_wrap(vars(df1[[1]]))+
    labs(title=mtitle,
         x =xtitle, y =ytitlt , color = ltitle)+geom_text(aes(label = as.integer(df1[[4]])),position = position_dodge(width = 1),
                                                          vjust = -0.5, size = 2) +newtheme 
  
  
}

total_ride(bike_share_duplicate, member_casual,rideable_type, month, "Total bike rental count for members and casual users ", "Month", "Average time", "Bike Type")






#filling few missing start station names
bike_share_duplicate <- bike_share_duplicate %>%
  group_by(start_lat, start_lng) %>%
  arrange(start_station_name) %>%
  fill(start_station_name) 


#creating a table to calculate total number of rides for each station as per membership type and bike type
bike_share_station <- bike_share_duplicate %>%
  group_by(member_casual,start_station_name)%>%
  summarise(number_of_rider= n())

bike_share_station1 <- bike_share_duplicate %>%
  group_by(start_station_name)%>%
  summarise(number_of_rider= n())

bike_share_station_bike <- bike_share_duplicate %>%
  group_by(start_station_name, rideable_type)%>%
  summarise(number_of_rider= n())

#creating a table with start station name and its location coordinates 
bike_share_station_location<- bike_share_duplicate %>%
  select(start_station_name, start_lat,start_lng)


#deleting duplicate values on the bases of start station names
bike_share_station_location <- bike_share_station_location[!duplicated(bike_share_station_location$start_station_name),]


#merging two tables
bike_share_station <- merge(x=bike_share_station,y=bike_share_station_location,by="start_station_name",all=TRUE)
bike_share_station <-bike_share_station[!is.na(bike_share_station$start_station_name),]


bike_share_station1<- merge(x=bike_share_station1,y=bike_share_station_location,by="start_station_name",all=TRUE)
bike_share_station1 <-bike_share_station1[!is.na(bike_share_station1$start_station_name),]


bike_share_station_bike<- merge(x=bike_share_station_bike,y=bike_share_station_location,by="start_station_name",all=TRUE)
bike_share_station_bike<- bike_share_station_bike[!is.na(bike_share_station_bike$start_station_name),]

bike_share_station <- bike_share_station%>%
  arrange(-number_of_rider,member_casual,start_station_name)
bike_share_station1 <- bike_share_station1%>%
  arrange(-number_of_rider)
bike_share_station_bike <- bike_share_station_bike%>%
  arrange(-number_of_rider)

#splitting the table into two
bike_share_station_member<- bike_share_station[bike_share_station$member_casual == "member", ]
bike_share_station_causal<- bike_share_station[bike_share_station$member_casual == "casual", ]
bike_share_station_docked_bike<- bike_share_station_bike[bike_share_station_bike$rideable_type == "docked_bike", ]
bike_share_station_classic_bike<- bike_share_station_bike[bike_share_station_bike$rideable_type == "classic_bike", ]
bike_share_station_electric_bike<- bike_share_station_bike[bike_share_station_bike$rideable_type == "electric_bike", ]




#Plotting map
bike_share_station1 %>%
  mapview(
  xcol = "start_lng", 
  ycol = "start_lat",
  cex = "number_of_rider",
  alpha = 0.9, 
  crs = 4269,
  color = "blue",
  grid = F, 
  legend = T,
  layer.name = " Start Station Locations"
)+

bike_share_station1[1:10,] %>%
  mapview(
    xcol = "start_lng", 
    ycol = "start_lat",
    zcol = "start_station_name",
    cex = "number_of_rider",
    alpha = 0.9, 
    crs = 4269,
    color = "red",
    grid = F, 
    legend = T,
    layer.name = " 50 Most Crowded Start Stations"
    
  )





bike_share_station_member %>%
  mapview(
    xcol = "start_lng", 
    ycol = "start_lat",
    cex = "number_of_rider",
    alpha = 0.9, 
    crs = 4269,
    color = "blue",
    grid = F, 
    legend = T,
    layer.name = " Annual Member Start Stations" 
  )+

bike_share_station_member[1:10,] %>%
  mapview(
    xcol = "start_lng", 
    ycol = "start_lat",
    zcol = "start_station_name",
    cex = "number_of_rider",
    alpha = 0.9, 
    crs = 4269,
    color = "red",
    grid = F, 
    legend = T,
    layer.name = " 10 most crowed annual member Start Stations" 
  )+


bike_share_station_causal %>%
  mapview(
    xcol = "start_lng", 
    ycol = "start_lat",
    cex = "number_of_rider",
    alpha = 0.9, 
    crs = 4269,
    color = "yellow",
    grid = F, 
    legend = T,
    layer.name = " Causal Member Start Stations"
  )+

bike_share_station_causal[1:10,] %>%
  mapview(
    xcol = "start_lng", 
    ycol = "start_lat",
    zcol = "start_station_name",
    cex = "number_of_rider",
    alpha = 0.9, 
    crs = 4269,
    color = "green",
    grid = F, 
    legend = T,
    layer.name = "  10 most crowed Causal Member Start Stations"
    
  )


 bike_share_station_classic_bike[1:10,]  %>%
  mapview(
    xcol = "start_lng", 
    ycol = "start_lat",
    zcol = "start_station_name",
    cex = "number_of_rider",
    alpha = 0.9, 
    crs = 4269,
    color = "blue",
    grid = F, 
    legend = T,
    layer.name = "10 most popular Start Stations for clasisic bike"
  )+
   bike_share_station_electric_bike[1:10,]  %>%
   mapview(
     xcol = "start_lng", 
     ycol = "start_lat",
     zcol = "start_station_name",
     cex = "number_of_rider",
     alpha = 0.9, 
     crs = 4269,
     color = "red",
     grid = F, 
     legend = T,
     layer.name = "10 most popular Start Stations for electric bike"
   )+
   bike_share_station_docked_bike[1:10,]  %>%
   mapview(
     xcol = "start_lng", 
     ycol = "start_lat",
     zcol = "start_station_name",
     cex = "number_of_rider",
     alpha = 0.9, 
     crs = 4269,
     color = "green",
     grid = F, 
     legend = T,
     layer.name = "10 most popular Start Stations for docked bike"
   )
 
 


 
 
 






