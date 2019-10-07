#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#######################--- 1. LIBRARIES------################################

library(data.table)
library(cluster)
library(dummies)
library(tidyverse)
library(leaflet)
library(shiny)


set.seed(100)

#######################--- 2. FILES------################################

### Principal File paths
# filepath_esther <- "/Users/Esther/Desktop/IE/Courses/Programming R/Project/group project/solar_dataet.RData"

# filepath_solar <-"C:/Users/Eduardo/Desktop/IE/PROGRAMMING IN R/group assignment - solar system/solar_dataset.RData"
# stations_file_path <-"C:/Users/Eduardo/Desktop/IE/PROGRAMMING IN R/group assignment - solar system/station_info.csv"

## Anup: 

filepath_solar <- "C:/Users/anups/Desktop/solar_dataset.RData"

## Anup 
stations_file_path <- "C:/Users/anups/Desktop/station_info.csv"


#######################--- 3. PROCESSING-----################################


#Reading the table given for the project
dataset<-readRDS(filepath_solar)
anex<-read.csv(stations_file_path,sep = ",")

### Dataset with hisotrical production data (Predictor)

stations<-dataset[1:5113,1:99]

### Dataset with parsed date values 

stations$Date <- as.Date(stations$Date, format = "%Y%m%d")

### Dataset with the explnatory variables

conditions<-dataset[,100:ncol(dataset)]

## summary statistics for only the integer columns i.e without date 
stations$mean_prod <- rowMeans(stations[,2:ncol(stations)])

stations_summary <- na.omit(stations[,-c("Date")][,list(stations = colnames(stations[,2:99]), 
                                                        mean = sapply(.SD, mean), 
                                                        median = sapply(.SD, median),
                                                        min = sapply(.SD, min),
                                                        max = sapply(.SD, max),
                                                        std = sapply(.SD,sd))])

################ note: dummy the date and calculate coefficients for each month

bind_summary <- cbind(stations_summary, anex)
cluster_dataset <- bind_summary[,-c("stid")]

######## Clustering ---------##########################

set.seed(100)

## standarization of the mean production

cluster_dataset$mean_stan <- cluster_dataset[,list(mean_stan = ((mean - mean(cluster_dataset$mean))/sd(cluster_dataset$mean)))]
cluster_dataset$std_stan <- cluster_dataset[,list(std_stan = ((mean - mean(cluster_dataset$std))/sd(cluster_dataset$std)))]

## clustering based on stanarised production

## can remove one of the variables

clustering <- kmeans(x = cluster_dataset[, c("mean_stan", "std_stan")], centers = 10);
table(clustering$cluster)
cluster_dataset$cluster <- as.numeric(clustering$cluster)

### calculating the silhoutte_score and ideal number of clusters

silhouette_score <- function(k){
  km <- kmeans(x = cluster_dataset[, c("mean_stan", "std_stan")], centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(cluster_dataset[, c("mean_stan", "std_stan")]))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)


#######################--- 4. SHINY UI-----################################



# Define UI for application that draws a histogram
ui <- fluidPage(
   
  
  titlePanel("Interactive Storytelling for the Solar Dataset"),
  
  h2("1. First explore production cyclicality by choosing a Date Range, it will give you the average production"),
  h2("2. Then choose the number of clusters you want. Explore how many clusters separate the stations in an ideal manner (0-10)"),
  h2("3. For your chosen cluster, you will be shown a data table with some production statistics"),
  h2("4. To ease your choice, a silhoutte chart is provided. It seems that 10 clusters have a good score and provide good separation of clusters in terms of production statistics"),
  h2("5. You can now explore where the stations in the 10 clusters/or any of the stations in the dataset are located so that you can see whether the cluster separation also holds in terms of location"),

## Open sidebar panel so that inputs can be chosen
  
  sidebarPanel(
    
    
# Set input for first plot
    
    dateInput('date',
              label = 'Start Date: yyyy-mm-dd',
              min = '1994-01-01', max = '2007-12-31',
              value = '2000-01-01'
    ),

    dateInput('date2',
              label = 'Start Date: yyyy-mm-dd',
              min = '1994-01-01', max = '2007-12-31',
              value = '2001-01-01'
    ),
  
# Set input for second plot
  
  numericInput('clusters', 'Cluster count', 5,
               min = 2, max = 10),
  
    
# the DT table has to inputs 

# Set input for final plot

  selectInput("Stations", "Select station(s)", choices = cluster_dataset[,stations], multiple = TRUE),
  selectInput("Cluster", "Cluster Group", choices = cluster_dataset[, cluster], multiple = TRUE)
  
  ),

  
# Set all the plot/dt outputs

  mainPanel(
    plotOutput("plot"),
    plotOutput("plot1"),
    DT::dataTableOutput("Cluster_DT"),
    plotOutput("plot2"),
    leafletOutput("map")
    
  )
  
)


#######################--- 4. SHINY SERVER-----################################



# Define server logic required to draw a histogram
server <- function(input, output) {
   
# Date plot output 
  
  output$plot <- renderPlot({
    plot(stations[Date > input$date & Date < input$date2,c("Date","mean_prod")])
  })
  
#K-means output 
  
  clusters <- reactive({
    kmeans(x = cluster_dataset[, c("mean_stan")], centers = input$clusters)
  })
  
  output$plot1 <- renderPlot({
    
    plot(x = clusters()$cluster, y = clusters()$mean, ylab='Number of clusters', xlab='Mean Production',
         col = clusters()$cluster)
  })

# DT output   
  
  compute_data <- reactive({
    dat <- cluster_dataset[,list(cluster_mean = mean(mean), cluster_median = median(median), cluster_min = min(min), cluster_max = max(max)), 
                           keyby = clusters()$cluster]
    return(dat)
  })

  output$Cluster_DT = DT::renderDataTable({
    
      data.table(compute_data())
    
  })
  
# Kmeans silhoutte output 
  
  output$plot2<- renderPlot({
    
    plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
    
  })
  
  
  
# Stations Output   
  
  compute_data1 <- reactive({
    dat <- data.table(cluster_dataset[stations %in% input$Stations | cluster %in% input$Cluster,])
    return(dat)
  })
  
  
  output$map<- renderLeaflet({
    leaflet(data = compute_data1()) %>% 
      addTiles () %>%
      addMarkers(~elon, ~nlat, popup = ~as.character(cluster_dataset$stations))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

