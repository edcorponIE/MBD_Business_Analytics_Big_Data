#_____________________________________________________________________________________________________________________

#######################--- 1. LIBRARIES------################################
#_____________________________________________________________________________________________________________________

## install.packages("outliers")
# install.packages("leaflet")
## install.packages("dummies")
##install.packages("ggthemes")

library(data.table)
library(tidyverse)
library(outliers)
library(cluster)
library(dummies)
library(caret)
library(leaflet)
library(broom)
library(ggthemes)


set.seed(100)

#_____________________________________________________________________________________________________________________

#######################--- 2. FILE-READING------################################
#_____________________________________________________________________________________________________________________


#######Files paths from different computers

### Principal File paths-----------------------------------------

##Anup 
filepath_solar <- "C:/Users/anups/Desktop/solar_dataset.RData"
##Juliana# 
filepath_solar <-"/Volumes/GoogleDrive/My Drive/Group Assignment/R/Rsolar_assig/solar_dataset.RData"

### Stations.csv path---------------------------------------------

##Juliana 
stations_file_path<-"/Volumes/GoogleDrive/My Drive/Group Assignment/R/Rsolar_assig/station_info.csv"
## Anup 
stations_file_path <- "C:/Users/anups/Desktop/station_info.csv"

#Reading the table given for the project
main_dataset<-readRDS(filepath_solar)
main_dataset$Date <- as.Date(main_dataset$Date, format = "%Y%m%d")
anex<-read.csv(stations_file_path,sep = ",")

##Checking the structure 
str(main_dataset)
colnames(main_dataset)
dim(main_dataset)

#_____________________________________________________________________________________________________________________

##########################------3.TABLES CREATION-------------############### 
#_____________________________________________________________________________________________________________________
### Missing values

sapply(main_dataset, function(x){sum(is.na(x))});

#Creation of sub-matrix

### Dataset with hisotrical production data (Predictor)


stations<-main_dataset[1:5113,1:99]
dim(stations)
head(stations)
tail(stations)

### Dataset with the explnatory variables

conditions<-main_dataset[,100:ncol(main_dataset)]
dim(conditions)
head(conditions)


#_____________________________________________________________________________________________________________________

########################### 4. PCAs ANALYSIS---------##########################3
#_____________________________________________________________________________________________________________________


######## 4. PCAs VERIFICATION NO CORR---------##########################

conditions_corr<- cor(conditions)


for (i in 1:nrow(conditions_corr)){
  
  for(j in 1:ncol(conditions_corr)){
    
    if((conditions_corr[i,j] > 0.5 & rownames(conditions_corr) != colnames(conditions_corr)) | 
       (conditions_corr[i,j] < - 0.5 & rownames[i] != colnames [j])){
      print(conditions_corr[i,j])}
  }
}

gc()


#_____________________________________________________________________________________________________________________

########################### 5. STATIONS ANALYSIS---------##########################
#_____________________________________________________________________________________________________________________


######## 5.1 EDA-------------------##########################

### N? of unique values 
sapply(stations, function(x){length(unique(x))});

### Statistics
sapply(stations, summary);

### Main value

my_mode <- function(x) {
  x <- as.factor(x);
  ret <- names(sort(table(x), decreasing = TRUE))[1];
  return(ret);
}
sapply(stations, my_mode);

# Top n most frequent values
top_n_frequencies <- function(x, n = 5){
  return(names(sort(table(x), decreasing = TRUE)[1:n]));
}
sapply(stations, top_n_frequencies);


# For numerics or integer: boxplot
boxplot(stations[,2]);

# dist of a particular column
plot(density(stations$ACME[!is.na(stations$ACME)]), type = "l", col = "blue");


#  VISUALIZATION OF PRODUCTION IN TIME

stations$mean_prod <- rowMeans(stations[,2:ncol(stations)])
head(stations)

stations%>%
  ggplot()+
  geom_line(mapping = aes (x = Date, y=mean_prod), linetype = 1)


## We can see that the production is not "stable". Let's try to verify this by checking the information for only 1 year (the most recent)

stations[Date >= "2007-01-01" & Date<= "2007-12-31"] %>%
  ggplot()+
  geom_line(mapping = aes (x = Date, y= mean_prod), linetype = 6)

## so production seems to peak around end of June, drop in July, peak again in August then trend back down towards Jan
# in other words, the month has to be taken into account when predicting the prices! 

################ note: dummy the date and calculate coefficients for each month

## summary statistics for only the integer columns i.e without date 

stations_summary <- na.omit(stations[,-c("Date")][,list(stations = colnames(stations[,2:99]), 
                                                        mean = sapply(.SD, mean), 
                                                        median = sapply(.SD, median),
                                                        min = sapply(.SD, min),
                                                        max = sapply(.SD, max),
                                                        std = sapply(.SD,sd))])

stations_summary%>%
  filter(mean == max(mean))

stations_summary%>%
  filter(median == max(median))

stations_summary%>%
  filter(min == min(min))

stations_summary%>%
  filter(max == max(max))

stations_summary%>%
  filter(std == max(std))


stations_summary[, list(stations = "ALL", mean = mean(mean), median = median(median), min = min(min), max = max(max), std = sd(std))]

head(stations_summary)

### lets check the average production by station

stations_summary%>%
  ggplot()+
  geom_point(mapping = aes(x = stations, y = mean), col = "purple")+
  geom_hline(yintercept = mean(stations_summary$mean, na.rm = TRUE), col = "yellow",size = 2)+
  theme_economist()+
  theme(axis.text.x = element_blank())


## there are no obvious outliers, and there does seem to be a central tendency around the mean


######## 5.2 STATIONS CORR---------##########################

## -------------------------WE NEED TO REDO THIS -------------------

#Stations Correlation Analysis 

stations_corr<- data.frame(cor(stations[,2:99]))
corr_check <- stations_corr[stations_corr > 0.5 & stations_corr < -0.5,]
head(corr_check)

## no correlation between stations

######## 5.3 STATIONS MAP---------##########################

bind_summary <- cbind(stations_summary, anex)
cluster_dataset <- bind_summary[,-c("stid")]
head(cluster_dataset)

#Plot of the stations

cluster_dataset %>%
  ggplot()+
  geom_point(mapping = aes(x = nlat, y = elon))


#Map based on the stations (Oklahoma)

leaflet(data = cluster_dataset) %>% 
  addTiles () %>%
  addMarkers(~elon, ~nlat, popup = "Oklahoma", label = ~as.character(mean))

#_____________________________________________________________________________________________________________________

########################### 6. STATIONS CLUSTERING---------##########################
#_____________________________________________________________________________________________________________________


## correlation checks 

cor(cluster_dataset$mean, cluster_dataset$elev)
cor(cluster_dataset$mean, cluster_dataset$elon)
cor(cluster_dataset$mean, cluster_dataset$nlat)
cor(cluster_dataset$elev, cluster_dataset$nlat)


## standarization of the mean production


cluster_dataset$mean_stan <- cluster_dataset[,list(mean_stan = ((mean - mean(cluster_dataset$mean))/sd(cluster_dataset$mean)))]
cluster_dataset$std_stan <- cluster_dataset[,list(std_stan = ((mean - mean(cluster_dataset$std))/sd(cluster_dataset$std)))]


## clustering based on stanarised production


## can remove one of the variables

clustering <- kmeans(x = cluster_dataset[, c("mean_stan", "std_stan")], centers = 10);
table(clustering$cluster);

## we added latitude but delted because all the stations are in te same state so latitude would not be different
##

## adding cluster labels to the dataset

cluster_dataset$cluster <-clustering$cluster

### calculating the silhoutte_score and ideal number of clusters

silhouette_score <- function(k){
  km <- kmeans(x = cluster_dataset[, c("mean_stan", "std_stan")], centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(cluster_dataset[, c("mean_stan", "std_stan")]))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)



## visulising mean production by cluster

cluster_dataset[, list(prod = mean), by = "cluster"] %>%
  ggplot()+
  geom_point(mapping = aes(x=cluster, y = prod))


## visulising elev production by cluster

cluster_dataset[, list(elev= elev), by = "cluster"] %>%
  ggplot()+
  geom_point(mapping = aes(x=cluster, y = elev))

## visulising elon production by cluster

cluster_dataset[, list(elon= elon), by = "cluster"] %>%
  ggplot()+
  geom_point(mapping = aes(x=cluster, y = elon))


## there does seem to be some difference in he production levels per cluster, with some obvious overlaps

#_____________________________________________________________________________________________________________________

########################### 6. MODELLING & PREDICTING---------##########################
#_____________________________________________________________________________________________________________________

## modelling dataset

head(conditions,2)
head(stations,2)
head(cluster_dataset,5)

dt <- data.table(stations)


predict_dataset <- main_dataset[5114:nrow(main_dataset),]
predict_dataset$month <- month(predict_dataset$Date)
dummy_months <- dummy(predict_dataset$month, sep = "=")
dummy_dataset <- cbind(predict_dataset, dummy_months[,-1])
final_predict_dataset <- dummy_dataset[, -c("Date", "month")]
head(final_predict_dataset)


model_list <- list()



for (i in 1:10){
  
    cluster_group <- cluster_dataset%>%
    filter(cluster == i)
  
    index <- which(cluster_dataset$cluster == i, arr.ind = T)
  
    dataset <- dt[,.SD,.SDcols=c(index+1)]
 
    mean <- rowMeans(dataset)

    data <- stations[,c("Date")]
 
    model_dataset <- cbind(data, conditions[1:5113,], mean)
 
    model_dataset$month <- model_dataset[,list(month = month(Date))]

    dummy_months <- dummy(model_dataset$month, sep = "=")
    dummy_dataset <- cbind(model_dataset, dummy_months[,-1])
   
    final_dataset <- dummy_dataset[, -c("Date", "month")]
   
    train_index <- sample(1:nrow(final_dataset), 0.7*nrow(final_dataset))
    val_index <- sample(setdiff(1:nrow(final_dataset), train_index), 0.15 *nrow(final_dataset))
    test_index <- setdiff(1:nrow(final_dataset),c(train_index, val_index))
   
    train_dataset <- final_dataset[train_index]
    dim(train_dataset)
   
    val_dataset <- final_dataset[val_index]
    dim(val_dataset)
   
    test_dataset <- final_dataset[test_index]
    dim(test_dataset)
   
   
   ## train and test dataset split
   
   
   
   ## model for cluster 1
   
   
   model_1 <- lm(mean ~., data = train_dataset)
   summary(model_1)
   
   ## pick the most significant variables
   
   
 
   
   j <- 1
   
   while(j <= 10){
     
     summary_p <- data.table(tidy(model_1))
     head(summary_p)
     
     summary_filter <- summary_p[2:nrow(summary_p),][p.value < 0.01, list (significants = term)]
     head(summary_filter)
     
     significants <- as.character(paste(summary_filter$significants,collapse =  "+"))
     head(significants)  
     
     formula <- paste("mean ~", paste(significants,collapse =  "+"))
     
     model_final <- lm(formula, data = final_dataset)
     summary(model_final)
     
     j = j + 1
     model_1 <- model_final
   }
   
   print(summary(model_final))
   
   model_list[[i]] <- model_final
   
   
   ## get predictions for both datasets
   
   predictions_train <- predict(model_final, newdata = train_dataset)
   predictions_test <- predict(model_final, newdata = test_dataset)
   
   ## get the errors
   errors_train <- predictions_train - train_dataset$mean;
   errors_test <- predictions_test - test_dataset$mean;
   
   # Compute Metrics
   mse_train <- round(mean(errors_train^2), 2);
   mae_train <- round(mean(abs(errors_train)), 2);
   
   mse_test <- round(mean(errors_test^2), 2);
   mae_test <- round(mean(abs(errors_test)), 2);
   
   #Agroup results from the model and bind them together
   
   
   # Build comparison table
   comp <- data.table(model = c("model 6"), 
                      mse_train = mse_train, mae_train = mae_train,
                      mse_test = mse_test, mae_test = mae_test);
   
  
   
   comp;
  
   
   model <- (model_list[[i]])
   
   
   dataset_predict_2 <- predict(model, newdata = final_predict_dataset)
   dataset_predict_2 <- as.vector(dataset_predict_2)
    
for (k in index + 1) {
   main_dataset[5114:nrow(main_dataset), index + 1] <- dataset_predict_2
}
   
}



write.csv(main_dataset[5114:nrow(main_dataset), 1:99], "C:/Users/anups/Desktop/main_dataset.csv")
