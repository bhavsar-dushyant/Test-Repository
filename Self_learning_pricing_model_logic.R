###########################################################################################
## Code for Self-Learning Model
## 1. Import Cluster-SKU Price Range level Win Rate from Previous model Refresh
## 2. Import Cluster-SKU Price Range level Win Rate from Latest model Refresh
## 3. Function that calculates Win Rate difference of two price ranges
## 4. Create a Business Checks based self-learning model 
###########################################################################################

library(data.table)
library(dplyr)

## 1. Import Cluster-SKU Price Range level Win Rate from Previous model Refresh
Cluster_SKU_Pranges_Prev <- fread('C:/Users/Vikas Khanna/Documents/Vikas/TVH/06_Pricing/Win Rate/Cluster_SKU_PRanges_Final.csv', header=T, sep=",", showProgress = TRUE)
Cust_SKU_hist <- Cluster_SKU_Pranges_Prev[,c("Country","sku","Final_Cluster","Price_range","Win_Rate")]
head(Cluster_SKU_Pranges_Prev)

## 2. Import Cluster-SKU Price Range level Win Rate from Latest model Refresh
Cluster_SKU_Pranges_New <- fread('C:/Users/Vikas Khanna/Documents/Vikas/TVH/06_Pricing/Win Rate/Cluster_SKU_PRanges_Final.csv', header=T, sep=",", showProgress = TRUE)
Cust_SKU_new <- Cluster_SKU_Pranges_New[,c("Country","sku","Final_Cluster","Price_range","Win_Rate")]
head(Cluster_SKU_Pranges_New)

## 3. Function that calculates Win Rate difference of two price ranges
Create_WR_diff <- function(input_data){

input_mini <- data.table(unique(input_data[,c("Country","sku","Final_Cluster","Price_range","Win_Rate")]))

# Create Next Price Range to calculate Win Rate difference  
input_data$Next_price_range <- ifelse(input_data$Price_range=='PR1','PR2',
                                               ifelse(input_data$Price_range=='PR2','PR3',
                                                      ifelse(input_data$Price_range=='PR3','PR4',
                                                             ifelse(input_data$Price_range=='PR4','PR5','PR5'))))

input_data <- rename(input_data, Curr_PR_WR=Win_Rate)
input_data <- merge(input_data, input_mini, by.x =c("Country","sku","Final_Cluster","Next_price_range"), 
                                                                by.y =c("Country","sku","Final_Cluster","Price_range"),  all.x=TRUE)

input_data <- rename(input_data, Next_PR_WR=Win_Rate)

## Win Rate difference from 2 price ranges
input_data$WR_diff <- ifelse(input_data$Price_range %in% c('PR1','PR2','PR3','PR4'), 
                             input_data$Next_PR_WR - input_data$Curr_PR_WR, 0)

return(input_data)
}

Prev_Refresh_data <- data.table(Create_WR_diff(Cluster_SKU_Pranges_Prev))
Latest_Refresh_data <- as.data.table(Create_WR_diff(Cluster_SKU_Pranges_New))

#######################################################################################################################
## 4. Create a Business Checks based self-learning model
## Self-Learning Model
## If WR difference increases more than 30%, no price increase
## Take latest 2018 data, put them in a price range and compare Win Rate difference over previous refreshes

keep_cols <- c("Country","sku","Final_Cluster","Price_range","WR_diff")
Prev_Refresh_data <- Prev_Refresh_data[,keep_cols, with=F]
Prev_Refresh_data <- rename(Prev_Refresh_data, Prev_WR_diff=WR_diff)

Latest_Refresh_data <- Latest_Refresh_data[,keep_cols, with=F]
Latest_Refresh_data <- rename(Latest_Refresh_data, Latest_WR_diff=WR_diff)


SL_model <- merge(Latest_Refresh_data, Prev_Refresh_data, by=c("Country","sku","Final_Cluster","Price_range"), all.x=TRUE)
SL_model$Is_Price_Inc <- ifelse(SL_model$Latest_WR_diff - SL_model$Prev_WR_diff > 0.3,"No","Yes")
head(SL_model)

write.csv(SL_model,'Self_Learning_model.csv', row.names = F)

