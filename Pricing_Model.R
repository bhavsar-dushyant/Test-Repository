##########################################################################################################
## Code to calculate % Price increase number based on Win Rate Archetypes

## 1. Import Cluster-SKU price ranges - based on historical data
## 2. Create archetypes based on Win Rate behavior
## 3. Define strategy for Win Rate conditions
## 4. Creates Price ranges based on quintiles 
## 5. Assign Sufficient data per Win Rate Graphs 
## 6. Create Final table of SKU Cluster Price Ranges

#########################################################################################################

library(dplyr)
library(data.table)

Cluster_SKU_Pranges <- fread('C:/Users/Vikas Khanna/Documents/Vikas/TVH/06_Pricing/Win Rate/Cluster_SKU_PRanges_Final.csv', header=T, sep=",", showProgress = TRUE)
head(Cluster_SKU_Pranges)

Cluster_SKU_Pranges$Next_PR_WR <- lead(Cluster_SKU_Pranges$Win_Rate)
Cluster_SKU_Pranges$lead_WR_diff <- ifelse(Cluster_SKU_Pranges$Price_range != 'PR5', 
                                      Cluster_SKU_Pranges$Next_PR_WR - Cluster_SKU_Pranges$Win_Rate, 0)

Cluster_SKU_Pranges$Prev_PR_WR <- lag(Cluster_SKU_Pranges$Win_Rate)
Cluster_SKU_Pranges$lag_WR_diff <- ifelse(Cluster_SKU_Pranges$Price_range != 'PR1', 
                                           Cluster_SKU_Pranges$Prev_PR_WR - Cluster_SKU_Pranges$Win_Rate, 0)

Cluster_SKU_Pranges$Next_PR_nCust <- lead(Cluster_SKU_Pranges$num_cust)
Cluster_SKU_Pranges$Prev_PR_nCust <- lag(Cluster_SKU_Pranges$num_cust)

Cluster_SKU_Pranges$Next_price_range <- ifelse(Cluster_SKU_Pranges$Price_range=='PR1','PR2',
                                            ifelse(Cluster_SKU_Pranges$Price_range=='PR2','PR3',
                                                   ifelse(Cluster_SKU_Pranges$Price_range=='PR3','PR4',
                                                          ifelse(Cluster_SKU_Pranges$Price_range=='PR4','PR5','PR5'))))

## Assign Archetypes based on Win Rate differences in two consecutive price ranges
Cluster_SKU_Pranges$Archetype <- ifelse(Cluster_SKU_Pranges$Price_range != 'PR5',
                                        ifelse(Cluster_SKU_Pranges$lead_WR_diff > 0.05 , "Increasing WR",
                                        ifelse(Cluster_SKU_Pranges$lead_WR_diff >= -0.05 & Cluster_SKU_Pranges$lead_WR_diff <= 0.05, "Flat WR",
                                        ifelse(Cluster_SKU_Pranges$lead_WR_diff < -0.05 & Cluster_SKU_Pranges$Next_PR_WR > 0.65,"Slightly Decreasing WR",
                                               "NA"))),"Highest PR")
table(Cluster_SKU_Pranges$Archetype)
head(Cluster_SKU_Pranges)


nCust_per_PR <- 5 ## Minimum number of customers needed per price range for robust suggestion
attach(Cluster_SKU_Pranges)
Cluster_SKU_Pranges$perc_price_inc <- ifelse(Archetype == 'Increasing WR' & Next_PR_nCust >= nCust_per_PR, ### <- Forward Looking approach
                                             ifelse(lead_WR_diff > 0.05 & lead_WR_diff <= 0.10, "4%",
                                             ifelse(lead_WR_diff > 0.10 & lead_WR_diff <= 0.20, "4%",
                                             ifelse(lead_WR_diff > 0.20 & lead_WR_diff <= 0.30, "4%",
                                             ifelse(lead_WR_diff > 0.30, "4%","NA")))),
                                      ifelse(Archetype == 'Flat WR' & Win_Rate >= 0.4 & Next_PR_nCust >= nCust_per_PR, "2%",
                                      ifelse(Archetype == 'Slightly Decreasing WR' & Next_PR_nCust >= nCust_per_PR, 
                                             ifelse(Win_Rate >= 0.75 & Win_Rate < 0.85, "3%",
                                             ifelse(Win_Rate >= 0.85 , "4%","NA")),
                                      ifelse(Archetype == 'Highest PR' & num_cust >= nCust_per_PR, 
                                             ifelse(Win_Rate >= 0.75 & Win_Rate < 0.85, "3%",
                                             ifelse(Win_Rate >= 0.85 , "4%","NA")),
                                             
                                      ifelse(Archetype == 'NA' & Win_Rate < 0.4 & Prev_PR_WR >= 0.4 & 
                                               lag_WR_diff > 0.20 & Prev_PR_nCust >= nCust_per_PR,  ### <- Backward Looking approach
                                             ifelse(lag_WR_diff > 0.20 & lag_WR_diff <= 0.30, "-10%",
                                             ifelse(lag_WR_diff > 0.30, "-10%","NA")),"NA")))))
  
table(Cluster_SKU_Pranges$perc_price_inc)

table(Cluster_SKU_Pranges$Archetype, Cluster_SKU_Pranges$perc_price_inc)

Price_recomm_table <- data.table(unique(Cluster_SKU_Pranges[,c("sku","Final_Cluster","Price_range","min_price","max_price","perc_price_inc")]))
write.csv(Price_recomm_table,'Price_recomm_table.csv', row.names = F)
