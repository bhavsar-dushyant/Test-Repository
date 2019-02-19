##########################################################################################################
## Code to Calculate Win Rate Graphs with Suffcient data for 5 countries

## 1. Import output(Final Offers) from Win Rate code
## 2. Apply Indexation rates and bring prices from 2015-2017 to 2018 levels
## 3. Map Clusters(from Customer clustering) to Customers and final_Cluster to data 
## 4. Normalize SKU prices for each country to a reference level
## 5. Creates Price ranges based on quintiles on cross-country normalized prices 
## 6. Assign Sufficient data per Win Rate Graphs 
## 7. Create Final table of SKU Cluster Price Ranges

#########################################################################################################

library(dplyr)
library(data.table)

setwd('C:/Users/Vikas Khanna/Documents/Vikas/TVH/06_Pricing/WR Output/')

France <- fread('France_3yrs_Final_Offers.csv', header = T, sep=",")
Italy <- fread('Italy_3yrs_Final_offers_v02.csv', header = T, sep=",")
UK <- fread('UK_3yrs_Final_offers_v02.csv', header = T, sep=",")

France$Country <- "France"
Italy$Country <- "Italy"
UK$Country <- "UK"

##Clean-up
# France$unitprice_transaction_currency <- France$unitprice_eur

## Append all files
Final_Offers_data <- rbind(France,Italy,UK)

#########################################################################################################
## 2. Apply Indexation rates and bring prices from 2015-2017 to 2018 levels
Final_Offers_data$unitprice_eur_norm <- ifelse(year(Final_Offers_data$order.offer_date)==2015, Final_Offers_data$unitprice_eur*1.08,
                                               ifelse(year(Final_Offers_data$order.offer_date)==2016, Final_Offers_data$unitprice_eur*1.05,
                                                      ifelse(year(Final_Offers_data$order.offer_date)==2017, Final_Offers_data$unitprice_eur*1.03,
                                                             Final_Offers_data$unitprice_eur)))

# Final_Offers_data$Revenue_norm <- ifelse(Final_Offers_data$Won_offer==1 | Final_Offers_data$Order_wo_Offer==1,
#                                                           Final_Offers_data$quantity * Final_Offers_data$unitprice_eur_norm , 0)
# sum(Final_Offers_data$Revenue_norm,na.rm=TRUE)

Final_Offers_data_v2 <- Final_Offers_data[Final_Offers_data$final_offer==1 | Final_Offers_data$Order_wo_Offer==1,]
head(Final_Offers_data_v2)

#########################################################################################################
## 3. Map Clusters(from Customer clustering) to Customers and final_Cluster to data 
Clusters <- fread('C:/Users/Vikas Khanna/Documents/Vikas/TVH/05_VA_Segments/Final_Clusters_data.csv', header=T, sep=",")
table(Clusters$Final_Cluster)
Clusters1 <- Clusters[, updated_actual_related_customer,Final_Cluster]
Final_Offers_Clusters <- merge(Final_Offers_data_v2, Clusters1, by=c("updated_actual_related_customer"), all.x=TRUE)
#########################################################################################################
## 4. Normalize SKU prices for each country to a reference level

# Unique SKUs per country
Final_Offers_Clusters %>% group_by(Country) %>% summarise(n_distinct(sku))

# Median price per SKU-Country
Country_SKU_avg_price <- Final_Offers_Clusters %>% group_by(Country,sku) %>% summarise(median_price = median(unitprice_eur_norm,na.rm = TRUE))
Country_SKU_avg_price1 <- dcast(Country_SKU_avg_price, sku~Country)

Country_SKU_avg_price1$Reference_price <- ifelse(!is.na(Country_SKU_avg_price1$France),Country_SKU_avg_price1$France,
                                                 ifelse(!is.na(Country_SKU_avg_price1$UK),Country_SKU_avg_price1$UK,Country_SKU_avg_price1$Italy)) 
sum(is.na(Country_SKU_avg_price1$Reference_price))

Country_SKU_avg_price1$Reference_country <- ifelse(!is.na(Country_SKU_avg_price1$France),"France",
                                                 ifelse(!is.na(Country_SKU_avg_price1$UK),"UK","Italy")) 
sum(is.na(Country_SKU_avg_price1$Reference_country))

Country_SKU_avg_price1$FR_adj_factor <- ifelse(!is.na(Country_SKU_avg_price1$France), Country_SKU_avg_price1$France/Country_SKU_avg_price1$Reference_price,1)
Country_SKU_avg_price1$IT_adj_factor <- ifelse(!is.na(Country_SKU_avg_price1$Italy), Country_SKU_avg_price1$Italy/Country_SKU_avg_price1$Reference_price,1)
Country_SKU_avg_price1$UK_adj_factor <- ifelse(!is.na(Country_SKU_avg_price1$UK), Country_SKU_avg_price1$UK/Country_SKU_avg_price1$Reference_price,1)

Country_SKU_avg_price1 <- data.table(Country_SKU_avg_price1)
Country_SKU_avg_price1 <- Country_SKU_avg_price1[,c(1,6:9)]

Country_SKU_avg_price2 <-  melt(Country_SKU_avg_price1, id.vars = c("sku", "Reference_country"),
                                     measure.vars = c("FR_adj_factor", "IT_adj_factor", "UK_adj_factor"))

Country_SKU_avg_price2 <- rename(Country_SKU_avg_price2, Country=variable, Normalization_factor=value)
Country_SKU_avg_price2 <- arrange(Country_SKU_avg_price2, sku)
head(Country_SKU_avg_price2)

Country_SKU_avg_price2$Country <- as.character(Country_SKU_avg_price2$Country)
Country_SKU_avg_price2$Country[Country_SKU_avg_price2$Country=='FR_adj_factor'] <- 'France'
Country_SKU_avg_price2$Country[Country_SKU_avg_price2$Country=='IT_adj_factor'] <- 'Italy'
Country_SKU_avg_price2$Country[Country_SKU_avg_price2$Country=='UK_adj_factor'] <- 'UK'
write.csv(Country_SKU_avg_price2,'FR_IT_UK_Normalization_table.csv')

Final_Offers_Clusters <- merge(Final_Offers_Clusters, Country_SKU_avg_price2, by=c("sku","Country"), all.x=TRUE)
## Across-country normalized price
Final_Offers_Clusters$unitprice_eur_ACN <- Final_Offers_Clusters$unitprice_eur_norm * Final_Offers_Clusters$Normalization_factor

#########################################################################################################
## 5. Creates Price ranges based on quintiles on cross-country normalized prices 
Cluster_SKU_distribution <- Final_Offers_Clusters %>% filter(!(is.na(Final_Offers_Clusters$Final_Cluster))) %>% group_by(sku,Final_Cluster) %>% 
  summarise(min=min(unitprice_eur_ACN,na.rm=TRUE),
            max=max(unitprice_eur_ACN,na.rm=TRUE),
            
            P20 = quantile(unitprice_eur_ACN, c(0.20)),
            P40 = quantile(unitprice_eur_ACN, c(0.40)),
            P60 = quantile(unitprice_eur_ACN, c(0.60)),
            P80 = quantile(unitprice_eur_ACN, c(0.80)))


Cluster_SKU_distribution <- as.data.table(Cluster_SKU_distribution)

Final_Offers_Clusters <- merge(Final_Offers_Clusters,Cluster_SKU_distribution, by=c("sku","Final_Cluster"), all.x=TRUE)

Final_Offers_Clusters <- Final_Offers_Clusters %>% mutate(
  Price_range = ifelse(unitprice_eur_ACN>=min & unitprice_eur_ACN<P20,"PR1",
                       ifelse(unitprice_eur_ACN>=P20 & unitprice_eur_ACN<P40,"PR2",
                              ifelse(unitprice_eur_ACN>=P40 & unitprice_eur_ACN<P60,"PR3",
                                     ifelse(unitprice_eur_ACN>=P60 & unitprice_eur_ACN<P80,"PR4",
                                            ifelse(unitprice_eur_ACN>=P80 & unitprice_eur_ACN<=max,"PR5","NA"))))))  

SKU_Cluster_PR_summ <- Final_Offers_Clusters %>% filter(!(is.na(Final_Offers_Clusters$Final_Cluster))) %>% 
                          group_by(sku,Final_Cluster,Price_range) %>% 
                          summarise(num_cust = n_distinct(updated_actual_related_customer),
                                    num_orders = sum(Won_offer+Order_wo_Offer, na.rm = TRUE),
                                    All_Offers = sum(Won_offer+Lost_offer+Order_wo_Offer, na.rm=TRUE),
                                    Win_Rate = sum(Won_offer+Order_wo_Offer)/sum(Won_offer+Lost_offer+Order_wo_Offer))

SKU_Cluster_PR_summ$Win_Rate[is.na(SKU_Cluster_PR_summ$Win_Rate)] <- 0
SKU_Cluster_PR_summ$WR_Graph <- paste0(SKU_Cluster_PR_summ$sku,"-",SKU_Cluster_PR_summ$Final_Cluster)
#########################################################################################################

#########################################################################################################
## 6. Assign Sufficient data per Win Rate Graphs 
WR_Graphs <- SKU_Cluster_PR_summ %>% group_by(WR_Graph) %>% summarise(nPriceRanges = n_distinct(Price_range),
                                                                               num_data_points = sum(All_Offers,na.rm = TRUE))
SKU_Cluster_PR_summ <- merge(SKU_Cluster_PR_summ, WR_Graphs, by=c("WR_Graph"), all.x = TRUE)

SKU_Cluster_PR_summ$Suff_data_per_PR <- ifelse(SKU_Cluster_PR_summ$nPriceRanges>=2 & 
                                                 SKU_Cluster_PR_summ$num_data_points>=10 & 
                                                 SKU_Cluster_PR_summ$num_cust>=5,1,0)

Suff_WRGraph <- SKU_Cluster_PR_summ %>% group_by(sku,Final_Cluster,WR_Graph) %>% summarise(Suff_WRGraph=max(Suff_data_per_PR,na.rm=TRUE))
SKU_Cluster_PR_summ <- merge(SKU_Cluster_PR_summ, Suff_WRGraph, by=c("sku","Final_Cluster","WR_Graph"), all.x=TRUE)

SKU_Cluster_PR_summ <- merge(SKU_Cluster_PR_summ,Cluster_SKU_distribution, by=c("sku","Final_Cluster"), all.x=TRUE)
head(SKU_Cluster_PR_summ)

Data_w_Suff_WRGraph <- SKU_Cluster_PR_summ[SKU_Cluster_PR_summ$Suff_WRGraph==1,]

#########################################################################################################
#########################################################################################################
Final_Offers_Clusters$min_price <- ifelse(Final_Offers_Clusters$Price_range=='PR1', Final_Offers_Clusters$min,
                                          ifelse(Final_Offers_Clusters$Price_range=='PR2', Final_Offers_Clusters$P20,
                                                 ifelse(Final_Offers_Clusters$Price_range=='PR3', Final_Offers_Clusters$P40,
                                                        ifelse(Final_Offers_Clusters$Price_range=='PR4', Final_Offers_Clusters$P60, Final_Offers_Clusters$P80))))

Final_Offers_Clusters$max_price <- ifelse(Final_Offers_Clusters$Price_range=='PR1', Final_Offers_Clusters$P20,
                                          ifelse(Final_Offers_Clusters$Price_range=='PR2', Final_Offers_Clusters$P40,
                                                 ifelse(Final_Offers_Clusters$Price_range=='PR3', Final_Offers_Clusters$P60,
                                                        ifelse(Final_Offers_Clusters$Price_range=='PR4', Final_Offers_Clusters$P80, Final_Offers_Clusters$max))))

Cluster_SKU_PRanges <- data.table(unique(Final_Offers_Clusters[,c("sku","Final_Cluster","Price_range","min_price","max_price")]))

Cluster_SKU_PRanges$min_price <- round(Cluster_SKU_PRanges$min_price,2)
Cluster_SKU_PRanges$max_price <- round(Cluster_SKU_PRanges$max_price,2)

Cluster_SKU_PRanges <- arrange(Cluster_SKU_PRanges, sku, Final_Cluster, Price_range)
#########################################################################################################
#########################################################################################################

#########################################################################################################
## 7. Create Final table of SKU Cluster Price Ranges
Cluster_SKU_PRanges_Final <- merge(Data_w_Suff_WRGraph, Cluster_SKU_PRanges, by=c("sku","Final_Cluster","Price_range"), all.x = TRUE)

Cluster_SKU_PRanges_Final <- Cluster_SKU_PRanges_Final[,c("sku","Final_Cluster","Price_range",
                                                          "min_price","max_price","num_cust","Win_Rate")]
head(Cluster_SKU_PRanges_Final)

## Write files
write.csv(Cluster_SKU_distribution,'Cluster_SKU_distribution.csv', row.names = F)
write.csv(Cluster_SKU_PRanges_Final,'Cluster_SKU_PRanges_Final.csv', row.names = F)
