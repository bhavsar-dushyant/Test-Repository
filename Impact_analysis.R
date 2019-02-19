####################
## Test it on Q1 2018 France data

## Input file with final offers tagged(Output from Win Rate Code)
Data_2018 <- fread('C:/Users/Vikas Khanna/Documents/Vikas/TVH/02_Data/Data_2018/TVH_2018_5C_FinalOffers.csv', header=T, sep=",", showProgress = TRUE)
MAN_2018 <- Data_2018[Data_2018$country_description=='France' & (Data_2018$final_offer==1 | Data_2018$Order_wo_Offer==1),]

## Merge France 2018 file with price ranges data
MAN_2018 <- merge(MAN_2018, Clusters1, by=c("updated_actual_related_customer"), all.x=TRUE)
## Either exclude customers who don't belong to a Cluster/assign them a default Cluster
## These Customers would be excluded in Business checks at the time of implementation
MAN_2018$Final_Cluster[is.na(MAN_2018$Final_Cluster)] <- '<25K_Others'

## Cluster SKU price ranges distribution
Cluster_SKU_distribution <- fread('Cluster_SKU_distribution.csv', header=T, sep=",", showProgress = TRUE)
MAN_2018 <- merge(MAN_2018, Cluster_SKU_distribution, by=c("sku","Final_Cluster"), all.x = TRUE)

## Assign price ranges on 2018 data
MAN_2018 <- MAN_2018 %>% mutate(
  Price_range = ifelse(unitprice_eur>=min & unitprice_eur<P20,"PR1",
                       ifelse(unitprice_eur>=P20 & unitprice_eur<P40,"PR2",
                              ifelse(unitprice_eur>=P40 & unitprice_eur<P60,"PR3",
                                     ifelse(unitprice_eur>=P60 & unitprice_eur<P80,"PR4",
                                            ifelse(unitprice_eur>=P80 & unitprice_eur<=max,"PR5","NA"))))))  

## Import Price recommendation table - coming from pricing model
Price_recomm_table <- fread('Price_recomm_table.csv', header=T, sep=",", showProgress = TRUE)
MAN_2018 <- merge(MAN_2018, Price_recomm_table, by=c("sku","Final_Cluster","Price_range"), all.x = TRUE)

## Price Increase factor based on the %factors defined
MAN_2018$price_inc_factor <- ifelse(MAN_2018$perc_price_inc=='4%',1.04,
                                    ifelse(MAN_2018$perc_price_inc=='3%',1.03,
                                           ifelse(MAN_2018$perc_price_inc=='2%',1.02,
                                                  ifelse(MAN_2018$perc_price_inc=='-4%',0.96,1))))
MAN_2018$price_inc_factor[is.na(MAN_2018$price_inc_factor)] <- 1 

MAN_2018$New_price <- MAN_2018$price_inc_factor * MAN_2018$unitprice_eur

MAN_2018$sales <- ifelse(MAN_2018$Won_offer==1 | MAN_2018$Order_wo_Offer==1, MAN_2018$quantity*MAN_2018$unitprice_eur,0)
MAN_2018$sales_New <- ifelse(MAN_2018$sales>0 , MAN_2018$quantity*MAN_2018$New_price,0)

sum(MAN_2018$sales_New, na.rm=TRUE) - sum(MAN_2018$sales, na.rm=TRUE)
