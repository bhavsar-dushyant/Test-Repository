
require(dplyr)
require(data.table)

Final_Offers <- fread('C:/Users/Vikas Khanna/Documents/Vikas/TVH/06_Pricing/WR Output/Austria_3yrs_Final_offers_v02.csv', header = T, sep=",")

sum(Final_Offers$final_offer, na.rm = TRUE)
sum(Final_Offers$Won_offer, na.rm = TRUE)
sum(Final_Offers$Lost_offer, na.rm = TRUE)
sum(Final_Offers$Order_wo_Offer, na.rm = TRUE)

library(zoo)
Final_Offers$yearmon <- as.yearmon(Final_Offers$order.offer_date)
Final_Offers$sales <- ifelse(Final_Offers$type=='order',Final_Offers$quantity*Final_Offers$unitprice_eur, 0)

sum(Final_Offers$sales, na.rm=TRUE)

Monthly_WR <- Final_Offers %>% group_by(yearmon) %>% summarise(WO = sum(Won_offer, na.rm = TRUE),
                                                                   LO = sum(Lost_offer, na.rm = TRUE),
                                                                   OWO = sum(Order_wo_Offer, na.rm = TRUE),
                                                                   sales = sum(sales, na.rm=TRUE))
