
############################################################################################################################
## Clusters on All TVH Countries
############################################################################################################################

setwd('C:/Users/Vikas Khanna/Documents/Vikas/TVH/02_Data')
getwd()
require(data.table)
require(stringr)
require(dplyr)

TVH <- fread('TVH_Consolidated_All_data.csv', header=TRUE, sep=",",showProgress=TRUE)

Imp_cols <- c("customer_code","original_customer","actual_related_customer","sku","sku_tvh","type","aup_landed","company_code",
              "order/offer_number","order/offer_date", "quantity","unitprice_eur","country_description","customer_pricegroup",
              "actual_rotation_type")

TVH <- TVH[, Imp_cols, with=FALSE]
TVH <- as.data.table(TVH)
head(TVH)
TVH$`order/offer_date` <- as.Date(TVH$`order/offer_date`)

names(TVH)[names(TVH) == "order/offer_number"] <- "order.offer_number"
names(TVH)[names(TVH) == "order/offer_date"] <- "order.offer_date"
colnames(TVH)

### 
#1. Within country price normalization - using indexation
TVH$unitprice_eur_norm <- ifelse(year(TVH$order.offer_date)==2015, TVH$unitprice_eur*1.08,
                                    ifelse(year(TVH$order.offer_date)==2016, TVH$unitprice_eur*1.05,
                                           ifelse(year(TVH$order.offer_date)==2017, TVH$unitprice_eur*1.03,TVH$unitprice_eur)))

# Clean data & select relevant variables
TVH_cleaned <- TVH[(quantity>0 & unitprice_eur_norm>0),]
TVH_cleaned <- data.frame(TVH_cleaned)
TVH_cleaned <- arrange(TVH_cleaned,actual_related_customer,customer_code,sku,order.offer_date)
TVH_cleaned <- as.data.table(TVH_cleaned)

# Create a self-created related customer group variable, for the customers who do not have a related customer group
TVH_cleaned$updated_actual_related_customer <- character(nrow(TVH_cleaned))
TVH_cleaned[actual_related_customer=="",updated_actual_related_customer := paste0(customer_code,"_ARC")]
TVH_cleaned[!(actual_related_customer==""),updated_actual_related_customer := actual_related_customer]
sum(is.na(TVH_cleaned$updated_actual_related_customer))

TVH_cleaned[, sales := quantity*unitprice_eur_norm]

##########################################################
# Creating Target variable 
##########################################################
SKU_ASP <- TVH_cleaned %>% filter(type=='order') %>% group_by(country_description, sku) %>% 
  summarise(avg_SKU_unitprice = sum(sales, na.rm = TRUE)/sum(quantity, na.rm=TRUE))

TVH_orders <- TVH_cleaned %>% filter(type=='order')
TVH_orders_Rev <- merge(x=TVH_orders, y=SKU_ASP, by=c("country_description","sku"), all.x=TRUE)

TVH_orders_Rev$Expected_Revenue <- TVH_orders_Rev$quantity * TVH_orders_Rev$avg_SKU_unitprice

TVH_orders_custlevel <- TVH_orders_Rev %>% group_by(updated_actual_related_customer) %>% 
  summarise(Actual_Revenue=sum(sales, na.rm=TRUE),
            Expected_Revenue=sum(Expected_Revenue, na.rm=TRUE))

TVH_orders_custlevel <- mutate(TVH_orders_custlevel, perc_deviation=(Actual_Revenue-Expected_Revenue)/Expected_Revenue)
Target_variable <- TVH_orders_custlevel[,c("updated_actual_related_customer","perc_deviation","Actual_Revenue")]

############################################################################################################################
## Create Customer size and Main Segment variables
############################################################################################################################

Cust_codes <- data.table(unique(TVH_cleaned[,customer_code,updated_actual_related_customer]))

Cust_country  <- data.table(unique(TVH_cleaned[,updated_actual_related_customer, country_description]))
Cust_country_rollup <- Cust_country %>% group_by(updated_actual_related_customer) %>% 
  summarise(Country=paste(country_description, collapse = "/"))

#1
Customer_Size <- TVH_cleaned %>% filter(type=='order') %>% group_by(updated_actual_related_customer) %>% 
                                                summarise(annual_revenue = sum(sales,na.rm=TRUE)/3)

#2
Cust_char1 <- fread('C:/Users/Vikas Khanna/Documents/Vikas/TVH/02_Data/General Data/customer_characteristics_part1_v2.csv', 
                    header=TRUE, sep=";", encoding = "UTF-8", dec = ".", quote = "\"")
Cust_char1 <- data.frame(Cust_char1)
Cust_char1_seg <- data.frame(unique(Cust_char1[c("customer_code","segmentation_main_description")]))
Cust_Char1_Mainseg <- Cust_char1_seg %>% group_by(customer_code) %>% 
  summarise(Main_Segment=paste(segmentation_main_description, collapse = "/"))
table(Cust_Char1_Mainseg$Main_Segment)
Cust_Char1_Mainseg$Main_Segment2 <- gsub("[[:space:]]", "", Cust_Char1_Mainseg$Main_Segment)
Main_segments <- Cust_Char1_Mainseg %>% group_by(Main_Segment2) %>% summarise(count=n()) %>% arrange(desc(count))

########################################################################################################################
Cust_codes <- merge(Cust_codes, Cust_Char1_Mainseg, by=c("customer_code"), all.x=TRUE)
Cust_codes_seg <- unique(Cust_codes[,updated_actual_related_customer,Main_Segment2])
Cust_UARC_Mainseg <- Cust_codes_seg %>% group_by(updated_actual_related_customer) %>% 
  summarise(Main_Segment2=paste(Main_Segment2, collapse = "/"))

Main_segments <- Cust_UARC_Mainseg %>% group_by(Main_Segment2) %>% summarise(count=n()) %>% arrange(desc(count))
# write.csv(Main_segments,'All_UARC_Main_segments_Freq.csv')

# table(Cust_UARC_Mainseg$Main_Segment)
# mainseg <- Cust_UARC_Mainseg %>% group_by(Main_Segment) %>% summarise(count=n()) %>% arrange(desc(count))

Cust_UARC_Mainseg$Main_Segment1 <- ifelse(Cust_UARC_Mainseg$Main_Segment2 %in% c('Repairer',
                                                                                'Trader',
                                                                                'Repairer/Trader',
                                                                                'Repairer/Repairer/Trader',
                                                                                'Repairer/Trader/Trader',
                                                                                '/Repairer',
                                                                                '/Trader',
                                                                                'Repairer/Repairer/Trader/Trader',
                                                                                '/Repairer/Trader',
                                                                                '/Repairer/Repairer/Trader',
                                                                                '/Repairer/Repairer/Trader/Trader',
                                                                                '/Repairer/Trader/Trader',
                                                                                'Repairer/',
                                                                                'Trader/'),"Repairer/Trader","Others")

Cust_UARC <- data.table(unique(TVH_cleaned[,"updated_actual_related_customer"]))
Cust_UARC1 <- merge(Cust_UARC, Customer_Size, by=c("updated_actual_related_customer"), all.x=TRUE)
Cust_UARC1 <- merge(Cust_UARC1, Cust_UARC_Mainseg, by=c("updated_actual_related_customer"), all.x=TRUE)
Cust_UARC1 <- merge(Cust_UARC1, Cust_country_rollup, by=c("updated_actual_related_customer"), all.x=TRUE)
Cust_UARC1 <- merge(Cust_UARC1, Target_variable, by=c("updated_actual_related_customer"), all.x=TRUE)

Country_freq <- Cust_UARC1 %>% group_by(Country) %>% summarise(count=n()) %>% arrange(desc(count))


Cust_UARC1$Final_Cluster <- ifelse(is.na(Cust_UARC1$annual_revenue),"NA",
                                   ifelse(Cust_UARC1$annual_revenue <= 25000 & Cust_UARC1$Main_Segment1=='Others',"<25K_Others",
                                          ifelse(Cust_UARC1$annual_revenue <= 25000 & Cust_UARC1$Main_Segment1=='Repairer/Trader', "<25K_Repairer/Trader",
                                                 ifelse(Cust_UARC1$annual_revenue > 25000 & Cust_UARC1$annual_revenue <= 150000, "25K - 150K",
                                                        ifelse(Cust_UARC1$annual_revenue > 150000, "> 150K","NA")))))
table(Cust_UARC1$Final_Cluster)
write.csv(Cust_UARC1, 'C:/Users/Vikas Khanna/Documents/Vikas/TVH/05_VA_Segments/TVH_All_Countries_Clusters.csv')
