
####################################################################################################################
##########################                     Win Rate logic                             ##########################
### Steps 
#1. Import all data files
#2. Clean up data
#3. Tagging final offers
#4. Tagging orders w/o offers
#5. Consolidating data
####################################################################################################################

require(dplyr)
require(sqldf)
require(data.table)


setwd('C:/Users/Vikas Khanna/Documents/Vikas/TVH/02_Data') ## <- Pass location here
getwd()

TVH <- fread('MAN 2017.csv', header=TRUE, sep=";", encoding = "UTF-8", dec = ".", quote = "\"")##<- Pass file name here
All_FM_SKUs <- fread('C:/Users/Vikas Khanna/Documents/Vikas/TVH/02_Data/All_FM_SKUs.csv', header=TRUE, sep=",",showProgress=TRUE)
Qty_discounts <- fread('C:/Users/Vikas Khanna/Documents/Vikas/TVH/02_Data/General Data/Quantity_discounts_simplified.csv', header=TRUE, sep=",")

names(TVH)[names(TVH) == "order/offer_number"] <- "order.offer_number"
names(TVH)[names(TVH) == "order/offer_date"] <- "order.offer_date"

Imp_cols <- c("original_customer","customer_code","actual_related_customer","sku","auto_create","item_no","sku_tvh","type",
              "actual_rotation_type","deliveryterm","unitprice_transaction_currency",
              "order.offer_number","order.offer_date", "quantity","unitprice_eur")

country <- TVH[(country_description %in% c("France")) & quantity>0 & unitprice_eur>0, Imp_cols, with=FALSE]
dim(country)
country_FF <- merge(country, All_FM_SKUs , by=c("sku"), all.x=TRUE)
dim(country_FF)

country <- as.data.table(country_FF)
country[,order.offer_date := as.Date(order.offer_date)]
str(country$order.offer_date)

# rm(TVH) ##<- remove big file

####################################################################################################################
# Add Justine's code - Remove 6% SKUs
####################################################################################################################

country[, sales := quantity*unitprice_eur]

# Identify customers with more than one actual related group & update the basetable with the correct actual related group 
customers_nonunique_ARC <-country[,.(nb_ARC = uniqueN(actual_related_customer)), by= customer_code][nb_ARC > 1,]
customers_non_unique_ARC <- unique(merge(customers_nonunique_ARC,country[,.(original_customer,customer_code,actual_related_customer)],by= "customer_code"))
customers_non_unique_ARC[,check := ifelse(customer_code == original_customer, "yes","no")]
correct_ARC_customers_non_unique_ARC <- customers_non_unique_ARC[check == "yes", .(customer_code,actual_related_customer)] # Correct group is the one who corresponds to the original_customer = customer_code 

for (i in 1:nrow(correct_ARC_customers_non_unique_ARC)){
  country[customer_code == correct_ARC_customers_non_unique_ARC$customer_code[i], actual_related_customer := correct_ARC_customers_non_unique_ARC$actual_related_customer[i]]
}

# Clean data & select relevant variables
Imp_cols <- c("customer_code","actual_related_customer","sku","deliveryterm", "item_no","sku_tvh","type","order.offer_number","order.offer_date", 
              "quantity","unitprice_eur","unitprice_transaction_currency", "sales")
country_cleaned <- country[(quantity>0 & unitprice_eur>0 & type == "order"),Imp_cols, with=FALSE]
country_cleaned <- data.frame(country_cleaned)
country_cleaned <- arrange(country_cleaned,actual_related_customer,customer_code,sku,order.offer_date)
country_cleaned <- as.data.table(country_cleaned)

# Create a self-created related customer group variable, for the customers who do not have a related customer group
country_cleaned$updated_actual_related_customer <- character(nrow(country_cleaned))
country_cleaned[actual_related_customer=="",updated_actual_related_customer := paste0(customer_code,"_ARC")]
country_cleaned[!(actual_related_customer==""),updated_actual_related_customer := actual_related_customer]
# sum(is.na(country_cleaned$updated_actual_related_customer))

country$updated_actual_related_customer <- character(nrow(country))
country[actual_related_customer=="",updated_actual_related_customer := paste0(customer_code,"_ARC")]
country[!(actual_related_customer==""),updated_actual_related_customer := actual_related_customer]
sum(is.na(country$updated_actual_related_customer))

summary_CC_avgipt <- country_cleaned[, .(AVG_Repurchase = sum(difftime(order.offer_date, shift(order.offer_date), units = "days"), na.rm=TRUE)/(.N-1), count = .N), by = .(updated_actual_related_customer,customer_code,sku)]
summary_CC_avgipt$AVG_Repurchase_days <- as.numeric(summary_CC_avgipt$AVG_Repurchase, unit='days')

### Per related customer group
country_cleaned <- arrange(country_cleaned,updated_actual_related_customer,sku,order.offer_date)
country_cleaned <- as.data.table(country_cleaned)
summary_RC_avgipt <- country_cleaned[, .(AVG_Repurchase = sum(difftime(order.offer_date, shift(order.offer_date), units = "days"), na.rm=TRUE)/(.N-1), count = .N), by = .(updated_actual_related_customer,sku)]
summary_RC_avgipt$AVG_Repurchase_days <- as.numeric(summary_RC_avgipt$AVG_Repurchase, unit='days')

selection_RC_avgipt <- summary_RC_avgipt[AVG_Repurchase_days<=20 & !(is.na(AVG_Repurchase_days)) & count>=12, 1:2]

## Testing
### Remove such Customer/SKU from data
Imp_cols2 <- c("updated_actual_related_customer","sku","sku_tvh","type","auto_create","actual_rotation_type","deliveryterm",
               "order.offer_number","order.offer_date", "quantity","unitprice_eur","sales","unitprice_transaction_currency")
country_cleaned1 <- country[(quantity>0 & unitprice_eur>0 & deliveryterm %in% c('st','ST','')),Imp_cols2, with=FALSE]
table(country_cleaned1$type)
country_cleaned1 %>% group_by(actual_rotation_type) %>% summarise(sum(sales,na.rm=TRUE))

# & actual_rotation_type %in% c('F','FF')

country_cleaned1 <- country[(quantity>0 & unitprice_eur>0 & 
                           deliveryterm %in% c('st','ST','') & 
                           actual_rotation_type %in% c('F','FF')),Imp_cols2, with=FALSE]
country_selected <- anti_join(country_cleaned1, selection_RC_avgipt, by=c("updated_actual_related_customer","sku"))
table(country_selected$type)

## Keep In-stock and Fast moving items data
country_selected <- as.data.table(country_selected)
country_selected1 <- country_selected[(quantity>0 & unitprice_eur>0 & deliveryterm %in% c('st','ST','') & 
                                     actual_rotation_type %in% c('F','FF')),Imp_cols2, with=FALSE]
# table(country_selected0$type)
country_selected <- country_selected1
############################################################################################################################
############################################################################################################################

country_selected <- arrange(country_selected, updated_actual_related_customer,sku,order.offer_date,type)
View(country_selected)

#########################################################################
#Remove duplicate offers and keep only selected columns
del_columns <- c("actual_rotation_type","deliveryterm","order.offer_number")
country_selected0 <- as.data.table(country_selected[, !names(country_selected) %in% del_columns, drop=F])
country_selected1 <- country_selected0[!duplicated(country_selected0)]
country_selected1 <- arrange(country_selected1, updated_actual_related_customer,sku,order.offer_date,type)
table(country_selected1$type)

#############################################################################################################33
Qty_disc_TVH <- Qty_discounts[Qty_discounts$Validfor=="TVH", qty_disc:=1L]
Qty_disc_TVH <- Qty_disc_TVH[complete.cases(Qty_disc_TVH[,qty_disc]), ]
Qty_disc_TVH <- Qty_disc_TVH[, c('sku', 'qty_disc'), with=FALSE]
Qty_disc_TVH1 <- data.frame(unique(Qty_disc_TVH[,list(sku,qty_disc)]))

country_selected1 <- merge(x=country_selected1, y=Qty_disc_TVH1, by=c("sku"),all.x = TRUE)
country_selected1$qty_disc[is.na(country_selected1$qty_disc)] <- 0

# country_new1 <- country_cleaned[1:100,]
country_selected1 <- as.data.table(country_selected1)
# sample_c <- sample(unique(country_selected1[,updated_actual_related_customer]), 10)
# country_new1 <- country_selected1[updated_actual_related_customer %in% sample_c,]

country_new1 <- country_selected1

country_new1[,sku := as.character(sku)]
country_new1$order.offer_date <- as.Date(country_new1$order.offer_date)

country_new1 <- arrange(country_new1, updated_actual_related_customer,sku,order.offer_date,type)
country_new1 <- country_new1[,c("updated_actual_related_customer","sku","type","order.offer_number",
                            "order.offer_date","auto_create","quantity","unitprice_eur","qty_disc","unitprice_transaction_currency")]
country_new1 <- as.data.table(country_new1)

# country_new1 <- country_new1[5:10,]
View(country_new1)
dim(country_new1)
colnames(country_new1)

###############################################################################################################
#
#
#
## Win Rate logic starts
#
#
#
###############################################################################################################

customers <- unique(country_new1[,updated_actual_related_customer]) ## unique customers list
country_new1[,final_offer := 0L]
country_new1[,Won_offer := 0L]
country_new1[,Lost_offer := 0L]

n <- 0   ##Initialize a counter to reach at right row number
# print(paste0("n=", n))

#1###########################################################
#first loop across customers
for(i in 1:length(customers)){     
  # print(unique(country_new1$updated_actual_related_customer)[i])
  
  customer_c <- customers[i]
  skus <- unique(country_new1[updated_actual_related_customer == customer_c,sku])
  
  #2###########################################################  
  #Second loop across sku for each customer
  for(j in 1:length(skus)){      
    # print(paste0("sku = ",skus[j]))
    
    select_data <- country_new1[updated_actual_related_customer == customer_c  &  sku == skus[j], ]
    nrow_sd <- nrow(select_data)
    # print(paste0("#rows for given cust-sku=",nrow_sd))
    
    if(nrow_sd==1){
      ## We check if there is only one order without any offer.. 
      # print("Only One Order - so Won")
      # print(paste0("Row#=",n+k))
      if(select_data$type[1]=='offer'){  
        country_new1[n+1,final_offer := 1L]
        country_new1[n+1,Lost_offer := 1L]
      }
    }
    
    else if(nrow_sd>1){
      #3###########################################################
      #third loop for each row  
      for(k in 1:nrow_sd){       ## A loop for #rows in a sku for a customer
        # print(paste0("K loop will go from ",k," to ",nrow_sd))
        # print(paste0("k=",k))
        
        if(k==nrow_sd){ ### If there is only one row in selected data or the last row of selected data is an offer.. It is final and lost
          if(select_data$type[k]=='offer'){  
            country_new1[n+k,final_offer := 1L]
            country_new1[n+k,Lost_offer := 1L]
            break
          }
          else if(select_data$type[k]=='order'){break} ## Do not process the last row, when it's an order
        }
        
        if(select_data$type[k] == 'offer'){
          Current_date <- select_data$order.offer_date[k]   ###Store date of current row#
          given_unitprice <- select_data$unitprice_transaction_currency[k]  ###Store unit price of current row#
          Is_autocreated <- select_data$auto_create[k]
          
          #4###########################################################
          #fourth loop within 30days window 
          for(l in k+1:nrow_sd){
            
            # print(paste0("k=",k))
            # print(paste0("l=",l))
            # print(paste0("L loop will go from ",l," to ",nrow_sd))
            
            ### If next row is within 30 days 
            if(select_data$order.offer_date[l]>=Current_date & select_data$order.offer_date[l] < Current_date+30){
              
              if(select_data$type[l]=='offer'){ ## First is an offer, second is also an offer
                
                if(Is_autocreated=='Y' & select_data$auto_create[l]=='Y'){
                  # print("First Offer is AC and second is also AC, move on..")
                  break} ## If First Offer is AC and second is also AC, stop for this offer
                if(Is_autocreated=='Y' & select_data$auto_create[l]=='N'){
                  # print("first Offer is AC and second is MC, AC is neither won/loss, move on..")  
                  break} ## If first Offer is AC and second is MC, AC is neither won/loss, move on
                if(Is_autocreated=='N' & select_data$auto_create[l]=='Y'){ ## If first Offer is MC and second is AC, check prices
                  # print("first Offer is MC and second is AC. Check prices..")
                  if(select_data$unitprice_transaction_currency[l] == given_unitprice){
                    # print("Offer with same price") 
                    break      ###If we found that there is a similar offer available in next - 30 days, End further iterations
                  }
                  else if(select_data$unitprice_transaction_currency[l] != given_unitprice){
                    # print("Offer with diff price")
                    # print(paste0("Row#=",n+k))
                    if(select_data$qty_disc[l]==1){break}
                    else {
                      country_new1[n+k,final_offer := 1L]
                      country_new1[n+k,Lost_offer := 1L]
                      break
                    }
                  }
                } 
                
                if(Is_autocreated=='N' & select_data$auto_create[l]=='N'){ ## Both are manual offers, Normal case
                  if(select_data$unitprice_transaction_currency[l] == given_unitprice){
                    # print("Offer with same price") 
                    break      ###If we found that there is a similar offer available in next - 30 days, End further iterations
                  }
                  else if(select_data$unitprice_transaction_currency[l] != given_unitprice){
                    # print("Offer with diff price")
                    # print(paste0("Row#=",n+k))
                    if(select_data$qty_disc[l]==1){break}
                    else {
                      country_new1[n+k,final_offer := 1L]
                      country_new1[n+k,Lost_offer := 1L]
                      break
                    }
                    ## We established that this is a final-lost offer. break the loop
                  }
                }  
              }
              else if(select_data$type[l]=='order'){
                if(select_data$unitprice_transaction_currency[l]==given_unitprice){
                  # print("Order with same price - Won Offer")
                  # print(paste0("Row#=",n+k))
                  country_new1[n+k,final_offer := 1L]
                  country_new1[n+k,Won_offer := 1L]
                  country_new1[n+k,Lost_offer := 0L]
                  break
                }
                else if(select_data$unitprice_transaction_currency[l]>given_unitprice){
                  # print("Order with higher price - Not a final offer")
                  break
                }
                else if(select_data$unitprice_transaction_currency[l]<given_unitprice){
                  # print("Order with low price")
                  if(select_data$qty_disc[l]==1){
                    country_new1[n+k,final_offer := 1L]
                    country_new1[n+k,Won_offer := 1L]
                    country_new1[n+k,Lost_offer := 0L]
                    break
                  }
                  else {
                    country_new1[n+k,final_offer := 1L]
                    country_new1[n+k,Won_offer := 0L]
                    country_new1[n+k,Lost_offer := 1L]
                    break
                  }
                }
              }
            }
            else if(select_data$order.offer_date[l] >= Current_date+30){
              # print("Current date is more than 30 days")
              country_new1[n+k,final_offer := 1L]
              country_new1[n+k,Lost_offer := 1L]
              break
            }
          }
        }
      }
    }
    n <- n + nrow_sd    ##Value of n should be updated once loop has run for selected data
    #print(paste0("New n=", n))
  }
}

#View(country_new1)

#####################################################################################################
## Calculate Orders w/o Offers
#####################################################################################################


n <- 0   ##Initialize a counter to reach at right row number
# print(paste0("n=", n))

country_new1[,Order_wo_Offer := 0L]


#1###########################################################
#first loop across customers
for(i in 1:length(customers)){     
  # print(unique(country_new1$original_customer)[i])
  
  customer_c <- customers[i]
  skus <- unique(country_new1[updated_actual_related_customer == customer_c,sku])
  
  #2###########################################################  
  #Second loop across sku for each customer
  for(j in 1:length(skus)){      
    # print(paste0("sku = ",skus[j]))
    
    select_data <- country_new1[updated_actual_related_customer == customer_c  &  sku == skus[j], ]
    nrow_sd <- nrow(select_data)
    # print(paste0("#rows for given cust-sku=",nrow_sd))
    
    if(nrow_sd==1){
      if(select_data$type[1]=='order'){  
        country_new1[n+1,Order_wo_Offer := 1L]
      }
    }
    
    else if(nrow_sd>1){
      #3###########################################################
      #third loop for each row  
      for(k in nrow_sd:1){       ## A loop for #rows in a sku for a customer
        # print(paste0("K loop will go from ",k," to ",nrow_sd))
        # print(paste0("k=",k))
        
        if(select_data$qty_disc[k]==0 & select_data$type[k] == 'order'){
          # print("This row is an Order")
          Order_date <- select_data$order.offer_date[k]   ###Store date of current row#
          Ordered_unitprice <- select_data$unitprice_transaction_currency[k]  ###Store unit price of current row#
          
          select_data_30d <- select_data[select_data$order.offer_date > Order_date-30 &
                                           select_data$order.offer_date <= Order_date, ]
          
          nrow_sd30 <- nrow(select_data_30d)
          
          ### If there are only orders, then assign each as order w/o offer
          if(length(unique(select_data_30d$type))==1){
            # print("If only orders in selected data...")
            country_new1[n+k,Order_wo_Offer := 1L]
          }
          
          ### If >1 rows, then start a loop - row by row
          else if(length(unique(select_data_30d$type))>1){
            for(l in nrow_sd30-1:1){
              if(select_data_30d$type[l]=='offer'){
                if(select_data_30d$unitprice_transaction_currency[l] == Ordered_unitprice){
                  # print("Matching offer found")
                  # print(paste0("Row#=",n+k))
                  country_new1[n+k,Order_wo_Offer := 0L]
                  break
                }
                else if(select_data_30d$unitprice_transaction_currency[l] != Ordered_unitprice){
                  # print("No Matching offer")
                  # print(paste0("Row#=",n+k))
                  country_new1[n+k,Order_wo_Offer := 1L]
                }
              }
              if(select_data_30d$type[l]=='order') { 
                break 
              }
            }
          }
        }
      }
    }
    n <- n+nrow_sd    ##Value of n should be updated once loop has run for selected data
    #print(paste0("New n=", n))
  }
}
write.csv(country_new1,'France_FinalOffers.csv')

#########################################################
#   Win Rate Calculate
#########################################################

# country_new1$Revenue <- ifelse(country_new1$type=='order',
#                              country_new1$quantity*country_new1$unitprice_eur,0) 
# sum(country_new1$Revenue)
# 
# # Cust_groups <- country %>% group_by(updated_actual_related_customer, customer_pricegroup) %>% summarise(n_cust=n())
# Cust_sku_base <- country_new1 %>% group_by(updated_actual_related_customer, sku) %>% summarise(count=n(), rev=sum(Revenue,na.rm=TRUE))
# sum(Cust_sku_base$rev)
# 
# # Cust_sku_base <- merge(x=Cust_sku_base, y=Cust_groups, by=c("updated_actual_related_customer"), all.x=TRUE)
# 
# Final_offers <- country_new1 %>% group_by(updated_actual_related_customer, sku, unitprice_eur) %>% filter(final_offer==1) %>%
#   summarise(WO = sum(Won_offer, na.rm = TRUE), LO = sum(Lost_offer, na.rm = TRUE))
# Order_wo_offers <- country_new1 %>% group_by(updated_actual_related_customer, sku, unitprice_eur) %>% 
#   summarise(OwO = sum(Order_wo_Offer, na.rm = TRUE))
# 
# Cust_sku_Win_Rate1 <- merge(x=Cust_sku_base, y=Final_offers, by=c("updated_actual_related_customer","sku"), all.x=TRUE)
# Cust_sku_Win_Rate2 <- merge(x=Cust_sku_base, y=Order_wo_offers, by=c("updated_actual_related_customer","sku"), all.x=TRUE)
# 
# Cust_sku_Win_Rate <- merge(x=Cust_sku_Win_Rate1, y=Cust_sku_Win_Rate2, by=c("updated_actual_related_customer","sku","unitprice_eur"), all.y=TRUE)
# 
# Cust_sku_Win_Rate$Revenue <- Cust_sku_Win_Rate$rev.y
# Cust_sku_Win_Rate[is.na(Cust_sku_Win_Rate)] <- 0
# Cust_sku_Win_Rate <- Cust_sku_Win_Rate %>% 
#   select(updated_actual_related_customer, sku, unitprice_eur,WO,LO,OwO,Revenue) %>% 
#   mutate(Win_Rate = round(((WO+OwO)/(WO+LO+OwO)),2))
# 
# View(Cust_sku_Win_Rate)
# summary(Cust_sku_Win_Rate$Win_Rate)
# sum(Cust_sku_Win_Rate$Revenue, na.rm = TRUE)

# write.csv(Cust_sku_Win_Rate,'France_3years_WR_Agg.csv')
