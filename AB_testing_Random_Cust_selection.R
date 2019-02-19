
## Import input file with all final offers tagged, sales & margin calculated
output_2018_final <- fread('output_2018_final.csv', sep=",", header=T)

## Creating a Customer level summary
Cust2018_summ <- output_2018_final %>% filter(Country=='France') %>% 
  group_by(Country, Final_Cluster, updated_actual_related_customer) %>% 
  summarise(num_cust = n_distinct(updated_actual_related_customer),
            Won_offer=sum(Won_offer, na.rm = TRUE),
            Lost_offer=sum(Lost_offer, na.rm = TRUE),
            Order_wo_Offer=sum(Order_wo_Offer, na.rm = TRUE),
            num_sku = n_distinct(sku),
            sales = sum(sales, na.rm = TRUE),
            margin=sum(margin, na.rm = TRUE))

## Dividing Randomly 50% Customers into each base  
smp_size <- floor(0.50 * nrow(Cust2018_summ))
train_ind <- sample(seq_len(nrow(Cust2018_summ)), size = smp_size)
treat <- Cust2018_summ[train_ind,]
treat$group <- "treat"
control <- Cust2018_summ[-train_ind,]
control$group <- "control"
franc1 <- rbind(treat,control)

# Testing data partition approach using caret
# library(caret)
# sample <- createDataPartition(y = Cust2018_summ$Won_offer, p=0.5, list=F)
# treat1 <- Cust2018_summ[sample,]
# control1 <- Cust2018_summ[-sample,]
# treat1$group <- "treat"
# control1$group <- "control"
# franc1 <- rbind(treat1,control1)

## Summary of Treated vs Control group
France2018_summ <- franc1 %>% filter(Country=='France') %>% group_by(group) %>% 
  summarise(num_cust = sum(num_cust),
            num_orders = sum(Won_offer+Order_wo_Offer, na.rm = TRUE),
            All_Offers = sum(Won_offer+Lost_offer+Order_wo_Offer, na.rm=TRUE),
            num_sku = sum(num_sku),
            Win_Rate = sum(Won_offer+Order_wo_Offer)/sum(Won_offer+Lost_offer+Order_wo_Offer),
            sales = sum(sales, na.rm = TRUE),
            margin=sum(margin, na.rm = TRUE))

