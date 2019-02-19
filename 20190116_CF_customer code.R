#########################################################################################################
## Collaborative filtering code on customer code level 
## Owner: Justine Lootens (Justine_Lootens@mckinsey.com)

## 1. Import packages that are used in this code  
## 2. Import & prepare data for collaborative filtering  
## 3. Create basetable for collaborative filtering
## 4. Apply collaborative filtering
## 5. Create output table

#########################################################################################################
memory.limit()
memory.limit(size=86000)
memory.profile()


#########################################################################################################
# 1. Package Import
#########################################################################################################


if(!require(data.table)){ 
  install.packages("data.table", repos="https://cran.rstudio.com/", quiet = TRUE)
  require(data.table)
}

if(!require(dplyr)){ 
  install.packages("dplyr", repos="https://cran.rstudio.com/", quiet = TRUE)
  require(dplyr)
}

if(!require(plyr)){ 
  install.packages("plyr", repos="https://cran.rstudio.com/", quiet = TRUE)
  require(plyr)
}

if(!require(sqldf)){ 
  install.packages("sqldf", repos="https://cran.rstudio.com/", quiet = TRUE)
  require(sqldf)
}

if(!require(reshape)){ 
  install.packages("reshape", repos="https://cran.rstudio.com/", quiet = TRUE)
  require(reshape)
}

if(!require(stringr)){ 
  install.packages("stringr", repos="https://cran.rstudio.com/", quiet = TRUE)
  require(stringr)
}

if(!require(parallel)){ 
  install.packages("parallel", repos="https://cran.rstudio.com/", quiet = TRUE)
  require(parallel)
}

install.packages("parallel")
library(xlsx)
library(readr)
library(reshape)
library(openxlsx)
library(data.table)
library(stringr)
library(plyr)
library(dplyr)
library(utf8)
library(parallel)
library(sqldf)
library(tidyr)

numCores <- detectCores()
clus <- makeCluster(numCores)



#########################################################################################################
# 2. Data Import & Preparation
#########################################################################################################

# 2.1 Read in data & perform basic data preparation ##################################################### 

#setwd("C:/Users/Justine Lootens/Box Sync/TVH Data")

all_data_1 <- fread("2018_1.csv", header=T, sep=";", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
#all_data_1[,`order/offer_date` := as.Date(`order/offer_date`)]
#all_data_1$country_description <- tolower(all_data_1$country_description)
#all_data_1 <- all_data_1 %>% filter(country_description != "united kingdom") 
#all_data_1 %>% group_by(all_data_1$actual_rotation_type) %>% summarise(count = n())


all_data_2 <- fread("2018_2.csv", header=T, sep=";", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
#all_data_2[,':='(part_group_code = as.character(part_group_code), item_no = as.character(item_no), `order/offer_date` = as.Date(`order/offer_date`))]
#all_data_2$country_description <- tolower(all_data_2$country_description)
#all_data_2 <- all_data_2 %>% filter(country_description != "united kingdom") 


all_data_3 <- fread("2017_1.csv", header=T, sep=";", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
#all_data_3[,':='(part_group_code = as.character(part_group_code), item_no = as.character(item_no), `order/offer_date` = as.Date(`order/offer_date`))]
#all_data_3$country_description <- tolower(all_data_3$country_description)
#all_data_3 <- all_data_3 %>% filter(country_description != "united kingdom") 


all_data_4 <- fread("2017_2.csv", header=T, sep=";", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
#all_data_4[,':='(part_group_code = as.character(part_group_code), item_no = as.character(item_no), `order/offer_date` = as.Date(`order/offer_date`))]
#all_data_4$country_description <- tolower(all_data_4$country_description)
#all_data_4 <- all_data_4 %>% filter(country_description != "united kingdom") 


all_data <- rbind(all_data_1,all_data_2,all_data_3,all_data_4)
#all_data <- rbind(all_data_2,all_data_4)

all_data$quantity <- as.numeric(gsub(",","",all_data$quantity,fixed=TRUE))
all_data$unitprice_eur <- as.numeric(gsub(",","",all_data$unitprice_eur,fixed=TRUE))
#all_data$quantity <- as.numeric(gsub(",","",all_data$quantity,fixed=TRUE))
all_data[,`order/offer_date` := as.Date(`order/offer_date`)]

# Full offer/order data
#all_data <- fread("02. Data/02. Offer & Order/TVH_Consolidated.csv", header=T, sep=",", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
#all_data[,`order/offer_date` := as.Date(`order/offer_date`)]


# File which maps countries to regions
region = read.csv("Region_v2.csv")
region = as.data.table(region)
region_adj = region[country_description != "",]


# Product categorization file, which contains "INTERNAL USE" category 
product_categorization <- fread("Product_categorisatie.csv", header=T, sep=";", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
groupcodes_to_be_excluded=unique(product_categorization[Categorization=="INTERNAL USE",GROUPCODE])


# Customer characteristics 
## File which contains segment, specialty & market description information
cust_char <- read.csv("customer_characteristics_part1_v2.csv", header=TRUE, sep=";", encoding = "UTF-8", dec = ".", quote = "\"")
cust_char <- as.data.table(cust_char)
cust_char[,customer_code := as.character(customer_code)]
cust_char[,combi_spec_code := as.character(trimws(combi_spec_code, "both"))]
cust_char[,pure_spec_code := as.character(trimws(pure_spec_code, "both"))]
cust_char[,market_description := as.character(trimws(market_description, "both"))]
cust_char[,market_description:= ifelse(market_description == "To be determined","",market_description)]
cust_char[,segmentation_main_description := as.character(trimws(segmentation_main_description, "both"))]

## File which contains tenure information
cust_char2 <- read.csv("customer_characteristics_part2_v2.csv", header=TRUE, sep=";", encoding = "UTF-8", dec = ".", quote = "\"")
cust_char2 <- as.data.table(cust_char2)
cust_char2[,customer_code := as.character(customer_code)]


## File which maps market description to specialty
market_description_translation = read.csv("Copy of Market description_Specialty v2.csv")
market_description_translation = as.data.table(market_description_translation)
market_description_translation[,market_description := ï..Market.description]
market_description_translation[,ï..Market.description := NULL]


# File which contains the part group codes that are on MTS (= MyProductSearch) & that have GI properties (= technical properties)
MTS <- fread("PRODUCTGROEPEN IN MTS 2018.csv", header=T, sep=",", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
MTS = MTS[-1,]
MTS_partgroups = MTS[MTS == "yes" & `Has GI properties` == "yes",Group]
GI_partgroups = MTS[MTS == "no" & `Has GI properties` == "yes",Group]


# 2.2 Merge & filter datasets for basetable creation ####################################################

# Add region information to full offer/order data
all_data = merge(all_data, region_adj[,.(country_description,Region.to.add)], by = "country_description", all.x = TRUE)
all_data[,Region := as.character(Region.to.add)]
all_data[,Region.to.add := NULL]


# Set up timeframe
end_date = max(all_data$`order/offer_date`)
# Set up timeframe
#end_date = max(all_data[Region == "Southern Europe",`order/offer_date`])

start_date_3Y = as.Date(seq(end_date, length=2, by="-3 years")[2])
start_date_LY = as.Date(seq(end_date, length=2, by="-1 years")[2])


# Select the region & years of interest from the full order/offer data 
data=all_data[`order/offer_date`<= end_date &  `order/offer_date` >= start_date_3Y,]
#data=all_data[Region == "Southern Europe" & `order/offer_date`<= end_date &  `order/offer_date` >= start_date_3Y,]


# Create dataset for past purchase behaviour part of the basetable
data_filtered=data[!(part_group_code %in% groupcodes_to_be_excluded) & quantity>0 & unitprice_eur>0 & 
                     part_group_code!=0 & unitprice_eur < 1000000 & is.na(complaint_reason) & type=="order",]
data_filtered[,customer_code := as.character(customer_code)]
data_filtered_s=data_filtered[,.(customer_code,part_group_code,quantity)]


# Create dataset for revenue & %FF-F dummies of the basetable
data_filtered_LY = data_filtered[`order/offer_date`<= end_date &  `order/offer_date` >= start_date_LY,]


# Create dataset for simple conversion rate dummy of the basetable
data_filtered_LY_ordoff = data[!(part_group_code %in% groupcodes_to_be_excluded) & quantity>0 & unitprice_eur>0 & 
                                 part_group_code!=0 & unitprice_eur < 1000000 & is.na(complaint_reason) & `order/offer_date`<= end_date & `order/offer_date` >= start_date_LY,]
data_filtered_LY_ordoff[,customer_code := as.character(customer_code)]


# 2.3 Remove datasets which are not needed anymore ######################################################

rm(list = c("all_data","all_data_1","all_data_2","all_data_3","all_data_4","product_categorization","region","region_adj", "data", "MTS"))



#########################################################################################################
# 3. Basetable creation 
#########################################################################################################

# 3.1 Past purchase behaviour ###########################################################################

# Summarize the customer's last three years purchase behaviour of part groups 

#data_filtered_s$quantity <-sapply(data_filtered_s$quantity,as.numeric)
data_final=cast(data_filtered_s, as.formula(customer_code~part_group_code), value="quantity",fun.aggregate = sum,na.rm=TRUE)
data_final=as.data.frame((apply(data_final,2, function(x) ifelse(x>0,1,0))))


# Mark the part group variables with suffix "_PG"
for(n in 1:(ncol(data_final))){names(data_final)[n]=paste0(names(data_final)[n],"_PG",sep="")}


# 3.2 Dummy: Revenue in last year ############################################################################

# Add revenue to each order line
#data_filtered_LY$quantity <-sapply(data_filtered_LY$quantity,as.numeric)
#data_filtered_LY$unitprice_eur <-sapply(data_filtered_LY$unitprice_eur,as.numeric)
data_filtered_LY[,Revenue := unitprice_eur*quantity]


# Define revenue thresholds for dummies
Rev_lower_threshold = 5000
Rev_upper_threshold = 25000


# Summarize last year's revenue per customer 
revenue_summary = data_filtered_LY[,.(Revenue_sum = sum(Revenue, na.rm = TRUE)), by = .(customer_code)]


# Create 3 revenue dummy variables 
revenue_summary[,revenue_low:=ifelse(Revenue_sum<=Rev_lower_threshold,1,0)] ## 73% of customers in data_filtered_LY
revenue_summary[,revenue_medium:=ifelse(Revenue_sum>Rev_lower_threshold & Revenue_sum<=Rev_upper_threshold,1,0)] ## 18% of customers in data_filtered_LY
revenue_summary[,revenue_high:=ifelse(Revenue_sum>Rev_upper_threshold,1,0)] ## 9% of customers in data_filtered_LY


# Merge with basetable 
data_final$customer_code = row.names(data_final)
data_final=merge(data_final,revenue_summary[,.(customer_code,revenue_low,revenue_medium,revenue_high)],by="customer_code", all.x=TRUE)
data_final=as.data.table(data_final)


# Deal with missing values 
data_final[is.na(revenue_low), revenue_low := 0]
data_final[is.na(revenue_medium), revenue_medium := 0]
data_final[is.na(revenue_high), revenue_high := 0]


# 3.3 Dummy: %FF-F in last year ##############################################################################

# Summarize last year's %FF-F from total revenue per customer 
FF_F_summary = data_filtered_LY[(actual_rotation_type=="F"|actual_rotation_type=="FF"),.(FF_F_Revenue_sum = sum(Revenue, na.rm = TRUE)), by= .(customer_code)]
FF_F_summary = merge(revenue_summary[,.(customer_code,Revenue_sum)],FF_F_summary,by="customer_code", all=TRUE)
FF_F_summary[is.na(FF_F_Revenue_sum),FF_F_Revenue_sum := 0] 
FF_F_summary[,FF_F_ratio := FF_F_Revenue_sum/Revenue_sum]
#FF_F_summary[is.na(FF_F_ratio),FF_F_ratio := 0]


# Create 4 %FF-F dummy variables, based on quartile thresholds 
q1 = quantile(FF_F_summary$FF_F_ratio,0.25)
q2 = quantile(FF_F_summary$FF_F_ratio,0.5)
q3 = quantile(FF_F_summary$FF_F_ratio,0.75)

FF_F_summary[,':='(FF_F_ratio_q1=ifelse(FF_F_ratio<= q1,1,0),
                   FF_F_ratio_q2=ifelse(FF_F_ratio>q1 & FF_F_ratio<=q2,1,0),
                   FF_F_ratio_q3=ifelse(FF_F_ratio>q2 & FF_F_ratio<=q3,1,0),
                   FF_F_ratio_q4=ifelse(FF_F_ratio>q3,1,0))]


# Merge with basetable 
data_final=merge(data_final,FF_F_summary[,.(customer_code,FF_F_ratio_q1,FF_F_ratio_q2,FF_F_ratio_q3,FF_F_ratio_q4)],
                 by="customer_code",all.x=TRUE)


# Deal with missing values 
data_final[is.na(FF_F_ratio_q1),FF_F_ratio_q1 :=0]
data_final[is.na(FF_F_ratio_q2),FF_F_ratio_q2 :=0]
data_final[is.na(FF_F_ratio_q3),FF_F_ratio_q3 :=0]
data_final[is.na(FF_F_ratio_q4),FF_F_ratio_q4 :=0]


# 3.4 Dummy: Simple conversion rate in last year #############################################################

# Summarize last year's simple conversion rate per customer 
summary_orderlines = data_filtered_LY_ordoff[type=="order",.(count_orderlines = .N),by = .(customer_code)]
summary_offerlines = data_filtered_LY_ordoff[type=="offer",.(count_offerlines = .N),by = .(customer_code)]

conversion_summary=merge(summary_orderlines,summary_offerlines,by="customer_code",all=TRUE)
conversion_summary[,conversion_rate:=count_orderlines/count_offerlines]
conversion_summary[is.na(count_orderlines),conversion_rate:=0]
conversion_summary[is.na(count_offerlines),conversion_rate:=median(conversion_summary$conversion_rate, na.rm = TRUE)]


# Create 4 conversion rate dummy variables, based on quartile thresholds 
q1 = quantile(conversion_summary$conversion_rate,0.25)
q2 = quantile(conversion_summary$conversion_rate,0.5)
q3 = quantile(conversion_summary$conversion_rate,0.75)
conversion_summary[,':='(conversion_rate_q1=ifelse(conversion_rate<=q1,1,0),
                         conversion_rate_q2=ifelse(conversion_rate>q1 & conversion_rate<=q2,1,0),
                         conversion_rate_q3=ifelse(conversion_rate>q2 & conversion_rate<=q3,1,0),
                         conversion_rate_q4=ifelse(conversion_rate>q3,1,0))]


# Merge with basetable 
data_final=as.data.table(merge(data_final,conversion_summary[,.(customer_code,conversion_rate_q1,conversion_rate_q2,conversion_rate_q3,conversion_rate_q4)],
                               by="customer_code",all.x=TRUE))


# Deal with missing values 
data_final[is.na(conversion_rate_q1),conversion_rate_q1 :=0]
data_final[is.na(conversion_rate_q2),conversion_rate_q2 :=0]
data_final[is.na(conversion_rate_q3),conversion_rate_q3 :=0]
data_final[is.na(conversion_rate_q4),conversion_rate_q4 :=0]


# 3.5 Dummy: Segment ####################################################################################

# Create dataset which summarizes the customer segment(s)
segment = unique(cust_char[!(segmentation_main_description == ""),.(customer_code,segmentation_main_description)])
segment = as.data.table(stats::aggregate(segmentation_main_description ~ customer_code, data=segment, paste, collapse = "|"))


# Create 7 dummy variables, each representing a segment
# Customers with multiple segments, will have a 1 for multiple dummies 
segment[,':='(Repairer_dummy=ifelse(grepl("Repairer",segmentation_main_description)==1,1,0),
              Trader_dummy=ifelse(grepl("Trader",segmentation_main_description)==1,1,0),
              Rental_dummy=ifelse(grepl("Rental",segmentation_main_description)==1,1,0),
              End_cust_dummy=ifelse(grepl("End customer",segmentation_main_description)==1,1,0),
              Export_company_dummy=ifelse(grepl("Export company",segmentation_main_description)==1,1,0),
              Manufacturer_dummy=ifelse(grepl("Manufacturer",segmentation_main_description)==1,1,0),
              Multinational_end_users_dummy=ifelse(grepl("Multinational end users",segmentation_main_description)==1,1,0))]


# Merge with basetable 
data_final=merge(data_final,segment[,-2],by="customer_code",all.x=TRUE)


# Deal with missing values 
data_final[is.na(Repairer_dummy),Repairer_dummy :=0]
data_final[is.na(Trader_dummy),Trader_dummy :=0]
data_final[is.na(Rental_dummy),Rental_dummy :=0]
data_final[is.na(End_cust_dummy),End_cust_dummy :=0]
data_final[is.na(Export_company_dummy),Export_company_dummy :=0]
data_final[is.na(Manufacturer_dummy),Manufacturer_dummy :=0]
data_final[is.na(Multinational_end_users_dummy),Multinational_end_users_dummy :=0]


# 3.6 Dummy: Actual related customer? ###################################################################

# Add to each order line whether the customer has an actual related customer or not 
data_filtered[,actual_related_customer_binary := ifelse(actual_related_customer=="",0,1)]


# Summarize actual related customer per customer 
actual_related_customer_summary=data_filtered[,.(Actual_cust_dummy = sum(actual_related_customer_binary, na.rm = TRUE)),by= .(customer_code)]


# Create dummy variable, having a 1 if customer has >= 1 related customer groups
actual_related_customer_summary[,Actual_cust_dummy := ifelse(Actual_cust_dummy > 0,1,0)]
actual_related_customer_summary[is.na(Actual_cust_dummy),Actual_cust_dummy := 0]


# Merge with basetable 
data_final=merge(data_final,actual_related_customer_summary,by="customer_code",all.x=TRUE)


# Deal with missing values 
data_final[is.na(Actual_cust_dummy),Actual_cust_dummy :=0]


# 3.7 Dummy: Tenure #####################################################################################

# Customers only have one creation date 
# range(cust_char2[,.(nb_dates = uniqueN(creation_date)),by = .(customer_code)]$nb_dates)


# Calculate tenure (if available) per customer 
tenure_summary=unique(cust_char2[creation_date!="",c("customer_code","creation_date")])
tenure_summary[,Tenure_value:=((end_date-as.Date(substr(as.character(creation_date),1,regexpr(" ",as.character(creation_date))-1)))/365)]

# Create 4 tenure dummy variables, based on quartile thresholds 
q1 = quantile(tenure_summary$Tenure_value,0.25)
q2 = quantile(tenure_summary$Tenure_value,0.5)
q3 = quantile(tenure_summary$Tenure_value,0.75)
tenure_summary[,':='(Tenure_q1=ifelse(Tenure_value<=q1,1,0),
                     Tenure_q2=ifelse(Tenure_value>q1 & Tenure_value<=q2,1,0),
                     Tenure_q3=ifelse(Tenure_value> q2 & Tenure_value<=q3,1,0),
                     Tenure_q4=ifelse(Tenure_value>q3,1,0))]


# Merge with basetable 
data_final=unique(merge(data_final,tenure_summary[,.(customer_code,Tenure_q1,Tenure_q2,Tenure_q3,Tenure_q4)],by="customer_code",all.x=TRUE))


# Deal with missing values 
data_final[is.na(Tenure_q1),Tenure_q1 :=0]
data_final[is.na(Tenure_q2),Tenure_q2 :=1] # in case of missing values, classify them in tenure q2 (median)
data_final[is.na(Tenure_q3),Tenure_q3 :=0]
data_final[is.na(Tenure_q4),Tenure_q4 :=0]


# 3.8 Dummy: Country ####################################################################################

# Get all countries per customer in which he is active
data_country=unique(data_filtered[,c("customer_code","country_description")])
data_country[,country_description := as.character(trimws(country_description, "both"))]
data_country_summary = as.data.table(stats::aggregate(country_description ~ customer_code, data=data_country, paste, collapse = "|"))


# Create a dummy variable for each country in data
# Customers who are active in multiple countries, will have a 1 for multiple countries 
for(n in unique(data_country$country_description)){
  data_country_summary[,paste0("country_",n)]=ifelse(grepl(n,data_country_summary$country_description)==1,1,0)
}


# Merge with basetable 
data_final=unique(merge(data_final,data_country_summary[,-2],by="customer_code",all.x=TRUE))


# Deal with missing values 
for(n in names(data_final)[grepl("country_",names(data_final))]){
  data_final[[n]][is.na(data_final[[n]])]=0
}


# 3.9 Specialty #########################################################################################
# This variable will be used to group customers beforehand, i.e. only find similar customers within the same-specialty groups of customers 
# Order of priority to get this variable
# 1: pure_spec/ combi_spec from pricing team
# 2: market description

# Extract and modify the existing combi & pure specialty information in cust_char dataset 
## Filter out all possible customer - combi & pure spec combinations
specialty_all_customers = unique(cust_char[(combi_spec_code == "" & !(pure_spec_code == ""))
                                           |(!(combi_spec_code == "") & pure_spec_code == "")
                                           |(!(combi_spec_code == "") & !(pure_spec_code == "")),.(customer_code,combi_spec_code,pure_spec_code)])

## Pure spec
### If nothing in pure_spec, add "FORKLIFT"
specialty_all_customers[,pure_spec_code := ifelse(pure_spec_code == "","FORKLIFT",pure_spec_code)]
### Delete the ending number at the end of a pure spec, if there is one
specialty_all_customers[,pure_spec_code := ifelse(!is.na(as.numeric(substring(pure_spec_code,nchar(pure_spec_code)))),
                                                  substr(pure_spec_code,1, nchar(pure_spec_code)-1),
                                                  pure_spec_code)]

## Combi spec
### Split the (possibly) multiple combi specs over multiple columns
specialty_all_customers_adj <-cbind(specialty_all_customers, str_split_fixed(specialty_all_customers$combi_spec_code,",", n = Inf))
nb_combi_spec_columns <- sum(grepl("V",colnames(specialty_all_customers_adj)))
setnames(specialty_all_customers_adj, old = paste0("V",1:nb_combi_spec_columns), new = paste0("combi_spec_code", 1:nb_combi_spec_columns))
specialty_all_customers_adj[,combi_spec_code := NULL]
### Delete the ending number at the end of the combi spec(s)
combi_spec_columns = colnames(specialty_all_customers_adj)[grepl("combi_spec",colnames(specialty_all_customers_adj))]
specialty_all_customers_adj[, (combi_spec_columns) := lapply(.SD, function(x) substr(x,1, nchar(x)-1)), .SDcols = combi_spec_columns]

## Concatenate all pure and combi specs into the specialty variable 
### Remove the leading/lagging spaces (trimws) and then replace one or more spaces (\\s+) with a | using gsub
specialty_all_customers_adj$specialty = gsub("\\s+", "|", trimws(do.call(paste,  specialty_all_customers_adj[,-1])))


# Focus on region of attention
## Merge customers with orders in chosen time period with the existing specialty information
region_specialty = merge(as.data.table(unique(data_filtered[,.(customer_code)])),specialty_all_customers_adj[,.(customer_code,specialty)], by = c("customer_code"),all.x = TRUE)

## Merge customers without specialty information with the existing market description information
region_mkdsc = merge(region_specialty[(is.na(specialty)),.(customer_code)],unique(cust_char[!(market_description == ""),.(customer_code,market_description)]),by = c("customer_code"), all.x = TRUE)
region_mkdsc[,no_mkdsc := ifelse(is.na(market_description),1,0)] #NA represents customers without available market description information 
### Customers without market description information available get TBD specialty (update in original table)
region_specialty[customer_code %in% region_mkdsc[no_mkdsc == 1,customer_code],specialty := "TBD"]

## Add the corresponding specialty for the customers with available market description information
region_mkdsc_av = merge(region_mkdsc[no_mkdsc == 0,],market_description_translation,by = c("market_description"),all.x = TRUE)
region_mkdsc_av[,relevant_specialty := ifelse(Corresponding.specialty == "TBD",0,1)] #if relevant corresponding specialty, then relevant specialty = 1
summary_customer_mkdsc_av = region_mkdsc_av[,.(N = sum(relevant_specialty)),by= .(customer_code)]
### Customers without market description with corresponding specialty, get TBD specialty (update in original table)
region_specialty[customer_code %in% summary_customer_mkdsc_av[N == 0,customer_code],specialty := "TBD"]

## Customers with market description(s) with corresponding specialty, get (combination of these) specialty (update in original table)
region_mkdsc_av_relevant <- as.data.table(stats::aggregate(Corresponding.specialty ~ customer_code, data=unique(region_mkdsc_av[relevant_specialty == 1,.(customer_code,Corresponding.specialty)]), paste, collapse = "|"))
region_mkdsc_av_relevant[,specialty := Corresponding.specialty]
region_mkdsc_av_relevant[,Corresponding.specialty := NULL]
region_specialty = rbind(region_specialty[!(customer_code %in% region_mkdsc_av_relevant$customer_code),],region_mkdsc_av_relevant)

unique_specialty=unique(unlist(strsplit(as.character(region_specialty$specialty),'|', fixed=TRUE)))


# Merge with basetable 
data_final=merge(data_final,region_specialty,by="customer_code",all.x=TRUE)


# Deal with missing values 
data_final[is.na(specialty),specialty :="TBD"]


# Remove unnecessary datasets ############################################################################

# rm(list = setdiff(ls(),c("data_final","unique_specialty","clus",
  #                       "data_filtered_LY", "MTS_partgroups", "GI_partgroups")))


##########################################################################################################
# 4. Apply collaborative filtering
##########################################################################################################

# Run the collaborative filtering algorithm per specialty 

for(sp in unique_specialty){

  Data_Matrix <- as.data.frame(data_final[grepl(sp,data_final$specialty),])
  
  clusterExport(cl=clus, "Data_Matrix", envir=environment())

  
  # 4.1 Similarity calculation between customers ##########################################################

  # Function to calculate Cosine distance between 2 vectors (= 2 customers)
  clusterEvalQ(clus,getCosine <- function(x,y) 
  {
    if (sqrt(sum(x*x)) * sqrt(sum(y*y))==0) {this.cosine <- 0} else
    {this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
    return(this.cosine)}
  })
  
  cols_exclude=c("customer_code","specialty")
  clusterExport(cl=clus, "cols_exclude",envir=environment())
  
  
  # Calculate Cosine distance between all customers, taking into account past purchase behaviour & characteristic dummies
  Sys.time()
  Data_Matrix_similarity <- parApply(clus,Data_Matrix[,!names(Data_Matrix) %in% cols_exclude],1,
                                     function(x) apply(Data_Matrix[,!names(Data_Matrix) %in% cols_exclude],1,function(y) getCosine(x,y)))
  Sys.time()
  
  Data_Matrix_similarity <- as.data.frame(Data_Matrix_similarity)
  rownames(Data_Matrix_similarity) <- Data_Matrix$customer_code
  colnames(Data_Matrix_similarity) <- Data_Matrix$customer_code

  clusterExport(cl=clus, "Data_Matrix",envir=environment())
  
  clusterExport(cl=clus, "Data_Matrix_similarity",envir=environment())
  
  
  # 4.2 Compute likelihood scores per part group per customer, based on similar customers ############################
  
  # Function to calculate the score per part group per customer, based on similar customers
  # Minimum similarity score is 0.45 and min 10 & max 40 most similar customers 
  # If < 10 customers with a score > 0.45, take the 10 customers with the highest similarity scores 
  clusterEvalQ(clus,getScore <- function(history, similarities,required_sim=0.45,max=40,min=10)
  { 
    if(length(similarities[similarities>=required_sim])>min){ 
      vec <- names(similarities[order(similarities,decreasing=TRUE)][similarities>=required_sim]
                   [2:(1+min(max,max(min,length(similarities[similarities>=required_sim]))))])}
    else{vec=names(similarities[order(similarities,decreasing=TRUE)][2:(min+1)])}
    
    similarities[!(names(similarities) %in% vec)] <- 0
   
    if (sum(history*similarities)==0) {score <- 0} 
    else {
      score <- sum(history*similarities)/sum(similarities)
    }
    
    return(score)
  })
  
  
  # Calculate score per part group, per customer 
  PG_cols=names(Data_Matrix)[grepl("_PG",names(Data_Matrix))]
  
  Sys.time()
  Data_Matrix_scores <-as.data.frame(parApply(clus,Data_Matrix[,PG_cols],2,
                                              function(x) apply(Data_Matrix_similarity,1,function(y) getScore(x,y))))
  Sys.time()
  
  
  # Store scores, similarities & basetable
  assign(paste0("Recommendation scores_",sp,sep=""),Data_Matrix_scores)
  assign(paste0("Similarities_",sp,sep=""),Data_Matrix_similarity)
  assign(paste0("Customer_PG_char_matrix_",sp,sep=""),Data_Matrix)
  
}



##########################################################################################################
# 5. Create Output table
##########################################################################################################  

# 5.1 Prepare score tables for output table creation #####################################################
# Add specialty suffix to the colnames 
colnames(`Recommendation scores_ACCESS`)=paste0(colnames(`Recommendation scores_ACCESS`),"ACCESS")
colnames(`Recommendation scores_AGRI`)=paste0(colnames(`Recommendation scores_AGRI`),"AGRI")
colnames(`Recommendation scores_BATTERY`)=paste0(colnames(`Recommendation scores_BATTERY`),"BATTERY")
colnames(`Recommendation scores_CLEANING`)=paste0(colnames(`Recommendation scores_CLEANING`),"CLEANING")
colnames(`Recommendation scores_FORKLIFT`)=paste0(colnames(`Recommendation scores_FORKLIFT`),"FORKLIFT")
colnames(`Recommendation scores_TAIL_LIFT`)=paste0(colnames(`Recommendation scores_TAIL_LIFT`),"TAIL_LIFT")
colnames(`Recommendation scores_GROUND_SUPPORT`)=paste0(colnames(`Recommendation scores_GROUND_SUPPORT`),"GROUND_SUPPORT")
colnames(`Recommendation scores_PORT`)=paste0(colnames(`Recommendation scores_PORT`),"PORT")
colnames(`Recommendation scores_TBD`)=paste0(colnames(`Recommendation scores_TBD`),"TBD")


# Create customer code variable
`Recommendation scores_ACCESS`$customer_code = rownames(`Recommendation scores_ACCESS`)
`Recommendation scores_AGRI`$customer_code = rownames(`Recommendation scores_AGRI`)
`Recommendation scores_BATTERY`$customer_code = rownames(`Recommendation scores_BATTERY`)
`Recommendation scores_CLEANING`$customer_code = rownames(`Recommendation scores_CLEANING`)
`Recommendation scores_FORKLIFT`$customer_code = rownames(`Recommendation scores_FORKLIFT`)
`Recommendation scores_TAIL_LIFT`$customer_code = rownames(`Recommendation scores_TAIL_LIFT`)
`Recommendation scores_GROUND_SUPPORT`$customer_code = rownames(`Recommendation scores_GROUND_SUPPORT`)
`Recommendation scores_PORT`$customer_code= rownames(`Recommendation scores_PORT`)
`Recommendation scores_TBD`$customer_code= rownames(`Recommendation scores_TBD`)


# 5.2 Merge all score tables ##############################################################################

# Create df, joined score table, with rows = customers & columns = part groups (multiple times same part group, but with different suffix)
df <- join_all(list(`Recommendation scores_ACCESS`,`Recommendation scores_AGRI`
                    ,`Recommendation scores_BATTERY`, `Recommendation scores_CLEANING`,
                    `Recommendation scores_FORKLIFT`,`Recommendation scores_TAIL_LIFT`,
                    `Recommendation scores_GROUND_SUPPORT`,`Recommendation scores_PORT`, `Recommendation scores_TBD`), by = 'customer_code', type = 'full')


# Transpose df to df_t, with rows = part groups & columns = customers 
df_t = transpose(df[,-which(colnames(df) == "customer_code")])
colnames(df_t) = df$customer_code

## Delete the specialty suffix per part group column & add a PG column
PG = vector(mode="character", length=length(colnames(df[,-which(colnames(df) == "customer_code")])))
for(i in 1:length(PG)){
  PG[i] = substr(colnames(df[,-which(colnames(df) == "customer_code")])[i],1, regexpr("_",colnames(df[,-which(colnames(df) == "customer_code")])[i])[1]-1)
}
df_t$PG = PG 


# 5.3 Summarize score per customers #######################################################################

# Take per customer the maximum score per PG (coming from different specialties)
score_summary = data.frame(PG=character(),
                           score = numeric(),
                           stringsAsFactors=FALSE) 

for(i in setdiff(colnames(df_t),"PG")){
  x = aggregate(as.numeric(df_t[,c(i, "PG")][,c(i)]), by = list(df_t$PG), max, na.rm = TRUE)
  colnames(x) = c("PG","score")
  score_summary = rbind(data.frame(x, customer_code = i),score_summary)
}


# Prepare score_summary for impact calculation
## Rank based on customer code & decreasing score 
score_summary = score_summary[order(score_summary$customer_code,-score_summary$score),]
score_summary = as.data.table(score_summary)
score_summary_f = score_summary[score>0.2,]
score_summary_f[,customer_code := as.character(customer_code)]


# 5.4 Check some initial constraints ######################################################################

# Indicate which PG recommendations are on MTS/ have GI properties 
score_summary_f[,MTS_indicator := ifelse(PG %in% MTS_partgroups, 1,0)] #PGs which are both on MTS & have GI properties
score_summary_f[,GI_indicator := ifelse(PG %in% GI_partgroups, 1,0)] #PGs which are not on MTS, but have GI properties


# Indicate whether PG was bought by customer last year 

## Identify which part groups were bought by customer last year 
data_filtered_LY_s = data_filtered_LY[customer_code %in% score_summary_f$customer_code,.(customer_code,part_group_code,quantity)]
LY_PG = cast(data_filtered_LY_s, as.formula(customer_code~part_group_code), value="quantity",fun.aggregate = sum, na.rm=TRUE)
LY_PG = as.data.frame(apply(LY_PG,2, function(x) ifelse(x>0,1,0)))
LY_PG$customer_code = rownames(LY_PG)

## Bring in same format than score_summary
LY_PG_t = transpose(LY_PG[,-which(colnames(LY_PG) == "customer_code")])
colnames(LY_PG_t) = LY_PG$customer_code
LY_PG_t$PG = colnames(LY_PG)[which(colnames(LY_PG) != "customer_code")]

LY_PG_summary = data.frame(PG=character(),
                           Bought_LY = integer(),
                           stringsAsFactors=FALSE) 

for(i in setdiff(colnames(LY_PG_t),"PG")){
  x = LY_PG_t[,c(i, "PG")]
  colnames(x) = c("Bought_LY","PG")
  LY_PG_summary = rbind(data.frame(x, customer_code = i),LY_PG_summary)
}

## Merge with score_summary
score_summary_f = merge(score_summary_f, LY_PG_summary, by = c("customer_code","PG"), all.x = TRUE)

## Replace NA values with 0
score_summary_f[is.na(Bought_LY),Bought_LY :=0]

## Order based on customer code & score in descending order
score_summary_f = score_summary_f[order(score_summary_f$customer_code,-score_summary_f$score),]

## Output file
write.csv(score_summary_f,"20190117_Output table_CF on customer code_Europe.csv")