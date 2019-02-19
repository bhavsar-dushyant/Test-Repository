#########################################################################################################
## Market basket analysis code, both on SKU & PG level 
## Owner: Justine Lootens (Justine_Lootens@mckinsey.com)

## 1. Import packages that are used in this code  
## 2. Import & prepare data for market basket analysis   
## 3. Create transactions objects
## 4. Mine association rules 
## 5. Create output tables 

#########################################################################################################



#########################################################################################################
# 1. Package Import
#########################################################################################################


if(!require(xlsx)){ 
  install.packages('xlsx', repos="https://cran.rstudio.com/", quiet=TRUE)
  require(xlsx)
}

if(!require(readr)){ 
  install.packages('readr', repos="https://cran.rstudio.com/", quiet=TRUE)
  require(readr)
}

if(!require(arules)){ 
  install.packages('arules', repos="https://cran.rstudio.com/", quiet=TRUE)
  require(arules)
}

if(!require(arulesViz)){ 
  install.packages('arulesViz', repos="https://cran.rstudio.com/", quiet=TRUE)
  require(arulesViz)
}

if(!require(rJava)){ 
  install.packages('rJava', repos="https://cran.rstudio.com/", quiet=TRUE)
  require(rJava)
}

if(!require(reshape)){ 
  install.packages('reshape', repos="https://cran.rstudio.com/", quiet=TRUE)
  require(reshape)
}

if(!require(data.table)){ 
  install.packages("data.table", repos="https://cran.rstudio.com/", quiet = TRUE)
  require(data.table)
}

if(!require(stringr)){ 
  install.packages('stringr', repos="https://cran.rstudio.com/", quiet=TRUE)
  require(stringr)
}

if(!require(plyr)){ 
  install.packages("plyr", repos="https://cran.rstudio.com/", quiet=TRUE)
   require(plyr)
}

if(!require(utf8)){ 
  install.packages("utf8", repos="https://cran.rstudio.com/", quiet=TRUE)
  require(utf8)
}

if(!require(RColorBrewer)){ 
  install.packages("RColorBrewer", repos="https://cran.rstudio.com/", quiet=TRUE)
  require(RColorBrewer)
}

install.packages("openxlsx")
library(xlsx)
library(readr)
library(arules)
library(arulesViz)
library(reshape)
library(openxlsx)
library(data.table)
library(stringr)
library(plyr)
library(dplyr)
library(utf8)

Sys.setenv(JAVA_HOME='C:\Program Files (x86)\Common Files\Oracle\Java\javapath\Java')
library(rJava)
#########################################################################################################
# 2. Data Import & Preparation
#########################################################################################################

# 2.1 Read in data & perform basic data preparation ##################################################### 

#setwd("C:/Users/Justine Lootens/Box Sync/TVH Data")

all_data_1 <- fread("2018_1.csv", header=T, sep=";", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
all_data_1[,':='(part_group_code = as.character(part_group_code), item_no = as.character(item_no), `order/offer_date` = as.Date(`order/offer_date`))]

all_data_2 <- fread("2018_2.csv", header=T, sep=";", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
all_data_2[,':='(part_group_code = as.character(part_group_code), item_no = as.character(item_no), `order/offer_date` = as.Date(`order/offer_date`))]

all_data_3 <- fread("2017_1.csv", header=T, sep=";", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
all_data_3[,':='(part_group_code = as.character(part_group_code), item_no = as.character(item_no), `order/offer_date` = as.Date(`order/offer_date`))]

all_data_4 <- fread("2017_2.csv", header=T, sep=";", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
all_data_4[,':='(part_group_code = as.character(part_group_code), item_no = as.character(item_no), `order/offer_date` = as.Date(`order/offer_date`))]

all_data <- rbind(all_data_1,all_data_2,all_data_3,all_data_4)

#new_data <- all_data[which(item_no == '42706')]

# Full offer/order data
#all_data <- fread("2018.csv", header=T, sep=";", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
#all_data[,':='(part_group_code = as.character(part_group_code), item_no = as.character(item_no), `order/offer_date` = as.Date(`order/offer_date`))




# File which maps countries to regions
region = read.csv("Region_v2.csv")
region = as.data.table(region)
region_adj = region[country_description != "",]


# Product categorization file, which contains "INTERNAL USE" category 
product_categorization <- fread("Product_categorisatie.csv", header=T, sep=";", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
product_categorization[,part_group_code := as.character(GROUPCODE)]
product_categorization[,GROUPCODE := NULL]
groupcodes_to_be_excluded=unique(product_categorization[Categorization=="INTERNAL USE",part_group_code])


# Product relationships table
prod_rel_1 <- fread("output-aanv-stock-1.csv", header = T, sep = ",", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
prod_rel_2 <- fread("output-aanv-stock-2.csv", header = T, sep = ",", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
prod_rel <- rbind(prod_rel_1,prod_rel_2)
#prod_rel <- as.data.table(prod_rel)
prod_rel[,`Aanv-kode` := toupper(`Aanv-kode`)]


# Customer characteristics: file which contains segment, specialty & market description information
cust_char <- read.csv("customer_characteristics_part1_v2.csv", header=TRUE, sep=";", encoding = "UTF-8", dec = ".", quote = "\"")
cust_char <- as.data.table(cust_char)
cust_char[,combi_spec_code := as.character(trimws(combi_spec_code, "both"))]
cust_char[,pure_spec_code := as.character(trimws(pure_spec_code, "both"))]
cust_char[,market_description := as.character(trimws(market_description, "both"))]
cust_char[,market_description:= ifelse(market_description == "To be determined","",market_description)]
cust_char[,segmentation_main_description := as.character(trimws(segmentation_main_description, "both"))]

unique_segments = c("End customer","Export company", "Manufacturer", "Multinational end users", "Rental", "Repairer","Trader")
unique_specialties = c("ACCESS","CLEANING","TAIL_LIFT","GROUND_SUPPORT","PORT","BATTERY","AGRI","FORKLIFT")


# File which contains the part group codes that are on MTS (= MyProductSearch) & that have GI properties (= technical properties)
MTS <- fread("PRODUCTGROEPEN IN MTS 2018.csv", header=T, sep=",", encoding = "UTF-8", dec = ".", quote = "\"", showProgress = TRUE)
MTS = MTS[-1,]
MTS_partgroups = MTS[MTS == "yes" & `Has GI properties` == "yes",Group]
GI_partgroups = MTS[MTS == "no" & `Has GI properties` == "yes",Group]


# File which maps market description to specialty
market_description_translation = read.csv("Copy of Market description_Specialty v2.csv")
market_description_translation = as.data.table(market_description_translation)
market_description_translation[,market_description := ï..Market.description]
market_description_translation[,ï..Market.description := NULL]


# 2.2 Prepare product relationships & order data for market basket analysis #############################

# Select the complement and substitute relationships from the product relationships table
prod_rel_complements = prod_rel[`Aanv-kode` == "TO" | `Aanv-kode` == "SG",]
prod_rel_complements[,':='(`SKU TVHno` = as.character(`SKU TVHno`), `Related SKU TVHno` = as.character(`Related SKU TVHno`))]

prod_rel_substitutes = prod_rel[(`Aanv-kode` == "GE" & tekst != "" & (RelationType == "AP" | RelationType == "CA")) 
                                | (`Aanv-kode` == "RR") | (`Aanv-kode` == "R") | (`Aanv-kode` == "ST") | (`Aanv-kode` == "QT") |
                                  (`Aanv-kode` == "GE" & (State == "GN" | State == "PM" | State == "EV" | State2 == "GN" | State2 == "PM" | State2 == "EV"))]
prod_rel_substitutes[,':='(`SKU TVHno` = as.character(`SKU TVHno`), `Related SKU TVHno` = as.character(`Related SKU TVHno`))]


# Add region information
all_data = merge(all_data, region_adj[,.(country_description,Region.to.add)], by = "country_description", all.x = TRUE)
all_data[,Region := as.character(Region.to.add)]
all_data[,Region.to.add := NULL]


# Select Region & perform basic data cleaning 
#data_filtered <- all_data[Region %in% c("Southern Europe") & type=="order" & is.na(complaint_reason) & quantity>0 & part_group_code != 0 & unitprice_eur >0 
#                          & unitprice_eur < 1000000 & !(part_group_code %in% groupcodes_to_be_excluded),]

data_filtered <- all_data[type=="order" & is.na(complaint_reason) & quantity>0 & part_group_code != 0 & unitprice_eur >0 
                          & unitprice_eur < 1000000 & !(part_group_code %in% groupcodes_to_be_excluded),]


# Order based on customer_code & select the columns of interest 
data_filtered_s <- data_filtered[order(customer_code),][,c('order/offer_number','customer_code','item_no','part_group_code', 'order/offer_date', 'country_description')]


# Create mapping tables 
# IMPORTANT NOTE: Piece of code that can be used when we want to have the association rules coming out of the market basket analysis with sku & part group descriptions instead of numbers 

# # For every sku, create a table containing the original sku number, the sku description, the part_group_code to which it belongs
# mapping_table_sku <- unique(data_filtered[,.(item_no, sku_description, part_group_code)])
# 
# ## For skus which belong to multiple PG, select the most recent one as the final one
# nb_PG_per_sku <- mapping_table_sku[,.(nb_PG = uniqueN(part_group_code)), by = .(item_no, sku_description)]
# sku_multiple_PG = nb_PG_per_sku[nb_PG >1,item_no]
# mapping_table_sku_exc = data_filtered[item_no %in% sku_multiple_PG,][,.SD[which.max(`order/offer_date`)], by=item_no][,.(item_no,sku_description,part_group_code)]
# mapping_table_sku = rbind(mapping_table_sku[!(item_no %in% sku_multiple_PG)],mapping_table_sku_exc)
# rm(list = c("nb_PG_per_sku","sku_multiple_PG","mapping_table_sku_exc"))
# 
# ## Remove all commas in the sku_description
# mapping_table_sku[,sku_description := gsub(',','',sku_description)]
# 
# ## Make sure each sku description is unique (if not unique, concatenate with item_no number)
# mapping_table_sku[,count := .N, by = sku_description]
# mapping_table_sku[count>1,updated_sku_descr := paste0(sku_description, "_", item_no)]
# mapping_table_sku[count==1,updated_sku_descr := sku_description]
# mapping_table_sku[,':='(count = NULL, sku_description = updated_sku_descr, updated_sku_descr = NULL)]
# mapping_table_sku[,part_group_code := as.character(part_group_code)]
# mapping_table_sku[,item_no := as.character(item_no)]
# #mapping_table_sku$sku_description <- encodeString(mapping_table_sku$sku_description)
# 
# 
# # For every PG, create a table containing the original part group code, the part group description and the PG categorization
# mapping_table_PG <- unique(data_filtered[,.(part_group_code, part_group_description)])
# 
# ## Make sure each PG only has one PG description, by selecting only the first line per PG
# mapping_table_PG <- mapping_table_PG[, head(.SD, 1), by = .(part_group_code)]
# 
# ## Remove all commas in the part_group_description
# mapping_table_PG[,part_group_description := gsub(',','',part_group_description)]
# 
# ## Make sure each PG description is unique
# mapping_table_PG[,count := .N, by = part_group_description]
# mapping_table_PG[count>1,updated_part_group_descr := paste0(part_group_description, "_", part_group_code)]
# mapping_table_PG[count==1,updated_part_group_descr := part_group_description]
# mapping_table_PG[,':='(count = NULL, part_group_description = updated_part_group_descr, updated_part_group_descr = NULL)]
# 
# ## Add the customer specialty, segment & country characteristics also in this table as a PG
# mapping_table_PG <- rbind(mapping_table_PG,
#                           data.frame(part_group_code = c(unique_specialties,
#                                                          unique_segments,
#                                                          as.character(unique(data_filtered$country_description))),
#                                      part_group_description = c(paste("specialty",unique_specialties, sep = "_"),
#                                                                 paste("segment",unique_segments, sep = "_"),
#                                                                 paste("country",as.character(unique(data_filtered$country_description)), sep = "_"))))
# 
# ## Merge with product categorization file, to have have per PG its category
# mapping_table_PG[,part_group_code := as.character(part_group_code)]
# mapping_table_PG <- merge(mapping_table_PG,product_categorization,by = "part_group_code", all.x = TRUE)
# ### Give the customer specialty, segment & country characteristics the "CUSTOMER CHAR" categorization
# mapping_table_PG[part_group_code %in% c(unique_specialties,
#                                         unique_segments,
#                                         as.character(unique(data_filtered$country_description))),
#                  Categorization := "CUSTOMER CHAR"]
# ### Deal with NA PG categorization
# PG_with_NA = mapping_table_PG[is.na(Categorization),part_group_code]
# mapping_table_PG[is.na(Categorization), Categorization := "UNKNOWN"]
# mapping_table_PG <- droplevels(mapping_table_PG)
# 
# ## Add the PG description & PG categorization to the mapping_table_sku
# mapping_table_sku <- merge(mapping_table_sku,mapping_table_PG[,.(part_group_code, part_group_description, Categorization)],by = "part_group_code")
# mapping_table_sku <- droplevels(mapping_table_sku)
#
#
# # Merge data_filtered_s with the mapping_table_PG to have the Categorization per part_group_code 
# data_filtered_s <- merge(data_filtered_s, mapping_table_PG[,.(part_group_code, Categorization)], by = "part_group_code")


# 2.3 Prepare customer specialty ########################################################################

# Create dataset which summarizes the customer specialty(ies) per customer code 
# The specialty will be used to identify specialty-specific product relationships on PG-level  
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
## Merge customers with orders between 15-18 with the existing specialty information
region_specialty = merge(as.data.table(unique(data_filtered_s[,.(customer_code)])),specialty_all_customers_adj[,.(customer_code,specialty)], by = c("customer_code"),all.x = TRUE)

## Merge customers without specialty information with the existing market description information
region_mkdsc = merge(region_specialty[(is.na(specialty)),.(customer_code)],unique(cust_char[!(market_description == ""),.(customer_code,market_description)]),by = c("customer_code"), all.x = TRUE)
region_mkdsc[,no_mkdsc := ifelse(is.na(market_description),1,0)] #NA represents customers without available market description information 
### Customers without market description information, get the empty specialty (update in original table)
region_specialty[customer_code %in% region_mkdsc[no_mkdsc == 1,customer_code],specialty := ""]

## Add the corresponding specialty for the customers with available market description information
region_mkdsc_av = merge(region_mkdsc[no_mkdsc == 0,],market_description_translation,by = c("market_description"),all.x = TRUE)
region_mkdsc_av[,relevant_specialty := ifelse(Corresponding.specialty == "TBD",0,1)] #if relevant corresponding specialty, then relevant specialty = 1
summary_customer_mkdsc_av = region_mkdsc_av[,.(N = sum(relevant_specialty)),by= .(customer_code)]
### Customers without market description with corresponding specialty, get the empty specialty (update in original table)
region_specialty[customer_code %in% summary_customer_mkdsc_av[N == 0,customer_code],specialty := ""]

## Customers with market description(s) with corresponding specialty, get (combination of these) specialty (update in original table)
region_mkdsc_av_relevant <- as.data.table(stats::aggregate(Corresponding.specialty ~ customer_code, data=unique(region_mkdsc_av[relevant_specialty == 1,.(customer_code,Corresponding.specialty)]), paste, collapse = "|"))
region_mkdsc_av_relevant[,specialty := Corresponding.specialty]
region_mkdsc_av_relevant[,Corresponding.specialty := NULL]
region_specialty = rbind(region_specialty[!(customer_code %in% region_mkdsc_av_relevant$customer_code),],region_mkdsc_av_relevant)


# 2.4 Prepare customer segment ##########################################################################

# Create dataset which summarizes the customer segment(s) per customer code 
segment_all_customers = unique(cust_char[!(segmentation_main_description == ""),.(customer_code,segmentation_main_description)])
segment_all_customers = as.data.table(stats::aggregate(segmentation_main_description ~ customer_code, data=segment_all_customers, paste, collapse = "|"))
region_segment = merge(as.data.table(unique(data_filtered_s[,.(customer_code)])),segment_all_customers[,.(customer_code,segmentation_main_description)], by = c("customer_code"),all.x = TRUE)


# Merge segment & specialty datasets into one dataset 
region_cust_char = merge(region_specialty,region_segment, by = c("customer_code"))


# 2.5 Remove unnecessary datasets #######################################################################

rm(list = c("all_data","data_filtered","product_categorization", "prod_rel_1", "prod_rel_2","prod_rel",
            "specialty_all_customers","specialty_all_customers_adj", "cust_char", "market_description_translation", 
            "MTS", "region", "region_adj", "region_specialty", "region_segment","region_mkdsc","region_mkdsc_av",
            "region_mkdsc_av_relevant","summary_customer_mkdsc_av", "segment_all_customers"))



#########################################################################################################
# 3. Create transactions objects
#########################################################################################################

#setwd("C:/Users/Justine Lootens/Box Sync/TVH Data")

# Assumption: each order basket is seen as a transaction 


# 3.1 SKU-level transactions object #####################################################################

# Summarize all SKUs per order in one line 
transactions_O_SKU <- stats::aggregate(item_no ~ `order/offer_number`, data=data_filtered_s, paste, collapse = "|")
write(file = "transactions_O_tvhNo", transactions_O_SKU[,2])
tr_O_SKU <- read.transactions('transactions_O_tvhNo', rm.duplicates=TRUE, format = 'basket', sep="|", encoding = "UTF-8", quote = "") 
#tr_O_SKU <- as.data.table(transactions_O_SKU[,2])

# Analyze tr_O_SKU object  
# summary(tr_O_SKU) 
# uniqueN(data_filtered_s$item_no) #301553
# table(data_filtered_s[,uniqueN(item_no), by = `order/offer_number`]$V1) #Distribution should be the same than length distribution in summary(tr_O_SKU)


# # Add sku descriptions, part group descriptions & part group categorizations from mapping table to transactions object 
# # IMPORTANT NOTE: Piece of code that can be used when we want to have the association rules coming out of the market basket analysis with sku descriptions instead of numbers 
# included_items <- data.frame(mapping_table_sku[item_no %in% itemInfo(tr_O_SKU)[,1],])
# included_items_ordered <- included_items[order(match(included_items[,c("item_no")],itemInfo(tr_O_SKU)[,1])),]
# included_items_ordered <- droplevels(included_items_ordered)
# itemInfo(tr_O_SKU) <- data.frame(labels = encodeString(as.character(included_items_ordered$sku_description)), level2 = included_items_ordered$part_group_description, level1 = included_items_ordered$Categorization)


# 3.2 PG-level transactions object, with customer characteristics #######################################
# Specialty, segment & country are added 

# Summarize all PGs per order, plus the specialty, segment & country of a customer in one line 
transactions_O_PG <- data.table(stats::aggregate(part_group_code ~ customer_code + `order/offer_number` + country_description, data=data_filtered_s, paste, collapse = "|")) 
#2 orders are linked to 2 customer codes, resulting in a total of 1685691 transactions instead of 1685689 transactions
transactions_O_PG <- merge(transactions_O_PG,region_cust_char,by= "customer_code", all.x = TRUE)

transactions_O_PG[specialty == "" & !(is.na(segmentation_main_description)), part_group_code_char := paste(part_group_code,country_description,segmentation_main_description, sep ="|")]
transactions_O_PG[!(specialty == "") & is.na(segmentation_main_description), part_group_code_char := paste(part_group_code,country_description,specialty, sep ="|")]
transactions_O_PG[!(specialty == "") & !(is.na(segmentation_main_description)), part_group_code_char := paste(part_group_code,segmentation_main_description,country_description,specialty,sep ="|")]
transactions_O_PG[specialty == "" & is.na(segmentation_main_description), part_group_code_char := paste(part_group_code,country_description, sep ="|")]

write(file = "transactions_O_PG", data.frame(transactions_O_PG)[,7])
tr_O_PG <- read.transactions('transactions_O_PG', rm.duplicates=TRUE, format = 'basket', sep="|", encoding = "UTF-8", quote = "") 


# Analyze tr_O_PG object  
# summary(tr_O_PG)


# # Add part group descriptions & part group categorizations from mapping table to transactions object 
# # IMPORTANT NOTE: Piece of code that can be used when we want to have the association rules coming out of the market basket analysis with part group descriptions instead of numbers 
# included_items <- data.frame(mapping_table_PG[part_group_code %in% itemInfo(tr_O_PG)[,1],])
# included_items_ordered <- included_items[order(match(included_items[,c("part_group_code")],itemInfo(tr_O_PG)[,1])),]
# included_items_ordered <- droplevels(included_items_ordered)
# itemInfo(tr_O_PG) <- data.frame(labels = included_items_ordered$part_group_description, level1 = included_items_ordered$Categorization)



#########################################################################################################
# 4. Mine association rules
#########################################################################################################


# 4.1 Assumptions #######################################################################################
# Assumption: each order basket is seen as a transaction 

SupportThreshold_Abs_SKU = 15
SupportThreshold_Rel_SKU = SupportThreshold_Abs_SKU/nrow(transactions_O_SKU)

SupportThreshold_Abs_PG = 75
SupportThreshold_Rel_PG = SupportThreshold_Abs_PG/nrow(transactions_O_PG)

ConfidenceThresholdMax = 0.95
ConfidenceThresholdMin = 0.2


# 4.2 SKU-level rules ###################################################################################

# Mine the rules
#tr_O_SKU$V1 <- as.factor(tr_O_SKU$V1)
#tr_O_SKU <- sapply(tr_O_SKU,as.factor)
rules_O_SKU <- apriori(tr_O_SKU, parameter = list(supp=SupportThreshold_Rel_SKU, conf=ConfidenceThresholdMin, maxlen = 4, target = "rules", maxtime = 60))


# Remove redundant rules
# A rule is redundant if a more general rule with the same or a higher lift exists
rules_O_SKU <- rules_O_SKU[!is.redundant(rules_O_SKU, measure = "lift")]


# Add rules' coverage
quality(rules_O_SKU) <- cbind(quality(rules_O_SKU), coverage = coverage(rules_O_SKU))


# Summarize the rules
summary(rules_O_SKU)


# Sort by lift & store in dataframe
rules_O_SKU <- sort(rules_O_SKU, by='lift', decreasing = TRUE)
#inspect(rules_O_SKU)
rules_O_SKU_df = DATAFRAME(rules_O_SKU, separate = TRUE, setStart = '', itemSep = ' + ', setEnd = '')
#df_basket <- as(rules_O_SKU,"data.frame")


# Visualizations
# plot(rules_O_SKU)
# plot(rules_O_SKU,control=list(col=brewer.pal(11,"Spectral")),main="")
# plot(rules_O_SKU[c(1:20)], method="graph",control=list(type="items",main=""))


# 4.3 PG-level rules ####################################################################################

# Mine the rules (only include rules with are PG in RHS )*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
#tr_O_PG$V1 <- as.factor(tr_O_PG$V1)
rules_O_PG <- apriori(tr_O_PG, parameter = list(supp=SupportThreshold_Rel_PG, 
                                                conf=ConfidenceThresholdMin, 
                                                maxlen = 5, target = "rules", 
                                                maxtime = 100),
                      appearance = list(lhs = c(unique_segments,
                                                unique_specialties,
                                                as.character(unique(data_filtered_s$country_description))),
                                        both = unique(data_filtered_s$part_group_code)))


# Remove redundant rules
rules_O_PG <- rules_O_PG[!is.redundant(rules_O_PG, measure = "lift")]


# Add rules' coverage
quality(rules_O_PG) <- cbind(quality(rules_O_PG), coverage = coverage(rules_O_PG))


# Summarize the rules
summary(rules_O_PG)


# Sort by lift & store in dataframe
rules_O_PG <- sort(rules_O_PG, by='lift', decreasing = TRUE)
rules_O_PG_df = DATAFRAME(rules_O_PG, separate = TRUE, setStart = '', itemSep = ' + ', setEnd = '')


# Visualizations
# plot(rules_O_PG)
# plot(rules_O_PG,control=list(col=brewer.pal(11,"Spectral")),main="")
# plot(rules_O_PG[c(1:20)], method="graph",control=list(type="items",main=""))


# 4.4 Remove unnecessary datasets #######################################################################

#rm(list = c("included_items","included_items_ordered"))

rm(list = c("transactions_O_PG","transactions_O_SKU", "region_cust_char"))



#########################################################################################################
# 5. Create output tables
#########################################################################################################

# 5.1 SKU-level rules ###################################################################################

# Create SKU-level output tables: rules_O_SKU_df_length_2 & rules_O_SKU_df_length_more_than_2
rules_O_SKU_df = as.data.table(rules_O_SKU_df)
rules_O_SKU_df[,":="(LHS = as.character(LHS),RHS = as.character(RHS))]

## Filter out rules which have a confidence above or equal to ConfidenceThresholdMax (0.95)
rules_O_SKU_df = rules_O_SKU_df[confidence < ConfidenceThresholdMax,]

## Split in length 2 and >2

#inspect(rules_O_SKU)

#rules_O_SKU_df_length_1 = rules_O_SKU_df[!grepl("\\+",LHS),]

rules_O_SKU_df_length_2 = rules_O_SKU_df[!grepl("\\+",LHS),]

rules_O_SKU_df_length_more_than_2 = rules_O_SKU_df[grepl("\\+",LHS),]

## Filter out length 2-rules which recommend substitutes
rules_O_SKU_df_length_2 = merge(rules_O_SKU_df_length_2,prod_rel_substitutes[,.(`SKU TVHno`,`Related SKU TVHno`,State)], by.x = c("LHS","RHS"), by.y = c("SKU TVHno","Related SKU TVHno"),all.x = TRUE)
rules_O_SKU_df_length_2 = rules_O_SKU_df_length_2[is.na(State),]
rules_O_SKU_df_length_2[,State := NULL]

## Indicate which length 2-rules are in the product relationships table 
rules_O_SKU_df_length_2 <- merge(rules_O_SKU_df_length_2, prod_rel_complements[,.(`SKU TVHno`,`Related SKU TVHno`,`Aanv-kode`)], by.y = c("SKU TVHno","Related SKU TVHno"), by.x = c("LHS","RHS"), all.x=TRUE)
rules_O_SKU_df_length_2[,In_Prod_Rel_Table := ifelse(is.na(`Aanv-kode`),0,1)]

## Output the files 
write.csv(rules_O_SKU_df_length_2, "20181120_tvhNo Rules with 2 SKUs_Southern Europe.csv")
write.csv(rules_O_SKU_df_length_more_than_2, "20181120_tvhNo Rules with more than 2 SKUs_Southern Europe.csv")
write.csv(rules_O_SKU_df, "20181120_tvhNo Rules with all SKUs_Southern Europe.csv")

# Calculate basic statistics

# ## Number of rules
# nrow(rules_O_SKU_df_length_2)
# nrow(rules_O_SKU_df_length_more_than_2)
# 
# ## Lift of rules
# boxplot(rules_O_SKU_df$lift)
# summary(rules_O_SKU_df$lift)
# 
# boxplot(rules_O_SKU_df_length_2$lift)
# summary(rules_O_SKU_df_length_2$lift)
# 
# boxplot(rules_O_SKU_df_length_more_than_2$lift)
# summary(rules_O_SKU_df_length_more_than_2$lift)
# 
# ## Count of rules
# boxplot(rules_O_SKU_df$count)
# summary(rules_O_SKU_df$count)
# 
# boxplot(rules_O_SKU_df_length_2$count)
# summary(rules_O_SKU_df_length_2$count)
# 
# boxplot(rules_O_SKU_df_length_more_than_2$count)
# summary(rules_O_SKU_df_length_more_than_2$count)
# 
# ## Unique nb of SKUs
# uniqueN(rules_O_SKU_df$RHS)
# uniqueN(rules_O_SKU_df_length_2$RHS)
# uniqueN(rules_O_SKU_df_length_more_than_2$RHS)
# 
# ## Categorization of rules
# ## IMPORTANT NOTE: Need to run the mapping table code from above
# ### RHS
# rules_O_SKU_df = merge(rules_O_SKU_df,mapping_table_sku[,.(item_no,Categorization)],by.x = c("RHS"), by.y = c("item_no"), all.x = TRUE)
# table(rules_O_SKU_df$Categorization)/nrow(rules_O_SKU_df)
# 
# rules_O_SKU_df_length_2 = merge(rules_O_SKU_df_length_2,mapping_table_sku[,.(item_no,Categorization)],by.x = c("RHS"), by.y = c("item_no"), all.x = TRUE)
# table(rules_O_SKU_df_length_2$Categorization)/nrow(rules_O_SKU_df_length_2)
# 
# rules_O_SKU_df_length_more_than_2 = merge(rules_O_SKU_df_length_more_than_2,mapping_table_sku[,.(item_no,Categorization)],by.x = c("RHS"), by.y = c("item_no"), all.x = TRUE)
# table(rules_O_SKU_df_length_more_than_2$Categorization)/nrow(rules_O_SKU_df_length_more_than_2)
# 
# ### LHS-RHS of length 2 rules
# rules_O_SKU_df_length_2[,Categorization_RHS := Categorization]
# rules_O_SKU_df_length_2[,Categorization := NULL]
# rules_O_SKU_df_length_2 = merge(rules_O_SKU_df_length_2,mapping_table_sku[,.(item_no,Categorization)],by.x = c("LHS"), by.y = c("item_no"), all.x = TRUE)
# rules_O_SKU_df_length_2[,Categorization_LHS := Categorization]
# rules_O_SKU_df_length_2[,Categorization := NULL]
# table(rules_O_SKU_df_length_2$Categorization_LHS,rules_O_SKU_df_length_2$Categorization_RHS)/nrow(rules_O_SKU_df_length_2)


# 5.2 PG-level rules ####################################################################################

# Create PG-level output tables: rules_O_PG_df_length_2 & rules_O_PG_df_length_more_than_2
rules_O_PG_df = as.data.table(rules_O_PG_df)
rules_O_PG_df[,":="(LHS = as.character(LHS),RHS = as.character(RHS))]

## Filter out rules which have a confidence above or equal to ConfidenceThresholdMax (0.95)
rules_O_PG_df = rules_O_PG_df[confidence < ConfidenceThresholdMax,]

## Indicate whether the recommended part group is on MTS/ has GI properties 
rules_O_PG_df[,MTS_indicator := ifelse(RHS %in% MTS_partgroups, 1,0)] #PGs which are both on MTS & have GI properties
rules_O_PG_df[,GI_indicator := ifelse(RHS %in% GI_partgroups, 1,0)] #PGs which are not on MTS, but have GI properties 

## Split in length 2 and >2
characteristics = c(unique_segments, unique_specialties, as.character(unique(data_filtered_s$country_description)))
for (i in 1:nrow(rules_O_PG_df)){
  rules_O_PG_df[i, nb_characteristics := sum(unlist(lapply(characteristics,function(x) grep(x,LHS))),na.rm = TRUE)]
}

countCharOccurrences <- function(char, s) {
  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}
rules_O_PG_df[,nb_LHS_elements := countCharOccurrences("\\+",LHS)+1]
rules_O_PG_df[,nb_total_elements := nb_LHS_elements+1]
rules_O_PG_df[,length_2 := ifelse(nb_characteristics==(nb_LHS_elements-1),1,0)]

rules_O_PG_df_length_2 = rules_O_PG_df[length_2 == 1,]
rules_O_PG_df_length_2[,length_2 := NULL,]
rules_O_PG_df_length_more_than_2 = rules_O_PG_df[length_2 == 0,]
rules_O_PG_df_length_more_than_2[,length_2 := NULL,]

## Output the files 
write.csv(rules_O_PG_df_length_2, "20181030_PG Rules with 2 PGs_Southern Europe.csv")
write.csv(rules_O_PG_df_length_more_than_2, "20181030_PG Rules with more than 2 PGs_Southern Europe.csv")
write.csv(rules_O_PG_df, "20181030_PG Rules with all PGs_Southern Europe.csv")

# # Calculate basic statistics
# 
# ## Number of rules
# nrow(rules_O_PG_df_length_2)
# nrow(rules_O_PG_df_length_more_than_2)
# 
# ## Lift of rules
# boxplot(rules_O_PG_df$lift)
# summary(rules_O_PG_df$lift)
# 
# boxplot(rules_O_PG_df_length_2$lift)
# summary(rules_O_PG_df_length_2$lift)
# 
# boxplot(rules_O_PG_df_length_more_than_2$lift)
# summary(rules_O_PG_df_length_more_than_2$lift)
# 
# ## Count of rules
# boxplot(rules_O_PG_df$count)
# summary(rules_O_PG_df$count)
# 
# boxplot(rules_O_PG_df_length_2$count)
# summary(rules_O_PG_df_length_2$count)
# 
# boxplot(rules_O_PG_df_length_more_than_2$count)
# summary(rules_O_PG_df_length_more_than_2$count)
# 
# ## Unique nb of PGs
# uniqueN(rules_O_PG_df$RHS)
# uniqueN(rules_O_PG_df_length_2$RHS)
# uniqueN(rules_O_PG_df_length_more_than_2$RHS)
# 
# ## Categorization of rules
# ## IMPORTANT NOTE: Need to run the mapping table code from above
# ### RHS
# rules_O_PG_df = merge(rules_O_PG_df,mapping_table_PG[,.(part_group_code,Categorization)],by.x = c("RHS"), by.y = c("part_group_code"), all.x = TRUE)
# table(rules_O_PG_df$Categorization)/nrow(rules_O_PG_df)
# 
# rules_O_PG_df_length_2 = merge(rules_O_PG_df_length_2,mapping_table_PG[,.(part_group_code,Categorization)],by.x = c("RHS"), by.y = c("part_group_code"), all.x = TRUE)
# table(rules_O_PG_df_length_2$Categorization)/nrow(rules_O_PG_df_length_2)
# 
# rules_O_PG_df_length_more_than_2 = merge(rules_O_PG_df_length_more_than_2,mapping_table_PG[,.(part_group_code,Categorization)],by.x = c("RHS"), by.y = c("part_group_code"), all.x = TRUE)
# table(rules_O_PG_df_length_more_than_2$Categorization)/nrow(rules_O_PG_df_length_more_than_2)
# 


# 5.3 Remove unnecessary datasets #######################################################################

rm(list = c("prod_rel_complements","prod_rel_substitutes"))
#rm(list = c(""mapping_table_PG", "mapping_table_sku""))
