#Nielsen project

rm(list=ls(all=TRUE))

library(dplyr)
library(ggplot2)
library(sandwich)
library(foreign)
library(Matrix)
library(arules)
library(arulesViz)
library(data.table)
library(magrittr)
library(e1071)
library(tibble)
library(dendextend)
library(colorspace)


##Uploading list of product types
(list.files("DATA/") %>%
    grep("pro_ross",.) %>%
    list.files("DATA/", full.names = TRUE)[.] -> file_list)

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file, header=TRUE, sep="|", dec=",")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.table(file, header=TRUE, sep="|", dec=",")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

summary(dataset$Retailer_Product_Description)
dataset <- unique(dataset)
dataset["type"] <- paste(dataset$category, dataset$subcategory_group, dataset$family_group, sep = "_")



##Uploading shopping baskets
(list.files("DATA/") %>%
    grep("bas_ross",.) %>%
    list.files("DATA/", full.names = TRUE)[.] -> file_list)

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("DT")){
    DT <- read.table(file, header=TRUE, sep="|", dec=",")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("DT")){
    temp_dataset <-read.table(file, header=TRUE, sep="|", dec=",")
    DT<-rbind(DT, temp_dataset)
    rm(temp_dataset)
  }
  
}

DT["year"] <- substr(DT$Transaction_Date_Time, 1, 4)
DT["year"] <- as.numeric(DT$year)

DT["month"] <- substr(DT$Transaction_Date_Time, 5, 6)
DT["month"] <- as.numeric(DT$month)

DT["day"] <- substr(DT$Transaction_Date_Time, 7, 8)
DT["day"] <- as.numeric(DT$day)

DT["hour"] <- substr(DT$Transaction_Date_Time, 9, 10)
DT["hour"] <- as.numeric(DT$hour)

DT["minute"] <- substr(DT$Transaction_Date_Time, 11, 12)
DT["minute"] <- as.numeric(DT$minute)

DT["daytime_id"] <- 60*DT$hour + DT$minute

#creating data sample
set.seed(123)
nrow(DT) %>% {sample(.,. * 0.01)} -> index
DT_train <- DT[index,]
DT_train <- subset(DT_train, Total_Basket_Value > 0)

summary(DT_train)
hist(DT$daytime_id)

DTt_hour <- DT_train %>% group_by(hour) %>% summarise(mean = mean(Total_Basket_Value), sd = sd(Total_Basket_Value))
plot(DTt_hour$hour, DTt_hour$mean, type = "l", main="Average total basket value", xlab="Hour", ylab="Value [CZK]", col="blue")

DTt_price_shop <-
  DT_train %>%
  group_by(Retailer_Store_Number) %>%
  summarise(mean_value = mean(Total_Basket_Value, na.rm = T),
            sd_value = sd(Total_Basket_Value),
            mean_items = mean(Total_Item_Count),
            mean_hour = mean(hour))

#location_labels
LOC <- read.csv(file="DATA/stores.csv", sep=";", dec=".")
LOC["loc"] <- paste(LOC$mesto, LOC$ulice, sep = "_")
colnames(LOC)[colnames(LOC)=="store_id"] <- "Retailer_Store_Number"
DTt_price_shop <- merge(DTt_price_shop, LOC, by="Retailer_Store_Number")


DTt_price_shop <- DTt_price_shop[order(DTt_price_shop[,2],decreasing=TRUE),]

DTt_price_shop <- arrange(DTt_price_shop, desc(mean_items))
barplot(height = DTt_price_shop$mean_items, width = 1, space = 1.5,
        names.arg = as.factor(DTt_price_shop$Retailer_Store_Number), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = NULL, border = par("fg"),
        main = "Items bought in stores", sub = NULL, xlab = "Store number", ylab = "Average total items bought",
        xlim = NULL, ylim = NULL, xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        inside = TRUE, plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)

DTt_price_shop <- arrange(DTt_price_shop, desc(mean_value))
barplot(height = DTt_price_shop$mean_value, width = 1, space = 1.5,
        names.arg = as.factor(DTt_price_shop$Retailer_Store_Number), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = NULL, border = par("fg"),
        main = "Value of baskets", sub = NULL, xlab = "Store number", ylab = "Mean value",
        xlim = NULL, ylim = NULL, xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        inside = TRUE, plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)

DTt_price_shop <- arrange(DTt_price_shop, desc(mean_hour))
barplot(height = DTt_price_shop$mean_hour, width = 1, space = 1.5,
        names.arg = as.factor(DTt_price_shop$Retailer_Store_Number), legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = NULL, border = par("fg"),
        main = "Mean shopping hour", sub = NULL, xlab = "Store number", ylab = "Mean value",
        xlim = NULL, ylim = NULL, xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        inside = TRUE, plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, args.legend = NULL)

#items clasification
DTt_price_shop["items_class"]

#value/items plot
par(mfrow=c(1,1))
plot(DTt_price_shop$mean_value, DTt_price_shop$mean_items, main="Items vs. price", xlab="Average basket value", ylab="Average number of items")


#distance to highway regression
DIST <- read.csv(file="DATA_CREATED/highway_distance.csv",
                 sep=",", dec=".", encoding = "latin")
DIST["HubDist"] <- DIST$HubDist/1000
DIST["logHubDist"] <- log(DIST$HubDist)
summary(lm(DIST$R_mean_ite ~ DIST$HubDist + DIST$R_mean_val))
summary(lm(DIST$R_mean_ite ~ DIST$logHubDist + DIST$R_mean_val))




#dendogram
shops_spec <- DTt_price_shop
shops_spec["Retailer_Store_Number"] <- NULL
shops_spec["mean_hour"] <- NULL
shops_spec["sd_value"] <- NULL
d_shops <- dist(shops_spec) # method="man" # is a bit better
hc_shops <- hclust(d_shops, method = "complete")
shops_id <- rev(levels(DTt_price_shop[,13]))


dend <- as.dendrogram(hc_shops)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:134)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=5) #, groupLabels=iris_species)

# We shall add the flower type to the labels:
labels(dend) <- paste(as.character(DTt_price_shop[,13])[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Clustered shops", 
     horiz =  TRUE,  nodePar = list(cex = .007))
legend("topleft", legend = shops_id, fill = rainbow_hcl(3))

###
# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(DTt_price_shop[,1])[order.dendrogram(dend)]
  )]


#write CSV file
write.table(DTt_price_shop, file = "DATA_GENERATED/DTt_price_shop.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")



##Uploading item list
(list.files("DATA/") %>%
    grep("purchase_ross",.) %>%
    list.files("DATA/", full.names = TRUE)[.] -> file_list)

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("ITM")){
    ITM <- read.table(file, header=TRUE, sep="|", dec=",")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("ITM")){
    temp_dataset <-read.table(file, header=TRUE, sep="|", dec=",")
    ITM<-rbind(ITM, temp_dataset)
    rm(temp_dataset)
  }
  
}

ITM <- merge(ITM, dataset, by="Retailer_Product_ID")

#creating data sample
set.seed(123)
nrow(ITM) %>% {sample(.,. * 0.01)} -> index
ITMt <- ITM[index,]
ITMt <- subset(ITMt, Sold_Amount > 0)

ITMt["year"] <- substr(ITMt$Transaction_Date_Time, 1, 4)
ITMt["year"] <- as.numeric(ITMt$year)

ITMt["month"] <- substr(ITMt$Transaction_Date_Time, 5, 6)
ITMt["month"] <- as.numeric(ITMt$month)

ITMt["day"] <- substr(ITMt$Transaction_Date_Time, 7, 8)
ITMt["day"] <- as.numeric(ITMt$day)

ITMt["hour"] <- substr(ITMt$Transaction_Date_Time, 9, 10)
ITMt["hour"] <- as.numeric(ITMt$hour)

ITMt["minute"] <- substr(ITMt$Transaction_Date_Time, 11, 12)
ITMt["minute"] <- as.numeric(ITMt$minute)

ITMt["daytime_id"] <- 60*ITMt$hour + ITMt$minute


summary(ITMt)
hist(DT$daytime_id)

#group by items
ITMt_items <- ITMt %>% group_by(Retailer_Product_ID) %>% summarise(sum_itm = sum(Retailer_Product_ID))
ITMt_items["sum_itm"] <- ITMt_items$sum_itm/ITMt_items$Retailer_Product_ID
ITMt_items <- merge(ITMt_items, dataset, by="Retailer_Product_ID")

#group by items types
ITMt_types <- ITMt %>% group_by(type) %>% summarise(sum_type = n())
arrange(ITMt_types, desc(sum_type))



#basket with negative values
DT_f1 <- subset(DT, DT$Total_Basket_Value < 0)
DT_check <- DT

for(i in 1:nrow(DT_f1)){
  if (!exists("DT_neg")){
    DT_neg <- subset(DT_check, DT_f1$Total_Basket_Value==DT_check$Total_Basket_Value & DT_f1$Retailer_Store_Number==DT_check$Retailer_Store_Number & DT_f1$day==DT_check$day)
  }
  if (exists("DT_neg")){
    temp_dataset <- subset(DT_check, DT_f1$Total_Basket_Value==DT_check$Total_Basket_Value & DT_f1$Retailer_Store_Number==DT_check$Retailer_Store_Number & DT_f1$day==DT_check$day)
    DT_neg <-rbind(DT_neg, temp_dataset)
    rm(temp_dataset)
  }

}





hist(DT$Total_Basket_Value)
summary(DT$Total_Basket_Value, rm.na=T)



DT_cl <- subset(DT, DT$Total_Basket_Value > 0)

summary(DT_cl$Total_Basket_Value)
hist(DT_f1$Total_Basket_Value, xlim = range(-1000,0), nclass = 100, col = "grey", main = NULL)
hist(DT_cl$Retailer_Store_Number)
hist(DT_cl$Total_Basket_Value, xlim = range(0,500), nclass = 1000, col = "grey", main = NULL)
hist(DT_cl$log_value, xlim = range(0,10), nclass = 100, col = "grey", main = NULL)
hist(DT_cl$log_value, xlim = range(0,10), nclass = 40, col = "grey", main = NULL)
hist(DT_cl$Total_Unique_Count, xlim = range(0,20), nclass = 100, col = "grey", main = NULL)
hist(DT_cl$Total_Item_Count, xlim = range(0,20), nclass = 700, col = "grey", main = NULL)
hist(DT_cl$Total_Item_Count, xlim = range(0,10), nclass = 200, col = "grey", main = NULL)

DT_susp <- subset(DT_cl, DT_cl$Total_Basket_Value < 1)
DT_susp2 <- subset(DT_cl, DT_cl$Tax < 0)
DT_cl <- subset(DT_cl, DT_cl$Tax > 0)

DT_cl["log_value"] <- log(DT_cl$Total_Basket_Value)



summary(DT_cl$year)
summary(DT_cl$month)
summary(DT_cl$day)
summary(DT_cl$hour)

hist(DT_cl$day, col = "grey", main = NULL)
hist(DT_cl$hour, col = " grey", main = NULL)

summary(DT)
summary(DT_cl)




