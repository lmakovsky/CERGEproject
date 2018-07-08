#CERGE project

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
library(circlize)
library(stringr)
library(stringi)

##Uploading list of product types
(list.files("DATA/") %>%
    grep("pro_ross",.) %>%
    list.files("DATA/", full.names = TRUE)[.] -> file_list)

rm(dataset)

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file, header=TRUE, sep="|", dec=",", encoding = "latin")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.table(file, header=TRUE, sep="|", dec=",", encoding = "latin")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

#creating goods categories
summary(dataset$Retailer_Product_Description)
dataset <- unique(dataset)
dataset["type"] <- paste(dataset$category, dataset$subcategory_group, dataset$family_group, sep = "_")


rm(DT)

##Uploading shopping baskets
(list.files("DATA/") %>%
    grep("bas_ross",.) %>%
    list.files("DATA/", full.names = TRUE)[.] -> file_list)

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("DT")){
    DT <- read.table(file, header=TRUE, sep="|", dec=",", encoding = "latin")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("DT")){
    temp_dataset <-read.table(file, header=TRUE, sep="|", dec=",", encoding = "latin")
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

#stores size
SQM <-read.table("DATA/SQMu.txt", header=TRUE, sep=";", dec=",")

#location_labels
LOC <- read.csv(file="DATA/stores.csv", sep=";", dec=".", encoding = "latin")
LOC["loc"] <- paste(LOC$mesto, LOC$ulice, sep = "_")
colnames(LOC)[colnames(LOC)=="store_id"] <- "Retailer_Store_Number"
DTt_price_shop <- merge(DTt_price_shop, LOC, by="Retailer_Store_Number")


#creating data sample
set.seed(123)
nrow(DT) %>% {sample(.,. * 0.01)} -> index
DT_train <- DT[index,]
DT_train <- subset(DT_train, Total_Basket_Value > 0)

summary(DT_train)
hist(DT$daytime_id)

#simple basket statistics on store level
DTt_price_shop <- DT %>%
  group_by(Retailer_Store_Number) %>%
  summarise(mean_value = mean(Total_Basket_Value, na.rm = TRUE),
            sd_value = sd(Total_Basket_Value, na.rm=TRUE),
            mean_items = mean(Total_Item_Count, na.rm=TRUE),
            sd_items = sd(Total_Item_Count, na.rm=TRUE),
            mean_hour = mean(hour, na.rm=TRUE),
            count = n())

#statistics - shopping time
DT_hour <- DT %>% group_by(hour) %>% 
  summarise(mean_value = mean(Total_Basket_Value, na.rm=TRUE), 
            sd_value = sd(Total_Basket_Value, na.rm=TRUE),
            mean_items = mean(Total_Item_Count, na.rm=TRUE),
            sd_items = sd (Total_Item_Count, na.rm=TRUE),
            count = n())

#average basket value by hour
plot(DT_hour$hour, DT_hour$mean_value, 
     type = "l", main="Average total basket value", 
     xlab="Hour", ylab="Value [CZK]", col="blue")


#time-pattern in minutes sections
DT_daytime <- DT %>% group_by(daytime_id) %>% 
  summarise(mean_value = mean(Total_Basket_Value, na.rm=TRUE), 
            sd_value = sd(Total_Basket_Value, na.rm=TRUE),
            count = n(),
            mean_items = mean(Total_Item_Count, na.rm=TRUE),
            sd_items = sd(Total_Item_Count, na.rm=TRUE))


#merge with stores size
colnames(SQM)[colnames(SQM)=="store_id"] <- "Retailer_Store_Number"
DTt_price_shop <- merge(DTt_price_shop, SQM, by="Retailer_Store_Number")

#revenue per meter
DTt_price_shop["revenue"] <- DTt_price_shop$mean_value*DTt_price_shop$count
DTt_price_shop["rev_month_meter"] <- DTt_price_shop$revenue/(12*DTt_price_shop$SQM)

DTt_price_shop <- DTt_price_shop[order(DTt_price_shop[,2],decreasing=TRUE),]

#stores segmantation based on basket structure
DTt_segment_shop <- DT %>%
  group_by(Retailer_Store_Number) %>%
  summarise(mean_value2 = mean(Total_Basket_Value[Total_Item_Count<3], na.rm = TRUE),
            mean_value5 = mean(Total_Basket_Value[Total_Item_Count<6 & Total_Item_Count>2], na.rm = TRUE),
            mean_value6 = mean(Total_Basket_Value[Total_Item_Count>5], na.rm = TRUE),
            return2 = sum(Total_Basket_Value[Total_Item_Count<3], na.rm = TRUE),
            return5 = sum(Total_Basket_Value[Total_Item_Count<6 & Total_Item_Count>2], na.rm = TRUE),
            return6 = sum(Total_Basket_Value[Total_Item_Count>5], na.rm = TRUE))

DTt_segment_shop["count2"] <- DTt_segment_shop$return2/DTt_segment_shop$mean_value2
DTt_segment_shop["count5"] <- DTt_segment_shop$return5/DTt_segment_shop$mean_value5
DTt_segment_shop["count6"] <- DTt_segment_shop$return6/DTt_segment_shop$mean_value6
DTt_segment_shop["count"] <- DTt_segment_shop$count2 + DTt_segment_shop$count5 + DTt_segment_shop$count6

DTt_segment_shop["return"] <- DTt_segment_shop$return2 + DTt_segment_shop$return5 + DTt_segment_shop$return6

DTt_segment_shop["count2_share"] <- DTt_segment_shop$count2/DTt_segment_shop$count 
DTt_segment_shop["count5_share"] <- DTt_segment_shop$count5/DTt_segment_shop$count 
DTt_segment_shop["count6_share"] <- DTt_segment_shop$count6/DTt_segment_shop$count 

DTt_segment_shop["log_return"] <- log(DTt_segment_shop$return)
DTt_segment_shop["log_count2"] <- log(DTt_segment_shop$count2)
DTt_segment_shop["log_count5"] <- log(DTt_segment_shop$count5)
DTt_segment_shop["log_count6"] <- log(DTt_segment_shop$count6)

DTt_segment_shop["mean_item_price"] <- DTt_segment_shop$mean_value/DTt_segment_shop$mean_items

DTt_price_shop["log_rev_month_meter"] <- log(DTt_price_shop$rev_month_meter)
summary(DTt_segment_shop$mean_item_price)

DTt_segment_shop <- merge(DTt_segment_shop, DTt_price_shop, by="Retailer_Store_Number")

#revenue per meter plot
plot(DTt_segment_shop$SQM, DTt_segment_shop$rev_month_meter, main="Monthly revenue per square meter", xlab="Store area [m2]", ylab="Monthly revenue [Kč/m2]")

#revenue per plot, basket segmentation
summary(DTt_segment_shop$count2_share)
summary(DTt_segment_shop$count5_share)
summary(DTt_segment_shop$count6_share)

#revenue per square meter
plot(DTt_segment_shop$SQM[DTt_segment_shop$count2_share<0.46], DTt_segment_shop$rev_month_meter[DTt_segment_shop$count2_share<0.46], 
     main="Monthly revenue per square meter", xlab="Store area [m2]", ylab="Monthly revenue [Kč/m2]", col="lightgray", pch=16, 
     xlim=range(min(DTt_segment_shop$SQM), max(DTt_segment_shop$SQM)), ylim=range(min(DTt_segment_shop$rev_month_meter), max(DTt_segment_shop$rev_month_meter)))
points(DTt_segment_shop$SQM[DTt_segment_shop$count2_share<0.5&DTt_segment_shop$count2_share>=0.46], DTt_segment_shop$rev_month_meter[DTt_segment_shop$count2_share<0.5&DTt_segment_shop$count2_share>=0.46], 
       col="cyan", pch=16)
points(DTt_segment_shop$SQM[DTt_segment_shop$count2_share<0.52&DTt_segment_shop$count2_share>=0.5], DTt_segment_shop$rev_month_meter[DTt_segment_shop$count2_share<0.52&DTt_segment_shop$count2_share>=0.5], 
       col="orange", pch=16)
points(DTt_segment_shop$SQM[DTt_segment_shop$count2_share>=0.52], DTt_segment_shop$rev_month_meter[DTt_segment_shop$count2_share>=0.52], 
       col="red", pch=16)

#revenue total
plot(DTt_segment_shop$SQM[DTt_segment_shop$count6_share<0.17], DTt_segment_shop$revenue[DTt_segment_shop$count6_share<0.17], 
     main="Total revenue", xlab="Store area [m2]", ylab="Total revenue [Kč]", col="lightgray", pch=16)
points(DTt_segment_shop$SQM[DTt_segment_shop$count6_share<0.2&DTt_segment_shop$count6_share>=0.17], DTt_segment_shop$revenue[DTt_segment_shop$count6_share<0.2&DTt_segment_shop$count6_share>=0.17], 
       col="cyan", pch=16)
points(DTt_segment_shop$SQM[DTt_segment_shop$count6_share<0.22&DTt_segment_shop$count6_share>=0.2], DTt_segment_shop$revenue[DTt_segment_shop$count6_share<0.22&DTt_segment_shop$count6_share>=0.2], 
       col="orange", pch=16)
points(DTt_segment_shop$SQM[DTt_segment_shop$count6_share>=0.22], DTt_segment_shop$revenue[DTt_segment_shop$count6_share>=0.22], 
       col="red", pch=16)

#clustering shops based on the types of their baskets
shops_segs1 <- DTt_segment_shop[,c("count2_share","count5_share", "count6_share", "mean_value", "count.y", "SQM")]
shops_segs1 <- scale(shops_segs1, center = TRUE, scale = TRUE)

# Determine number of clusters
wss <- (nrow(shops_segs1)-1)*sum(apply(shops_segs1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(shops_segs1, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# K-Means Cluster Analysis
fit <- kmeans(shops_segs1, 3) # 3 cluster solution
# get cluster means 
aggregate(shops_segs1,by=list(fit$cluster),FUN=mean)
# append cluster assignment
shops_segs1 <- data.frame(shops_segs1, fit$cluster)

# K-Means Cluster Analysis
fit <- kmeans(shops_segs1, 6) # 6 cluster solution
# get cluster means 
aggregate(shops_segs1,by=list(fit$cluster),FUN=mean)
# append cluster assignment
shops_segs1 <- data.frame(shops_segs1, fit$cluster)

#assigning cluster solution to main dataset
DTt_segment_shop["cluster3"] <- shops_segs1$fit.cluster
DTt_segment_shop["cluster6"] <- shops_segs1$fit.cluster.1

#3cluster descriptive statistics
STc1 <- DTt_segment_shop %>%
  group_by(cluster3) %>%
  summarise(mean_count = mean(count.x, na.rm = TRUE),
            mean_count2_share = mean(count2_share, na.rm = TRUE),
            mean_count5_share = mean(count5_share, na.rm = TRUE),
            mean_count6_share = mean(count6_share, na.rm = TRUE),
            mean_revenue_month_meter = mean(rev_month_meter, na.rm = TRUE),
            mean_sqm = mean(SQM, na.rm = TRUE),
            mean_revenue = mean(revenue, na.rm = TRUE),
            mean_value = mean(mean_value, na.rm = TRUE),
            count = n())

#6cluster descriptive statistics
STc2 <- DTt_segment_shop %>%
  group_by(cluster6) %>%
  summarise(mean_count = mean(count.x, na.rm = TRUE),
            mean_count2_share = mean(count2_share, na.rm = TRUE),
            mean_count5_share = mean(count5_share, na.rm = TRUE),
            mean_count6_share = mean(count6_share, na.rm = TRUE),
            mean_revenue_month_meter = mean(rev_month_meter, na.rm = TRUE),
            mean_sqm = mean(SQM, na.rm = TRUE), 
            mean_revenue = mean(revenue, na.rm = TRUE),
            mean_value = mean(mean_value, na.rm = TRUE),
            count = n())

#colors_for_clusters
col.cl1 <- "#E64428"
col.cl2 <- "#4DBBE8"
col.cl3 <- "#1561AE"
col.cl4 <- "#A6CB59"
col.cl5 <- "#AA66A9"
col.cl6 <- "#FFC226"

#barplot of mean items in basket in stores
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

#value/items plot
par(mfrow=c(1,1))
plot(DTt_price_shop$mean_value, DTt_price_shop$mean_items, 
     main="Items vs. price", xlab="Average basket value", ylab="Average number of items")


#distance to highway regression
DIST <- read.csv(file="DATA_CREATED/highway_distance.csv",
                 sep=",", dec=".", encoding = "latin")

DIST["HubDist"] <- DIST$HubDist/1000
DIST["logHubDist"] <- log(DIST$HubDist)
summary(lm(DIST$R_mean_ite ~ DIST$HubDist + DIST$R_mean_val))
summary(lm(DIST$R_mean_ite ~ DIST$logHubDist + DIST$R_mean_val))

#basket shares on return regression (lin-lin)
summary(lm(DTt_segment_shop$return ~ DTt_segment_shop$count2_share + DTt_segment_shop$count5_share))

#basket shares on return regression (log-lin)
summary(lm(DTt_segment_shop$log_return ~ DTt_segment_shop$count2_share + DTt_segment_shop$count5_share))
#the highest magnitude comes from share of middle baskets (but it is in percentage points and these have lower share), seems it is wrong specification

#basket shares on return regression (log-log)
summary(lm(DTt_segment_shop$log_return ~ DTt_segment_shop$log_count2 + DTt_segment_shop$log_count5 + DTt_segment_shop$log_count6))
#the highest magnitude comes from large baskets

#basket shares on return per square meter regression (log-log)
summary(lm(DTt_segment_shop$log_rev_month_meter ~ DTt_segment_shop$log_count2 + DTt_segment_shop$log_count5 + DTt_segment_shop$log_count6))
#the highest magnitude comes from large baskets

#dendogram
d_shops <- dist(shops_segs1) # method="man" # is a bit better
hc_shops <- hclust(d_shops, method = "complete")
shops_id <- rev(levels(DTt_segment_shop[,"mesto"]))


dend <- as.dendrogram(hc_shops)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:134)

# We shall add the flower type to the labels:
labels(dend) <- paste(as.character(DTt_segment_shop[,"mesto"])[order.dendrogram(dend)],
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
     main = "Clustered stores", 
     horiz =  TRUE,  nodePar = list(cex = .007))


#write CSV file
write.table(DTt_price_shop, file = "DATA_CREATED/DTt_price_shop.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")
write.table(DTt_segment_shop, file = "DATA_CREATED/DTt_segment_shop.xls",row.names=FALSE, na="",col.names=TRUE, sep=",")


##Uploading item list
(list.files("DATA/") %>%
    grep("purchase_ross",.) %>%
    list.files("DATA/", full.names = TRUE)[.] -> file_list)

rm(ITM)

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("ITM")){
    ITM <- read.table(file, header=TRUE, sep="|", dec=",", encoding = "latin")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("ITM")){
    temp_dataset <-read.table(file, header=TRUE, sep="|", dec=",", encoding = "latin")
    ITM<-rbind(ITM, temp_dataset)
    rm(temp_dataset)
  }
  
}

ITM <- merge(ITM, dataset, by="Retailer_Product_ID")

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
ITMt_items <- ITM %>% group_by(Retailer_Product_ID) %>% 
  summarise(sum_itm = n())

ITMt_items <- merge(ITMt_items, dataset, by="Retailer_Product_ID")

#group by items types
ITMt_types <- ITM %>% group_by(type) %>% 
  summarise(sum_type = n())

arrange(ITMt_types, desc(sum_type))

#basket with negative values
DT_f1 <- subset(DT, DT$Total_Basket_Value < 0)
DT_check <- DT


#association analysis - matrix creation
#copying all items data into new dataset
AS <- ITM
#selecting 2 association features - baskets and product types bought 
AS %<>% 
  select("Retailer_Basket_ID", "type")

## create a product table
prodTable <- AS %>%
  group_by(type) %>%
  summarise(count = n())
prodTable["prodID"] <- c(1:nrow(prodTable))
prodTable["count"] <- NULL

## create a transaction table 
transTable <- AS %>%
  group_by(Retailer_Basket_ID) %>%
  summarise(count = n())
transTable["transID"] <- c(1:nrow(transTable))
transTable["count"] <- NULL

## bind to the original table
AS <- AS %>% right_join(prodTable, by = "type")
AS <- merge(x=AS, y=transTable, all.x=TRUE, by="Retailer_Basket_ID")

class(AS$prodID)
class(AS$transID)

## create sparse matrix based on IDs

AS <- sparseMatrix(j = AS$transID,
                         i = AS$prodID)


rownames(AS) <- prodTable$type
colnames(AS) <- transTable$Retailer_Basket_ID

model <- apriori(AS, parameter = list(support = 0.001, confidence = 0.1))

model %>% inspect() %>% as.data.frame()

rules <- cbind(labels = labels(model), model@quality)

rules$lhs <- gsub("=>.*","", rules$labels)
rules$rhs <- gsub(".*=>","", rules$labels)

rules <- rules[,c("lhs","rhs","support","confidence","lift", "count")]

View(rules)

CIRC1 <- rules  %>% select("lhs", "rhs", "confidence")

#remove {}
CIRC1["lhs"] <- str_sub(CIRC1$lhs, 2, -3)
CIRC1["rhs"] <- str_sub(CIRC1$rhs, 2, -2)
CIRC1["rhs"] <- str_sub(CIRC1$rhs, 2, -1)


#circlize
#base_col = c("#00aeef", "#b21dac", "#8dc63f", "#ffb100", "#dc0015", "#000000", "#a6a6a6")
circos.clear()
chordDiagram(CIRC1,
            grid.col = "grey",
            grid.border = NULL,
            directional = 1,
            self.link = 2,
            diffHeight = F,
            preAllocateTracks = 1)
            
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
              xlim = get.cell.meta.data("xlim")
              ylim = get.cell.meta.data("ylim")
              sector.name = get.cell.meta.data("sector.index")
              circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.2)
              })
            
circos.clear()

#circos code rest
#annotationTrack = c("name","grid"),
#circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)

#association within clusters
ITM <- merge(x=ITM, y=stores_clust, all.x=TRUE, by="Retailer_Store_Number")

#cluster baskets subset
ITMcl1 <- subset(ITM, cluster6==1)
ITMcl2 <- subset(ITM, cluster6==2)
ITMcl3 <- subset(ITM, cluster6==3)
ITMcl4 <- subset(ITM, cluster6==4)
ITMcl5 <- subset(ITM, cluster6==5)
ITMcl6 <- subset(ITM, cluster6==6)

#association analysis - cluster 1
#copying all items data into new dataset
AS1 <- ITMcl1
#selecting 2 association features - baskets and product types bought 
AS1 %<>% 
  select("Retailer_Basket_ID", "type")

## create a product table
prodTable1 <- AS1 %>%
  group_by(type) %>%
  summarise(count = n())
prodTable1["prodID"] <- c(1:nrow(prodTable1))
prodTable1["count"] <- NULL

## create a transaction table 
transTable1 <- AS1 %>%
  group_by(Retailer_Basket_ID) %>%
  summarise(count = n())
transTable1["transID"] <- c(1:nrow(transTable1))
transTable1["count"] <- NULL

## bind to the original table
AS1 <- AS1 %>% right_join(prodTable1, by = "type")
AS1 <- merge(x=AS1, y=transTable1, all.x=TRUE, by="Retailer_Basket_ID")

class(AS1$prodID)
class(AS1$transID)

## create sparse matrix based on IDs

AS1 <- sparseMatrix(j = AS1$transID,
                   i = AS1$prodID)


rownames(AS1) <- prodTable1$type
colnames(AS1) <- transTable1$Retailer_Basket_ID

model1 <- apriori(AS1, parameter = list(support = 0.001, confidence = 0.1))

model1 %>% inspect() %>% as.data.frame()

rules1 <- cbind(labels = labels(model1), model1@quality)

rules1$lhs <- gsub("=>.*","", rules1$labels)
rules1$rhs <- gsub(".*=>","", rules1$labels)

rules1 <- rules1[,c("lhs","rhs","support","confidence","lift", "count")]

View(rules1)

CIR1 <- rules1  %>% select("lhs", "rhs", "confidence")

#remove {}
CIR1["lhs"] <- str_sub(CIR1$lhs, 2, -3)
CIR1["rhs"] <- str_sub(CIR1$rhs, 2, -2)
CIR1["rhs"] <- str_sub(CIR1$rhs, 2, -1)


#circlize
#base_col = c("#00aeef", "#b21dac", "#8dc63f", "#ffb100", "#dc0015", "#000000", "#a6a6a6")
circos.clear()
chordDiagram(CIR1,
             grid.col = "grey",
             grid.border = NULL,
             directional = 1,
             self.link = 2,
             diffHeight = F,
             preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.2)
})

circos.clear()


#association analysis - cluster 2
#copying all items data into new dataset
AS2 <- ITMcl2
#selecting 2 association features - baskets and product types bought 
AS2 %<>% 
  select("Retailer_Basket_ID", "type")

## create a product table
prodTable2 <- AS2 %>%
  group_by(type) %>%
  summarise(count = n())
prodTable2["prodID"] <- c(1:nrow(prodTable2))
prodTable2["count"] <- NULL

## create a transaction table 
transTable2 <- AS2 %>%
  group_by(Retailer_Basket_ID) %>%
  summarise(count = n())
transTable2["transID"] <- c(1:nrow(transTable2))
transTable2["count"] <- NULL

## bind to the original table
AS2 <- AS2 %>% right_join(prodTable2, by = "type")
AS2 <- merge(x=AS2, y=transTable2, all.x=TRUE, by="Retailer_Basket_ID")

class(AS2$prodID)
class(AS2$transID)

## create sparse matrix based on IDs

AS2 <- sparseMatrix(j = AS2$transID,
                    i = AS2$prodID)


rownames(AS2) <- prodTable2$type
colnames(AS2) <- transTable2$Retailer_Basket_ID

model2 <- apriori(AS2, parameter = list(support = 0.001, confidence = 0.1))

model2 %>% inspect() %>% as.data.frame()

rules2 <- cbind(labels = labels(model2), model2@quality)

rules2$lhs <- gsub("=>.*","", rules2$labels)
rules2$rhs <- gsub(".*=>","", rules2$labels)

rules2 <- rules2[,c("lhs","rhs","support","confidence","lift", "count")]

View(rules2)

CIR2 <- rules2  %>% select("lhs", "rhs", "confidence")

#remove {}
CIR2["lhs"] <- str_sub(CIR2$lhs, 2, -3)
CIR2["rhs"] <- str_sub(CIR2$rhs, 2, -2)
CIR2["rhs"] <- str_sub(CIR2$rhs, 2, -1)


#circlize
#base_col = c("#00aeef", "#b21dac", "#8dc63f", "#ffb100", "#dc0015", "#000000", "#a6a6a6")
circos.clear()
chordDiagram(CIR2,
             grid.col = "grey",
             grid.border = NULL,
             directional = 1,
             self.link = 2,
             diffHeight = F,
             preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.2)
})

circos.clear()

#association analysis - cluster 3
#copying all items data into new dataset
AS3 <- ITMcl3
#selecting 2 association features - baskets and product types bought 
AS3 %<>% 
  select("Retailer_Basket_ID", "type")

## create a product table
prodTable3 <- AS3 %>%
  group_by(type) %>%
  summarise(count = n())
prodTable3["prodID"] <- c(1:nrow(prodTable3))
prodTable3["count"] <- NULL

## create a transaction table 
transTable3 <- AS3 %>%
  group_by(Retailer_Basket_ID) %>%
  summarise(count = n())
transTable3["transID"] <- c(1:nrow(transTable3))
transTable3["count"] <- NULL

## bind to the original table
AS3 <- AS3 %>% right_join(prodTable3, by = "type")
AS3 <- merge(x=AS3, y=transTable3, all.x=TRUE, by="Retailer_Basket_ID")

class(AS3$prodID)
class(AS3$transID)

## create sparse matrix based on IDs

AS3 <- sparseMatrix(j = AS3$transID,
                    i = AS3$prodID)


rownames(AS3) <- prodTable3$type
colnames(AS3) <- transTable3$Retailer_Basket_ID

model3 <- apriori(AS3, parameter = list(support = 0.001, confidence = 0.1))

model3 %>% inspect() %>% as.data.frame()

rules3 <- cbind(labels = labels(model3), model3@quality)

rules3$lhs <- gsub("=>.*","", rules3$labels)
rules3$rhs <- gsub(".*=>","", rules3$labels)

rules3 <- rules3[,c("lhs","rhs","support","confidence","lift", "count")]

View(rules3)

CIR3 <- rules3  %>% select("lhs", "rhs", "confidence")

#remove {}
CIR3["lhs"] <- str_sub(CIR3$lhs, 2, -3)
CIR3["rhs"] <- str_sub(CIR3$rhs, 2, -2)
CIR3["rhs"] <- str_sub(CIR3$rhs, 2, -1)


#circlize
#base_col = c("#00aeef", "#b21dac", "#8dc63f", "#ffb100", "#dc0015", "#000000", "#a6a6a6")
circos.clear()
chordDiagram(CIR3,
             grid.col = "grey",
             grid.border = NULL,
             directional = 1,
             self.link = 2,
             diffHeight = F,
             preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.2)
})

circos.clear()

#association analysis - cluster 4
#copying all items data into new dataset
AS4 <- ITMcl4
#selecting 2 association features - baskets and product types bought 
AS4 %<>% 
  select("Retailer_Basket_ID", "type")

## create a product table
prodTable4 <- AS4 %>%
  group_by(type) %>%
  summarise(count = n())
prodTable4["prodID"] <- c(1:nrow(prodTable4))
prodTable4["count"] <- NULL

## create a transaction table 
transTable4 <- AS4 %>%
  group_by(Retailer_Basket_ID) %>%
  summarise(count = n())
transTable4["transID"] <- c(1:nrow(transTable4))
transTable4["count"] <- NULL

## bind to the original table
AS4 <- AS4 %>% right_join(prodTable4, by = "type")
AS4 <- merge(x=AS4, y=transTable4, all.x=TRUE, by="Retailer_Basket_ID")

class(AS4$prodID)
class(AS4$transID)

## create sparse matrix based on IDs

AS4 <- sparseMatrix(j = AS4$transID,
                    i = AS4$prodID)


rownames(AS4) <- prodTable4$type
colnames(AS4) <- transTable4$Retailer_Basket_ID

model4 <- apriori(AS4, parameter = list(support = 0.001, confidence = 0.1))

model4 %>% inspect() %>% as.data.frame()

rules4 <- cbind(labels = labels(model4), model4@quality)

rules4$lhs <- gsub("=>.*","", rules4$labels)
rules4$rhs <- gsub(".*=>","", rules4$labels)

rules4 <- rules4[,c("lhs","rhs","support","confidence","lift", "count")]

View(rules4)

CIR4 <- rules4  %>% select("lhs", "rhs", "confidence")

#remove {}
CIR4["lhs"] <- str_sub(CIR4$lhs, 2, -3)
CIR4["rhs"] <- str_sub(CIR4$rhs, 2, -2)
CIR4["rhs"] <- str_sub(CIR4$rhs, 2, -1)


#circlize
#base_col = c("#00aeef", "#b21dac", "#8dc63f", "#ffb100", "#dc0015", "#000000", "#a6a6a6")
circos.clear()
chordDiagram(CIR4,
             grid.col = "grey",
             grid.border = NULL,
             directional = 1,
             self.link = 2,
             diffHeight = F,
             preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.2)
})

circos.clear()


#association analysis - cluster 5
#copying all items data into new dataset
AS5 <- ITMcl5
#selecting 2 association features - baskets and product types bought 
AS5 %<>% 
  select("Retailer_Basket_ID", "type")

## create a product table
prodTable5 <- AS5 %>%
  group_by(type) %>%
  summarise(count = n())
prodTable5["prodID"] <- c(1:nrow(prodTable5))
prodTable5["count"] <- NULL

## create a transaction table 
transTable5 <- AS5 %>%
  group_by(Retailer_Basket_ID) %>%
  summarise(count = n())
transTable5["transID"] <- c(1:nrow(transTable5))
transTable5["count"] <- NULL

## bind to the original table
AS5 <- AS5 %>% right_join(prodTable5, by = "type")
AS5 <- merge(x=AS5, y=transTable5, all.x=TRUE, by="Retailer_Basket_ID")

class(AS5$prodID)
class(AS5$transID)

## create sparse matrix based on IDs

AS5 <- sparseMatrix(j = AS5$transID,
                    i = AS5$prodID)


rownames(AS5) <- prodTable5$type
colnames(AS5) <- transTable5$Retailer_Basket_ID

model5 <- apriori(AS5, parameter = list(support = 0.001, confidence = 0.1))

model5 %>% inspect() %>% as.data.frame()

rules5 <- cbind(labels = labels(model5), model5@quality)

rules5$lhs <- gsub("=>.*","", rules5$labels)
rules5$rhs <- gsub(".*=>","", rules5$labels)

rules5 <- rules5[,c("lhs","rhs","support","confidence","lift", "count")]

View(rules5)

CIR5 <- rules5  %>% select("lhs", "rhs", "confidence")

#remove {}
CIR5["lhs"] <- str_sub(CIR5$lhs, 2, -3)
CIR5["rhs"] <- str_sub(CIR5$rhs, 2, -2)
CIR5["rhs"] <- str_sub(CIR5$rhs, 2, -1)


#circlize
#base_col = c("#00aeef", "#b21dac", "#8dc63f", "#ffb100", "#dc0015", "#000000", "#a6a6a6")
circos.clear()
chordDiagram(CIR5,
             grid.col = "grey",
             grid.border = NULL,
             directional = 1,
             self.link = 2,
             diffHeight = F,
             preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.2)
})

circos.clear()


#association analysis - cluster 6
#copying all items data into new dataset
AS6 <- ITMcl6
#selecting 2 association features - baskets and product types bought 
AS6 %<>% 
  select("Retailer_Basket_ID", "type")

## create a product table
prodTable6 <- AS6 %>%
  group_by(type) %>%
  summarise(count = n())
prodTable6["prodID"] <- c(1:nrow(prodTable6))
prodTable6["count"] <- NULL

## create a transaction table 
transTable6 <- AS6 %>%
  group_by(Retailer_Basket_ID) %>%
  summarise(count = n())
transTable6["transID"] <- c(1:nrow(transTable6))
transTable6["count"] <- NULL

## bind to the original table
AS6 <- AS6 %>% right_join(prodTable6, by = "type")
AS6 <- merge(x=AS6, y=transTable6, all.x=TRUE, by="Retailer_Basket_ID")

class(AS6$prodID)
class(AS6$transID)

## create sparse matrix based on IDs

AS6 <- sparseMatrix(j = AS6$transID,
                    i = AS6$prodID)


rownames(AS6) <- prodTable6$type
colnames(AS6) <- transTable6$Retailer_Basket_ID

model6 <- apriori(AS6, parameter = list(support = 0.001, confidence = 0.1))

model6 %>% inspect() %>% as.data.frame()

rules6 <- cbind(labels = labels(model6), model6@quality)

rules6$lhs <- gsub("=>.*","", rules6$labels)
rules6$rhs <- gsub(".*=>","", rules6$labels)

rules6 <- rules6[,c("lhs","rhs","support","confidence","lift", "count")]

View(rules6)

CIR6 <- rules6  %>% select("lhs", "rhs", "confidence")

#remove {}
CIR6["lhs"] <- str_sub(CIR6$lhs, 2, -3)
CIR6["rhs"] <- str_sub(CIR6$rhs, 2, -2)
CIR6["rhs"] <- str_sub(CIR6$rhs, 2, -1)


#circlize
#base_col = c("#00aeef", "#b21dac", "#8dc63f", "#ffb100", "#dc0015", "#000000", "#a6a6a6")
circos.clear()
chordDiagram(CIR6,
             grid.col = "grey",
             grid.border = NULL,
             directional = 1,
             self.link = 2,
             diffHeight = F,
             preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.2)
})

circos.clear()

#number of associations in each cluster
nrow(CIR1)
nrow(CIR2)
nrow(CIR3)
nrow(CIR4)
nrow(CIR5)
nrow(CIR6)

#diferences between clusters, adjusting rules
CIR1e <- CIR1
CIR1e["rule"] <- paste(CIR1$lhs, CIR1$rhs, sep = "|")
CIR1e$lhs <- NULL
CIR1e$rhs <- NULL
colnames(CIR1e)[colnames(CIR1e)=="confidence"] <- "confidence_1"

CIR2e <- CIR2
CIR2e["rule"] <- paste(CIR2$lhs, CIR2$rhs, sep = "|")
CIR2e$lhs <- NULL
CIR2e$rhs <- NULL
colnames(CIR2e)[colnames(CIR2e)=="confidence"] <- "confidence_2"

CIR3e <- CIR3
CIR3e["rule"] <- paste(CIR3$lhs, CIR3$rhs, sep = "|")
CIR3e$lhs <- NULL
CIR3e$rhs <- NULL
colnames(CIR3e)[colnames(CIR3e)=="confidence"] <- "confidence_3"

CIR4e <- CIR4
CIR4e["rule"] <- paste(CIR4$lhs, CIR4$rhs, sep = "|")
CIR4e$lhs <- NULL
CIR4e$rhs <- NULL
colnames(CIR4e)[colnames(CIR4e)=="confidence"] <- "confidence_4"

CIR5e <- CIR5
CIR5e["rule"] <- paste(CIR5$lhs, CIR5$rhs, sep = "|")
CIR5e$lhs <- NULL
CIR5e$rhs <- NULL
colnames(CIR5e)[colnames(CIR5e)=="confidence"] <- "confidence_5"

CIR6e <- CIR6
CIR6e["rule"] <- paste(CIR6$lhs, CIR6$rhs, sep = "|")
CIR6e$lhs <- NULL
CIR6e$rhs <- NULL
colnames(CIR6e)[colnames(CIR6e)=="confidence"] <- "confidence_6"

#cluster diferences, cluster results merge
CIRe <- merge(x=CIR1e, y=CIR2e, all.x=TRUE, all.y=TRUE, by="rule")
CIRe <- merge(x=CIRe, y=CIR3e, all.x=TRUE, all.y=TRUE, by="rule")
CIRe <- merge(x=CIRe, y=CIR4e, all.x=TRUE, all.y=TRUE, by="rule")
CIRe <- merge(x=CIRe, y=CIR5e, all.x=TRUE, all.y=TRUE, by="rule")
CIRe <- merge(x=CIRe, y=CIR6e, all.x=TRUE, all.y=TRUE, by="rule")

CIRe["avg"] <- (CIRe$confidence_1 + CIRe$confidence_2 + CIRe$confidence_3 + CIRe$confidence_4 + CIRe$confidence_5 + CIRe$confidence_6)/6
CIRe["dev"] <- (abs(CIRe$confidence_1 - CIRe$avg) + abs(CIRe$confidence_2 - CIRe$avg) + abs(CIRe$confidence_3 - CIRe$avg) + abs(CIRe$confidence_4 - CIRe$avg) +  abs(CIRe$confidence_5 - CIRe$avg) + abs(CIRe$confidence_6 - CIRe$avg))/CIRe$avg

#plot diferences in associations
CIRe <- CIRe[order(CIRe[,"dev"],decreasing=TRUE),]
CIRsum <- subset(CIRe, dev>0.5)

plot(as.factor(CIRsum$rule), CIRsum$confidence_1, las=3, cex=0.25, main="Association diferences among clusters", ylab="Association confidence", col=col.cl1, pch=16, ylim=range(0.1,0.4))
points(as.factor(CIRsum$rule), CIRsum$confidence_1, col=col.cl1, pch=16)
points(as.factor(CIRsum$rule), CIRsum$confidence_2, col=col.cl2, pch=16)
points(as.factor(CIRsum$rule), CIRsum$confidence_3, col=col.cl3, pch=16)
points(as.factor(CIRsum$rule), CIRsum$confidence_4, col=col.cl4, pch=16)
points(as.factor(CIRsum$rule), CIRsum$confidence_5, col=col.cl5, pch=16)
points(as.factor(CIRsum$rule), CIRsum$confidence_6, col=col.cl6, pch=16)

#cluster statistics
stores_clust <- DTt_segment_shop %>% select("Retailer_Store_Number", "cluster6")
DT <- merge(x=DT, y=stores_clust, all.x=TRUE, by="Retailer_Store_Number")

#cluster municipalities
stores_clust <- read.csv(file="DATA_CREATED/stores_clust_municipalities.csv", sep=",", dec=".", encoding = "latin")
stores_clust_sum <- stores_clust %>%
  group_by(Dcluster6) %>%
  summarise(count = n(),
            mean_pop = median(POCET_OBYV))


#cluster baskets subset
BSKcl1 <- subset(DT, cluster6==1)
BSKcl2 <- subset(DT, cluster6==2)
BSKcl3 <- subset(DT, cluster6==3)
BSKcl4 <- subset(DT, cluster6==4)
BSKcl5 <- subset(DT, cluster6==5)
BSKcl6 <- subset(DT, cluster6==6)

#shopping time in clusters
#statistics - shopping time
TIMEcl1 <- BSKcl1 %>% group_by(hour) %>% 
  summarise(mean_value = mean(Total_Basket_Value, na.rm=TRUE), 
            mean_items = mean(Total_Item_Count, na.rm=TRUE),
            count = n())
TIMEcl1["nstores"] <- STc2[1,"count"]
TIMEcl1["count.p.store"] <- TIMEcl1$count/TIMEcl1$nstores

TIMEcl2 <- BSKcl2 %>% group_by(hour) %>% 
  summarise(mean_value = mean(Total_Basket_Value, na.rm=TRUE), 
            mean_items = mean(Total_Item_Count, na.rm=TRUE),
            count = n())
TIMEcl2["nstores"] <- STc2[2,"count"]
TIMEcl2["count.p.store"] <- TIMEcl2$count/TIMEcl2$nstores

TIMEcl3 <- BSKcl3 %>% group_by(hour) %>% 
  summarise(mean_value = mean(Total_Basket_Value, na.rm=TRUE), 
            mean_items = mean(Total_Item_Count, na.rm=TRUE),
            count = n())
TIMEcl3["nstores"] <- STc2[3,"count"]
TIMEcl3["count.p.store"] <- TIMEcl3$count/TIMEcl3$nstores

TIMEcl4 <- BSKcl4 %>% group_by(hour) %>% 
  summarise(mean_value = mean(Total_Basket_Value, na.rm=TRUE), 
            mean_items = mean(Total_Item_Count, na.rm=TRUE),
            count = n())
TIMEcl4["nstores"] <- STc2[4,"count"]
TIMEcl4["count.p.store"] <- TIMEcl4$count/TIMEcl4$nstores

TIMEcl5 <- BSKcl5 %>% group_by(hour) %>% 
  summarise(mean_value = mean(Total_Basket_Value, na.rm=TRUE), 
            mean_items = mean(Total_Item_Count, na.rm=TRUE),
            count = n())
TIMEcl5["nstores"] <- STc2[5,"count"]
TIMEcl5["count.p.store"] <- TIMEcl5$count/TIMEcl5$nstores

TIMEcl6 <- BSKcl6 %>% group_by(hour) %>% 
  summarise(mean_value = mean(Total_Basket_Value, na.rm=TRUE), 
            mean_items = mean(Total_Item_Count, na.rm=TRUE),
            count = n())
TIMEcl6["nstores"] <- STc2[6,"count"]
TIMEcl6["count.p.store"] <- TIMEcl6$count/TIMEcl6$nstores

#histogram of basket values, cathegories creation
BSKcl1["value_tens"] <- round(BSKcl1$Total_Basket_Value, digits = -1)
BSKpcl1 <- BSKcl1 %>% group_by(value_tens) %>% 
  summarise(count = n())
BSKpcl1 <- subset(BSKpcl1, value_tens>0)
BSKpcl1["count"] <- BSKpcl1$count/sum(BSKpcl1$count)

BSKcl2["value_tens"] <- round(BSKcl2$Total_Basket_Value, digits = -1)
BSKpcl2 <- BSKcl2 %>% group_by(value_tens) %>% 
  summarise(count = n())
BSKpcl2 <- subset(BSKpcl2, value_tens>0)
BSKpcl2["count"] <- BSKpcl2$count/sum(BSKpcl2$count)

BSKcl3["value_tens"] <- round(BSKcl3$Total_Basket_Value, digits = -1)
BSKpcl3 <- BSKcl3 %>% group_by(value_tens) %>% 
  summarise(count = n())
BSKpcl3 <- subset(BSKpcl3, value_tens>0)
BSKpcl3["count"] <- BSKpcl3$count/sum(BSKpcl3$count)

BSKcl4["value_tens"] <- round(BSKcl4$Total_Basket_Value, digits = -1)
BSKpcl4 <- BSKcl4 %>% group_by(value_tens) %>% 
  summarise(count = n())
BSKpcl4 <- subset(BSKpcl4, value_tens>0)
BSKpcl4["count"] <- BSKpcl4$count/sum(BSKpcl4$count)

BSKcl5["value_tens"] <- round(BSKcl5$Total_Basket_Value, digits = -1)
BSKpcl5 <- BSKcl5 %>% group_by(value_tens) %>% 
  summarise(count = n())
BSKpcl5 <- subset(BSKpcl5, value_tens>0)
BSKpcl5["count"] <- BSKpcl5$count/sum(BSKpcl5$count)

BSKcl6["value_tens"] <- round(BSKcl6$Total_Basket_Value, digits = -1)
BSKpcl6 <- BSKcl6 %>% group_by(value_tens) %>% 
  summarise(count = n())
BSKpcl6 <- subset(BSKpcl6, value_tens>0)
BSKpcl6["count"] <- BSKpcl6$count/sum(BSKpcl6$count)

#plot of histograms for stores
plot(BSKpcl1$value_tens, BSKpcl1$count, type="l", col=col.cl1, xlim=range(0,500), ylim=range(0.005,0.07),
     main="Basket value histogram", xlab="Basket value [Kč]", ylab="Share of baskets")
points(BSKpcl1$value_tens, BSKpcl1$count, type="l", col=col.cl1, lw=3)
points(BSKpcl2$value_tens, BSKpcl2$count, type="l", col=col.cl2, lw=3)
points(BSKpcl3$value_tens, BSKpcl3$count, type="l", col=col.cl3, lw=3)
points(BSKpcl4$value_tens, BSKpcl4$count, type="l", col=col.cl4, lw=3)
points(BSKpcl5$value_tens, BSKpcl5$count, type="l", col=col.cl5, lw=3)
points(BSKpcl6$value_tens, BSKpcl6$count, type="l", col=col.cl6, lw=3)

#histogram of basket items
BSKit1 <- BSKcl1 %>% group_by(Total_Unique_Count) %>% 
  summarise(count = n())
BSKit1["count"] <- BSKit1$count/sum(BSKit1$count)

BSKit2 <- BSKcl2 %>% group_by(Total_Unique_Count) %>% 
  summarise(count = n())
BSKit2["count"] <- BSKit2$count/sum(BSKit2$count)

BSKit3 <- BSKcl3 %>% group_by(Total_Unique_Count) %>% 
  summarise(count = n())
BSKit3["count"] <- BSKit3$count/sum(BSKit3$count)

BSKit4 <- BSKcl4 %>% group_by(Total_Unique_Count) %>% 
  summarise(count = n())
BSKit4["count"] <- BSKit4$count/sum(BSKit4$count)

BSKit5 <- BSKcl5 %>% group_by(Total_Unique_Count) %>% 
  summarise(count = n())
BSKit5["count"] <- BSKit5$count/sum(BSKit5$count)

BSKit6 <- BSKcl6 %>% group_by(Total_Unique_Count) %>% 
  summarise(count = n())
BSKit6["count"] <- BSKit6$count/sum(BSKit6$count)

#number of items in shopping basket histogram
plot(BSKit1$Total_Unique_Count, BSKit1$count, type="l", col=col.cl1, xlim=range(1,10), ylim=range(0,0.35),
     main="Basket size histogram", xlab="# unique items in basktet", ylab="Share of baskets")
points(BSKit1$Total_Unique_Count, BSKit1$count, type="l", col=col.cl1, lw=3)
points(BSKit2$Total_Unique_Count, BSKit2$count, type="l", col=col.cl2, lw=3)
points(BSKit3$Total_Unique_Count, BSKit3$count, type="l", col=col.cl3, lw=3)
points(BSKit4$Total_Unique_Count, BSKit4$count, type="l", col=col.cl4, lw=3)
points(BSKit5$Total_Unique_Count, BSKit5$count, type="l", col=col.cl5, lw=3)
points(BSKit6$Total_Unique_Count, BSKit6$count, type="l", col=col.cl6, lw=3)

#numer of shoppers across time per store
plot(TIMEcl1$hour, TIMEcl1$count.p.store, type="l", col=col.cl1, xlim=range(min(DT$hour),max(DT$hour)), ylim=range(0,20000),
     main="Number of customers per hour", xlab="Hour", ylab="# of baskets")
points(TIMEcl1$hour, TIMEcl1$count.p.store, type="l", col=col.cl1, lw=3)
points(TIMEcl2$hour, TIMEcl2$count.p.store, type="l", col=col.cl2, lw=3)
points(TIMEcl3$hour, TIMEcl3$count.p.store, type="l", col=col.cl3, lw=3)
points(TIMEcl4$hour, TIMEcl4$count.p.store, type="l", col=col.cl4, lw=3)
points(TIMEcl5$hour, TIMEcl5$count.p.store, type="l", col=col.cl5, lw=3)
points(TIMEcl6$hour, TIMEcl6$count.p.store, type="l", col=col.cl6, lw=3)

#basket value across time per store
plot(TIMEcl1$hour, TIMEcl1$mean_value, type="l", col=col.cl1, xlim=range(min(DT$hour),max(DT$hour)), ylim=range(0,300),
     main="Mean basket value per hour", xlab="Hour", ylab="Mean basket value")
points(TIMEcl1$hour, TIMEcl1$mean_value, type="l", col=col.cl1, lw=3)
points(TIMEcl2$hour, TIMEcl2$mean_value, type="l", col=col.cl2, lw=3)
points(TIMEcl3$hour, TIMEcl3$mean_value, type="l", col=col.cl3, lw=3)
points(TIMEcl4$hour, TIMEcl4$mean_value, type="l", col=col.cl4, lw=3)
points(TIMEcl5$hour, TIMEcl5$mean_value, type="l", col=col.cl5, lw=3)
points(TIMEcl6$hour, TIMEcl6$mean_value, type="l", col=col.cl6, lw=3)

#basket size across time per store
plot(TIMEcl1$hour, TIMEcl1$mean_items, type="l", col=col.cl1, xlim=range(min(DT$hour),max(DT$hour)), ylim=range(0,6),
     main="Mean items in basket per hour", xlab="Hour", ylab="# of items in basket")
points(TIMEcl1$hour, TIMEcl1$mean_items, type="l", col=col.cl1, lw=3)
points(TIMEcl2$hour, TIMEcl2$mean_items, type="l", col=col.cl2, lw=3)
points(TIMEcl3$hour, TIMEcl3$mean_items, type="l", col=col.cl3, lw=3)
points(TIMEcl4$hour, TIMEcl4$mean_items, type="l", col=col.cl4, lw=3)
points(TIMEcl5$hour, TIMEcl5$mean_items, type="l", col=col.cl5, lw=3)
points(TIMEcl6$hour, TIMEcl6$mean_items, type="l", col=col.cl6, lw=3)

#return to SQM among clusters
#revenue per square meter
plot(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==1], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==1], 
     main="Monthly revenue per square meter", xlab="Store area [m2]", ylab="Monthly revenue [Kč/m2]", col=col.cl1, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==1]/300),
     xlim=range(min(DTt_segment_shop$SQM), max(DTt_segment_shop$SQM)), ylim=range(min(DTt_segment_shop$rev_month_meter), max(DTt_segment_shop$rev_month_meter)))
points(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==2], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==2], 
       col=col.cl2, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==2]/300))
points(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==3], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==3], 
       col=col.cl3, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==3]/300))
points(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==4], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==4], 
       col=col.cl4, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==4]/300))
points(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==5], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==5], 
       col=col.cl5, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==5]/300))
points(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==6], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==6], 
       col=col.cl6, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==6]/300))

#return to SQM based on the share of small basktes
#revenue per square meter
plot(DTt_segment_shop$count2_share[DTt_segment_shop$cluster6==1], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==1], 
     main="Monthly revenue per square meter", xlab="Share of baskets with 1 or 2 items", ylab="Monthly revenue [Kč/m2]", col=col.cl1, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==1]/300),
     xlim=range(min(DTt_segment_shop$count2_share), max(DTt_segment_shop$count2_share)), ylim=range(min(DTt_segment_shop$rev_month_meter), max(DTt_segment_shop$rev_month_meter)))
points(DTt_segment_shop$count2_share[DTt_segment_shop$cluster6==2], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==2], 
       col=col.cl2, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==2]/300))
points(DTt_segment_shop$count2_share[DTt_segment_shop$cluster6==3], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==3], 
       col=col.cl3, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==3]/300))
points(DTt_segment_shop$count2_share[DTt_segment_shop$cluster6==4], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==4], 
       col=col.cl4, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==4]/300))
points(DTt_segment_shop$count2_share[DTt_segment_shop$cluster6==5], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==5], 
       col=col.cl5, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==5]/300))
points(DTt_segment_shop$count2_share[DTt_segment_shop$cluster6==6], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==6], 
       col=col.cl6, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==6]/300))

#return to SQM based on the mean price of item
#revenue per square meter
plot(DTt_segment_shop$mean_item_price[DTt_segment_shop$cluster6==1], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==1], 
     main="Monthly revenue per square meter", xlab="Average price of good in basket", ylab="Monthly revenue [Kč/m2]", col=col.cl1, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==1]/300),
     xlim=range(min(DTt_segment_shop$mean_item_price), max(DTt_segment_shop$mean_item_price)), ylim=range(min(DTt_segment_shop$rev_month_meter), max(DTt_segment_shop$rev_month_meter)))
points(DTt_segment_shop$mean_item_price[DTt_segment_shop$cluster6==2], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==2], 
       col=col.cl2, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==2]/300))
points(DTt_segment_shop$mean_item_price[DTt_segment_shop$cluster6==3], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==3], 
       col=col.cl3, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==3]/300))
points(DTt_segment_shop$mean_item_price[DTt_segment_shop$cluster6==4], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==4], 
       col=col.cl4, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==4]/300))
points(DTt_segment_shop$mean_item_price[DTt_segment_shop$cluster6==5], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==5], 
       col=col.cl5, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==5]/300))
points(DTt_segment_shop$mean_item_price[DTt_segment_shop$cluster6==6], DTt_segment_shop$rev_month_meter[DTt_segment_shop$cluster6==6], 
       col=col.cl6, pch=16, cex=(DTt_segment_shop$SQM[DTt_segment_shop$cluster6==6]/300))

#special events analysis
events <- read.csv(file="DATA_CREATED/Events.csv", sep=",", dec=".", encoding="utf-8")

events$group.temp <- gsub("(",":", events$V1, fixed = TRUE)
events$fin <- gsub(".*:","",events$group.temp)
events$fin <- gsub(")",":", events$fin, fixed = TRUE)
events$fin <- gsub(":.*","",events$fin)

events$group.temp <- NULL
colnames(events)[colnames(events)=="fin"] <- "NAZ_OBEC"

EV <- merge(x=events, y=stores_clust, by="NAZ_OBEC")
EV$V2 <- as.character(EV$V2)
EV$datetype <- nchar(EV$V2, type = "chars", allowNA = FALSE, keepNA = NA)

for(i in 1:nrow(EV)){
  if(EV[i,"datetype"] >=12 & EV[i,"datetype"]<=14){
    EV[i,"date_start"] <- str_sub(EV[i,"V2"], 4, -2)
    EV[i,"date_start"] <- gsub(" .*","",EV[i,"date_start"])
    EV[i,"date_start"] <- gsub(".","/", EV[i,"date_start"], fixed = TRUE)
    EV[i,"date_end"] <- EV[i,"date_start"]
  }else if(EV[i,"datetype"] >=16 & EV[i,"datetype"]<=19){
    EV[i,"date_start"] <- str_sub(EV[i,"V2"], 4, -1)
    EV[i,"date_start"] <- gsub(" .*","",EV[i,"date_start"])
    EV[i,"date_start"] <- gsub(".","/", EV[i,"date_start"], fixed = TRUE)
    EV[i,"date_end"] <- EV[i,"date_start"]
  }else if(EV[i,"datetype"] >=24 & EV[i,"datetype"]<=31){
    EV[i,"date_start"] <- str_sub(EV[i,"V2"], 4, -1)
    EV[i,"date_start"] <- gsub(" - .*","",EV[i,"date_start"])
    EV[i,"date_start"] <- gsub(".","/", EV[i,"date_start"], fixed = TRUE)
    EV[i,"date_end"] <- gsub(".* - ","",EV[i,"V2"])
    EV[i,"date_end"] <- str_sub(EV[i,"date_end"], 4, -1)
    EV[i,"date_end"] <- gsub(".","/", EV[i,"date_end"], fixed = TRUE)
  }else if(EV[i,"datetype"] >=34 & EV[i,"datetype"]<=42){
    EV[i,"date_start"] <- str_sub(EV[i,"V2"], 4, -1)
    EV[i,"date_start"] <- gsub(" - .*","",EV[i,"date_start"])
    EV[i,"date_start"] <- gsub(" .*","",EV[i,"date_start"])  
    EV[i,"date_start"] <- gsub(".","/", EV[i,"date_start"], fixed = TRUE)
    EV[i,"date_end"] <- gsub(".* - ","",EV[i,"V2"])
    EV[i,"date_end"] <- str_sub(EV[i,"date_end"], 4, -1)
    EV[i,"date_end"] <- gsub(" .*","",EV[i,"date_end"])     
    EV[i,"date_end"] <- gsub(".","/", EV[i,"date_end"], fixed = TRUE)      
  }else{
    EV[i,"date_start"] <- 0
    EV[i,"date_end"] <- 0  
  } 
}

EV$date_start <- as.Date(EV$date_start, format='%d/%m/%Y')
EV$date_end <- as.Date(EV$date_end, format='%d/%m/%Y')

EV$time_duration <- EV$date_end - EV$date_start

EV <- subset(EV, time_duration >= 0 & time_duration < 7)

EV$space_time <- paste(EV$store_id, EV$date_start, sep = "-")

DT$date <- substr(DT$Transaction_Date_Time, 1, 8)
DT$date <- as.Date(DT$date, format='%Y%m%d')

DTdate <- DT %>% group_by(Retailer_Store_Number, date) %>% 
  summarise(return = sum(Total_Basket_Value))

DTdate$space_time <- paste(DTdate$Retailer_Store_Number, DTdate$date, sep = "-")

DTdatelast <- DTdate

DTdatelast$date <- DTdatelast$date + 7
DTdatelast$space_time <- paste(DTdatelast$Retailer_Store_Number, DTdatelast$date, sep = "-")
DTdatelast$Retailer_Store_Number <- NULL
DTdatelast$date <- NULL
colnames(DTdatelast)[colnames(DTdatelast)=="return"] <- "return_last"

DTdate <- merge(x=DTdate, y=DTdatelast, by="space_time")

DTdatenext <- DTdate

DTdatenext$date <- DTdatenext$date - 7
DTdatenext$space_time <- paste(DTdatenext$Retailer_Store_Number, DTdatenext$date, sep = "-")
DTdatenext$Retailer_Store_Number <- NULL
DTdatenext$date <- NULL
DTdatenext$return_last <- NULL
colnames(DTdatenext)[colnames(DTdatenext)=="return"] <- "return_next"

DTdate <- merge(x=DTdate, y=DTdatenext, by="space_time")

DTdate <- subset(DTdate, return>0 & return_last>0 & return_next>0)

EVeval <- merge(EV, DTdate, by="space_time")





### desperate parts of some codes
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

#creating data sample
set.seed(123)
nrow(ITM) %>% {sample(.,. * 0.01)} -> index
ITMt <- ITM[index,]
ITMt <- subset(ITMt, Sold_Amount > 0)

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

###html srape
events <- matrix(0, ncol = 2, nrow = 512)
events <- as.data.frame(events)

for(i in 1:512){
  temp_1 <- "získání lokace"
  events[i,1] <- temp_1
  temp_2 <- "získání času"
  events[i,2] <- temp_2
}





