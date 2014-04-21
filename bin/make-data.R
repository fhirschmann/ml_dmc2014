#!/usr/bin/env Rscript
library(data.table)
library(plyr)
library(lubridate)
library(vcd)
library(zoo)

source("R/pp.R")


na.strings <- c("NA", "", "??", "?")

dt.train <- pp(read.csv("task/orders_train.txt", sep=";", na.strings=na.strings))
dt.test <- pp(read.csv("task/orders_class.txt", sep=";", na.strings=na.strings))

# customer blacklist
x <- as.data.frame.matrix(structable(returnShipment ~ customerID, data = dt.train))
z <- dt.train[dt.train$customerID %in% rownames(x[x[["0"]] == 0, ]), c("customerID", 
                                                                       "orderDate")]
z <- z[!duplicated(z), ]
zz <- as.data.frame(table(z$customerID))

#ideen für features:
#größe des customers aus bestellen items ermitteln
#preisfeature: abweichung von preis (höher, niedriger, gleich)
#anrede: runterbrechen auf "männlich" / "weiblich" (falls kein großer unterschied bei returnshipments zwischen anreden)



feat.simple <- function(dt) {
    dt2 <- dt
    dt2.tbl <- data.table(dt)
    
    dt2$creationDateMissing <- as.factor(ifelse(is.na(dt2$creationDate), "yes", "no"))
    
    dt2$deliveryTime <- as.integer(dt2$deliveryDate - dt2$orderDate)
    dt2$deliveryDateMissing <- as.factor(ifelse(is.na(dt2$deliveryDate), "yes", "no"))
    
    dt2$orderWeekday <- as.ordered(as.factor(lubridate::wday(dt2$orderDate, label=T, abbr=F)))
    
    # Customer Age in Years
    dt2$customerAge <- as.integer(year(dt2$orderDate) - year(dt2$dateOfBirth))
    dt2$dateOfBirthMissing <- as.factor(ifelse(is.na(dt2$dateOfBirth), "yes", "no"))
    
    # Account Age in Days
    dt2$accountAge <- as.numeric(dt2$orderDate - dt2$creationDate)
    
    dt2$sameItemsOrdered <- dt2.tbl[,x := .N, by=c("itemID", "customerID", "orderDate")]$x
    
    dt2$firstOrderDate <- as.Date(dt2.tbl[, x := as.integer(min(orderDate)), by=c("customerID")]$x)
    dt2$firstOrderDate <- as.Date(dt2$firstOrderDate)
    
    dt2$orderVolume <- dt.tbl[, x := as.integer(sum(price)), by=c("customerID", "orderDate")]$x
    
    dt2$totalOrderVolume <- dt.tbl[, x := as.integer(sum(price)), by=c("customerID")]$x
    
    #summarize colors:
    colors <- dt2$color
    colors <- revalue(colors, c("dark denim"="black", #blue?
                                "dark navy"="blue",
                                "ash"="grey",
                                "bordeaux"="red",
                                "mahagoni"="red", #brown?
                                "gold"="yellow",
                                "dark oliv"="green",
                                "striped"="other",
                                "anthracite"="black", #grey?
                                "antique pink"="red",
                                "floral"="other", #?
                                "baltic blue"="blue",
                                "nature"="other", #?
                                "ancient"="other", #?
                                "curry"="yellow",
                                "turquoise"="blue",
                                "navy"="blue",
                                "brown"="brown",
                                "aubergine"="red", #brown?
                                "mocca"="brown", #grey?
                                "blau"="blue",
                                "basalt"="grey",
                                "azure"="blue",
                                "coral"="red", #yellow?
                                "pallid"="grey", #?
                                "petrol"="blue",
                                "silver"="grey",
                                "habana"="red", #?
                                "darkblue"="blue",
                                "beige"="yellow", #brown/grey?
                                "mint"="green",
                                "khaki"="brown", #yellow?
                                "hibiscus"="red",
                                "orange"="red", #yellow?
                                "yellow"="yellow",
                                "black"="black",
                                "blue"="blue",
                                "purple"="red",
                                "almond"="brown", #yellow?
                                "red"="red",
                                "berry"="red",
                                "grey"="grey",
                                "ocher"="brown",
                                "avocado"="green",
                                "magenta"="red",
                                "olive"="green",
                                "white"="white",
                                "denim"="other", #?
                                "pink"="red",
                                "stained"="brown",
                                "kanel"="brown", #red?
                                "green"="green",
                                "jade"="green",
                                "aquamarine"="blue",
                                "aqua"="blue",
                                "ecru"="grey", #brown/yellow?
                                "iron"="grey",
                                "fuchsia"="red",
                                "ingwer"="red", #brown?
                                "cognac"="brown",
                                "terracotta"="brown", #red?
                                "apricot"="yellow",
                                "graphite"="grey",
                                "crimson"="red",
                                "lemon"="yellow",
                                "oliv"="green",
                                "leopard"="yellow", #brown/other?
                                "amethyst"="red",
                                "aviator"="other", #?
                                "bronze"="brown",
                                "brwon"="brown",
                                "caramel"="yellow",
                                "champagner"="yellow", #grey?
                                "cobalt blue"="blue",
                                "copper coin"="brown",
                                "cortina mocca"="brown",
                                "creme"="yellow",
                                "curled"="other", #?
                                "currant purple"="red",
                                "dark garnet"="black", #red?
                                "dark grey"="grey",
                                "ebony"="black",
                                "ivory"="white",
                                "mango"="yellow",
                                "opal"="other", #kann alles sein
                                "perlmutt"="other", #?
                                "vanille"="white"))
    dt2$fewcolors <- colors
    dt2
}

dt.train <- feat.simple(dt.train)
dt.test <- feat.simple(dt.test)

write.table(dt.train, file = "task/data_train.txt", sep = ";", na = "NA", 
            quote = F, row.names = F)
write.table(dt.test, file = "task/data_test.txt", sep = ";", na = "NA", 
            quote = F, row.names = F)
