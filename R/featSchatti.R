# Anleitung:
#
# In ./bin/make-data.R, Zeile 9, "schatti" auf T setzen. Dann hier Änderungen
# einbauen und (in Windows) Rscript bin\make-data.R aufrufen.
#
# Danach kann die Güte eines einzelnen Features folgendermaßen beurteilt werden:
#
# Unter Windows (zuerst install.packages("C50") in R ausführen):
#
#   Rscript bin\fe.R c50 M30 nameDesFeatures
#
# und
#
#   Rscript bin\fe.R c50 M31 nameDesFeatures
#
# Dann wird zuerst ein Modell mit allen Features gelernt (die erste Score), danach
# wird ein Modell gelernt bei dem `nameDesFeatures` weggelassen wird. Ist die Score
# geringer (!) ist das gut, andernfalls schlecht. Ein negativer Change ist also gut
# und in diesem Fall würde es "Feature Kept" heißen. Eine sehr gute Änderung
# wäre z.B. -50.

# Um zu schauen ob die Features einigermaßen passen, kann man z.B. folgendes machen:
#
# Hier auf `Source` klicken, dann
#
#   source("R/data.R")
#   x <- add.features.schatti(dt.train)
#   str(x)


add.features.schatti <- function(dt) {
    # Hier können Features hinzugefügt werden, die einfach nur an das vorgegebene
    # Train und Test Set gejoined werden oder auf einem Set berechnet werden
    # können ohne Einflussnahme des jeweils anderen
    require(plyr)
    
    dt2 <- dt
    nrow.before <- nrow(dt2)
    
    category <- data.table(read.table("task/itemManuCategory.txt", sep=";"))
    categorySize <- data.table(read.table("task/itemManuCategoryPlusSize.txt", sep=";"))
    
    dt2 <- join(dt2, category, by=c("manufacturerID", "itemID"))
    dt2 <- join(dt2, categorySize, by=c("manufacturerID", "itemID", "itemSize"))
    setnames(dt2,(ncol(dt2)-2),"spalte1")
    setnames(dt2,(ncol(dt2)-1),"spalte2")
    setnames(dt2,(ncol(dt2)),"spalte3")
    dt2<-data.table(dt2)
    dt2[is.na(itemCategory), itemCategory:=spalte1]
    dt2[is.na(itemSpecificCategory), itemSpecificCategory:=spalte2]
    dt2[is.na(itemSizeGroup), itemSizeGroup:=spalte3]
    dt2[,spalte1:=NULL]
    dt2[,spalte2:=NULL]
    dt2[,spalte3:=NULL]
    
    
    # Don't remove the next lines
    nrow.after <- nrow(dt2)
    if (nrow.before != nrow.after)
        stop("Something went wrong with the join. :(")
    dt2
}

add.features.schatti.all <- function(dt, alldata) {
    # Hier können Features hinzugefügt werden, die auf der Gesamtmenge aller verfügbaren
    # Daten berechnet werden sollen (alldata) und dem aktuellen Datenset (dt) hinzugefügt
    # werden sollen.
    d2end <-data.table(dt)
    nrow.before <- nrow(d2end)
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MEIN ZEUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    d2end <- dt
    #d2end <- dt.train #####
    #d2end <- dt.test #####
    #laden der beiden size transform tabellen
    transformShoes <- data.table(read.table("task/size_transforming_Shoes_FR_2_Shoes_UK.txt", sep=";"))
    transformClothes2Tops <- data.table(read.table("task/size_transforming_TopsShoes2Clothes.txt", sep=";"))
    transformClothes1 <- data.table(read.table("task/size_transforming_2Clothes1.txt", sep=";"))
    transformClothes2 <- data.table(read.table("task/size_transforming_2Clothes2.txt", sep=";"))
    
    #hier wird erstmal der Output ignoriert
    #alldata <- rbind(data.frame(dt.train), data.frame(dt.test, returnShipment=NA)) ####
    dt2 <- data.table(alldata)
    rm(alldata)
    #ACHTUNG: habe itemReturnRate neu gemacht
    
    dt2hold<-dt2[returnShipment=="no" & deliveryDateMissing=="no"]
    dt2return<-dt2[returnShipment=="yes" & deliveryDateMissing=="no"]
    
    
    # customer favorite Category buy
    dt2[, customerFavCategoryBuy := names(which.max(table(itemCategory))), by=c('customerID')]
    customerFavCategoryBuy <- unique(dt2[, c("customerID","customerFavCategoryBuy"), with=F])
    d2end <- join(d2end, customerFavCategoryBuy, by=c("customerID"))
    rm(customerFavCategoryBuy)
    
    # customer favorite specific Category all
    dt2[, customerFavSpeciCategoryBuy := names(which.max(table(itemSpecificCategory))), by=c('customerID')]
    customerFavSpeciCategoryBuy <- unique(dt2[, c("customerID","customerFavSpeciCategoryBuy"), with=F])
    d2end <- join(d2end, customerFavSpeciCategoryBuy, by=c("customerID"))
    rm(customerFavSpeciCategoryBuy)
    
    # customer favorite Category buy & hold
    dt2hold[, customerFavCategoryHold := names(which.max(table(itemCategory))), by=c('customerID')]
    customerFavCategoryHold <- unique(dt2hold[, c("customerID","customerFavCategoryHold"), with=F])
    d2end <- join(d2end, customerFavCategoryHold, by=c("customerID"))
    rm(customerFavCategoryHold)
    
    # customer favorite specific Category buy & hold
    dt2hold[, customerFavSpeciCategoryHold := names(which.max(table(itemSpecificCategory))), by=c('customerID')]
    customerFavSpeciCategoryHold <- unique(dt2hold[, c("customerID","customerFavSpeciCategoryHold"), with=F])
    d2end <- join(d2end, customerFavSpeciCategoryHold, by=c("customerID"))
    rm(customerFavSpeciCategoryHold)
    
    # customer favorite Category buy & return
    dt2return[, customerFavCategoryReturn := names(which.max(table(itemCategory))), by=c('customerID')]
    customerFavCategoryReturn <- unique(dt2return[, c("customerID","customerFavCategoryReturn"), with=F])
    d2end <- join(d2end, customerFavCategoryReturn, by=c("customerID"))
    rm(customerFavCategoryReturn)
    
    # customer favorite specific Category buy & return
    dt2return[, customerFavSpeciCategoryReturn := names(which.max(table(itemSpecificCategory))), by=c('customerID')]
    customerFavSpeciCategoryReturn <- unique(dt2return[, c("customerID","customerFavSpeciCategoryReturn"), with=F])
    d2end <- join(d2end, customerFavSpeciCategoryReturn, by=c("customerID"))
    rm(customerFavSpeciCategoryReturn)
    
    # customer favorite sizes buy & hold; by itemCategory, itemSpecificCategory and itemSizeGroup
    dt2hold[, customerSizeHold := names(which.max(table(itemSize))), by=c('customerID','itemCategory','itemSpecificCategory','itemSizeGroup')]
    customerSizeHold <- unique(dt2hold[, c("customerID","itemCategory","itemSpecificCategory","itemSizeGroup","customerSizeHold"), with=F])
    d2end <- join(d2end, customerSizeHold, c("customerID","itemCategory","itemSpecificCategory","itemSizeGroup"))
    rm(customerSizeHold)
    
    # customer shoe size FR
    temp<-dt2hold[itemSizeGroup=="FR"]
    temp[, customerShoeSizeFR := names(which.max(table(itemSize))), by=c('customerID')]
    customerShoeSizeFR <- unique(temp[, c("customerID","customerShoeSizeFR"), with=F])
    d2end <- join(d2end, customerShoeSizeFR, by=c("customerID"))
    rm(customerShoeSizeFR)
    
    # customer shoe size UK
    temp<-dt2hold[itemSizeGroup=="UK"]
    temp[, customerShoeSizeUK := names(which.max(table(itemSize))), by=c('customerID')]
    customerShoeSizeUK <- unique(temp[, c("customerID","customerShoeSizeUK"), with=F])
    d2end <- join(d2end, customerShoeSizeUK, by=c("customerID"))
    rm(customerShoeSizeUK)
    
    # customer Top size
    temp<-dt2hold[itemSizeGroup=="International"]
    temp[, customerTopSize := names(which.max(table(itemSize))), by=c('customerID')]
    customerTopSize <- unique(temp[, c("customerID","customerTopSize"), with=F])
    d2end <- join(d2end, customerTopSize, by=c("customerID"))
    rm(customerTopSize)
    
    # customer clothes size
    temp<-dt2hold[itemSpecificCategory=="womensClothes"]
    temp[, customerClothesSize1 := names(which.max(table(itemSize))), by=c('customerID')]
    customerClothesSize1 <- unique(temp[, c("customerID","customerClothesSize1"), with=F])
    d2end <- join(d2end, customerClothesSize1, by=c("customerID"))
    d2end[!is.na(customerClothesSize1),customerClothesSize2:=customerClothesSize1]
    d2end[!is.na(customerClothesSize1),customerClothesSizeDistinct:="yes"]
    rm(customerClothesSize1)
    
    #auffüllen der sizes über Referenztabellen ^^
    #shoes
    #UK to FR
    temp<-d2end[is.na(customerShoeSizeFR),list(customerID,customerShoeSizeFR, customerShoeSizeUK)]
    temp<-join(temp,transformShoes, by=c("customerShoeSizeUK"))
    setnames(temp,(ncol(temp)-1),"quelleSpalte")
    customerShoeSizeFR <- unique(temp[, c("customerID","quelleSpalte"), with=F])
    d2end <- join(d2end, customerShoeSizeFR, by=c("customerID"))
    d2end[!is.na(quelleSpalte),customerShoeSizeFR:=quelleSpalte]
    d2end[,quelleSpalte:=NULL]
    rm(customerShoeSizeFR)
    #FR to UK
    temp<-d2end[is.na(customerShoeSizeUK),list(customerID,customerShoeSizeFR, customerShoeSizeUK)]
    temp<-join(temp,transformShoes, by=c("customerShoeSizeFR"))
    setnames(temp,(ncol(temp)-1),"quelleSpalte")
    customerShoeSizeUK <- unique(temp[, c("customerID","quelleSpalte"), with=F])
    d2end <- join(d2end, customerShoeSizeUK, by=c("customerID"))
    d2end[!is.na(quelleSpalte),customerShoeSizeUK:=quelleSpalte]
    d2end[,quelleSpalte:=NULL]
    rm(customerShoeSizeUK)
    #clothes2tops
    temp<-d2end[is.na(customerTopSize),list(customerID,customerClothesSize1,customerTopSize)]
    temp<-join(temp,transformClothes2Tops, by=c("customerClothesSize1"))
    setnames(temp,(ncol(temp)),"quelleSpalte")
    customerTopSize <- unique(temp[, c("customerID","quelleSpalte"), with=F])
    d2end <- join(d2end, customerTopSize, by=c("customerID"))
    d2end[!is.na(quelleSpalte),customerTopSize:=quelleSpalte]
    d2end[,quelleSpalte:=NULL]
    rm(customerTopSize)
    #das ist schon jetzt bissel spekulativer
    #tops2clothes
    temp<-d2end[is.na(customerClothesSize1) & !is.na(customerShoeSizeUK) & !is.na(customerTopSize), list(customerID,customerClothesSize1,customerClothesSize1,customerTopSize,customerShoeSizeUK)]
    temp<-join(temp,transformShoes, by=c("customerShoeSizeUK"))
    temp<-join(temp,transformClothes1, by=c("shoeIndiz","customerTopSize"))
    temp<-join(temp,transformClothes2, by=c("shoeIndiz","customerTopSize"))
    customerClothesSize <- unique(temp[, c("customerID","clothesSize1","clothesSize2"), with=F])
    customerClothesSize[,customerClothesDistinct:="no"]
    rm(temp)
    d2end <- join(d2end, customerClothesSize, by=c("customerID"))
    setnames(d2end,(ncol(d2end)),"quelleSpalte")
    d2end[!is.na(clothesSize1),customerClothesSize1:=as.factor(clothesSize1)]
    d2end[!is.na(clothesSize2),customerClothesSize2:=as.factor(clothesSize2)]
    d2end[,clothesSize1:=NULL]
    d2end[,clothesSize2:=NULL]
    d2end[,quelleSpalte:=NULL]
    rm(customerClothesSize)
    
#     dt.tr<-d2end
#     dt.te<-d2end
#     dt.train<-dt.tr
#     dt.test<-dt.te
#     rm(d2end)
    #wie oft der customer insgesamt die size bestellt hat
    # FEATURE: orderItemNumSameItemsOrdered
    
    
    #TODO Feature: customerAccessoriesReturnRate
    
    
    
    
    #von den favorite sizes werden die  auch die anderen sizes für die anderen Customer füllen
    #write.table(dt2hold, "D:/dt2hold.txt", sep=";") 
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
    
    #MUSTER zum Hinzufügen der berechneten Sachen
    #temp<-temp[, favCustSpecCatHold := names(which.max(table(size))), by=c('customerID','itemCategory','specificItemCategory','sizeGroup')]
    #favCustSpecCatHold <- unique(temp[, c("customerID","itemCategory","specificItemCategory","sizeGroup","favCustSpecCatHold"), with=F])
    #temp <- join(temp, favCustSpecCatHold, by=c("customerID","itemCategory","specificItemCategory","sizeGroup"))
    
    
    
    
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEMP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ##erzeugt die daten zur Untersuchung
    #temp <- rbind(data.frame(dt.train, group="train"),
    #              data.frame(dt.test, group="test", returnShipment=NA))
    #
    #     only on data temp<-dt.train[returnShipment=="yes"]
    #     temp<-temp[, customerFavSpeciCategoryReturn := names(which.max(table(specificItemCategory))), by=c('customerID')]
    #     
    #     temp<-dt.train[returnShipment=="no",list(customerID,manufacturerID,itemID,itemCategory,specificItemCategory,sizeGroup,size,returnShipment,orderDate)]
    #     temp<-temp[order(customerID,orderDate,decreasing=FALSE)]
    #     temp<-temp[, customerFavSpeciCategoryHold := names(which.max(table(specificItemCategory))), by=c('customerID')]
    #     
    #     
    #     temp<-dt.train[returnShipment=="no",list(customerID,itemCategory,specificItemCategory,sizeGroup,size)]
    #     temp<-temp[order(customerID,decreasing=FALSE)]
    #     
    #     temp<-temp[, favCustSpecCatHold := names(which.max(table(size))), by=c('customerID','itemCategory','specificItemCategory','sizeGroup')]
    #     favCustSpecCatHold <- unique(temp[, c("customerID","itemCategory","specificItemCategory","sizeGroup","favCustSpecCatHold"), with=F])
    #     temp <- join(temp, favCustSpecCatHold, by=c("customerID","itemCategory","specificItemCategory","sizeGroup"))
    #     
    #     
    #     nrow(temp[size==favCustSpecCatHold & returnShipment=="yes"])
    #     nrow(temp[is.na(favCustSpecCatHold)])
    #     
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
    
    
    
    #%%%%%Analyse Zeugs%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
#     #ist immer salutation und item distinct?
#     # ja: dann bleibt alles wie es ist
#     # nein: alle Kategorien und Sizes müssen getrennt werden
#     temp<-dt.train[,.N,by=list(customerID, salutation)]
#     temp<-temp[,.N,by=customerID]
#     nrow(temp[N==1]) #59754
#     nrow(temp[N>1])  #0
#     #=>Eindeutig
    
#     #Attribut: ist Size für FavHold Artikel eindeutig?
#     temp<-dt.train[,list(orderItemID,customerID,itemSize,itemSpecificCategory,
#                          customerFavSpeciCategoryHold,
#                          customerFavSpeciCategoryReturn,
#                          customerSizeHold)]    
#  
 
#     temp<-dt.test[order(customerID, orderDate, decreasing=FALSE)]
#     
#     
#     rm(temp)
#     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    # Don't remove the next lines
    nrow.after <- nrow(d2end)
    if (nrow.before != nrow.after)
        stop("Something went wrong with the join. :(")
    d2end
}