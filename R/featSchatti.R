# Anleitung:
#
# In ./bin/make-data.R, Zeile 9, "schatti" auf T setzen. Dann hier Änderungen
# einbauen und (in Windows) Rscript bin\make-data.R aufrufen.
#
# Danach kann die Güte eines einzelnen Features folgendermaßen beurteilt werden:
#
# Unter Windows (zuerst install.packages("C50") in R ausführen):
#
# Rscript bin\fe.R c50 M30 nameDesFeatures
#
# und
#
# Rscript bin\fe.R c50 M31 nameDesFeatures
#
# Dann wird zuerst ein Modell mit allen Features gelernt (die erste Score), danach
# wird ein Modell gelernt bei dem `nameDesFeatures` weggelassen wird. Ist die Score
# geringer (!) ist das gut, andernfalls schlecht. Ein negativer Change ist also gut
# und in diesem Fall würde es "Feature Kept" heißen. Eine sehr gute Änderung
# wäre z.B. -50.

add.features.schatti <- function(dt) {
    # Hier können Features hinzugefügt werden, die einfach nur an das vorgegebene
    # Train und Test Set gejoined werden oder auf einem Set berechnet werden
    # können ohne Einflussnahme des jeweils anderen
    
    require(plyr)
    
    dt2 <- dt
    nrow.before <- nrow(dt2)
    
    category <- read.table("task/itemManuCategory.txt", sep=";")
    categorySize <- read.table("task/itemManuCategoryPlusSize.txt", sep=";")
    
    
    dt2 <- join(dt2, category, by=c("manufacturerID", "itemID"))
    dt2 <- join(dt2, categorySize, by=c("manufacturerID", "itemID", "itemSize"))
    
    nrow.after <- nrow(dt2)
    
    if (nrow.before != nrow.after)
        stop("Something went wrong with the join. :(")
    
    dt2
}

add.features.schatti.all <- function(dt, alldata) {
    # Hier können Features hinzugefügt werden, die auf der Gesamtmenge aller verfügbaren
    # Daten berechnet werden sollen (alldata) und dem aktuellen Datenset (dt) hinzugefügt
    # werden sollen.
    dt2 <- dt
    nrow.before <- nrow(dt2)
    
    
    
    
    if (nrow.before != nrow.after)
        stop("Something went wrong with the join. :(")
    dt
}