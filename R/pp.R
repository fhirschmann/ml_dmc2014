# Data Preprocessing goes here

pp <- function(dt) {
    # Preprocesses DMC2014 data, i.e. cleaning, creating features, etc...
    #
    # Args:
    #   dt: A data frame
    #
    # Returns:
    #   A preprocessed data frame
    require(data.table)
    require(lubridate)
    require(plyr)
    
    dt2 <- dt
    dt2.tbl <- data.table(dt)
    message("Preprocessing Data Frame")

    # Nominal Predictors    
    dt2_factors <- list(
        "customerID"=NULL,
        "manufacturerID"=NULL,
        "itemID"=NULL,
        "returnShipment"=c("0"="no", "1"="yes")
    )

    for (name in names(dt2_factors)) {
        if (name %in% colnames(dt2)) {
            dt2[[name]] <- as.factor(dt2[[name]])
            if (!is.null(dt2_factors[[name]]))
                dt2[[name]] <- revalue(dt2[[name]], dt2_factors[[name]])
        }
    }
    
    dt2$size <- as.factor(toupper(dt2$size))
        
    # Date Predictors
    dt2_dates <- c("dateOfBirth", "creationDate", "orderDate", "deliveryDate")
    dt2[dt2_dates] <- lapply(dt2[dt2_dates], as.Date)
    
    dt2$deliveryTime <- as.integer(dt2$deliveryDate - dt2$orderDate)

    dt2$instantorder <- as.factor(ifelse(dt2$orderDate == dt2$creationDate, "yes", "no"))
    
    # Add some features
    dt2$orderWeekday <- as.ordered(as.factor(lubridate::wday(dt2$orderDate, label=T, abbr=F)))

    dt2$customerAge <- as.integer(2013 - year(dt2$dateOfBirth))
    
    dt2$sameItemsOrdered <- dt2.tbl[,x := .N, by=c("itemID", "customerID", "orderDate")]$x
    
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
    
    
    #dt2$w0all <- Reduce("+", lapply(0:10, function(x) dt2[[paste("w", x, sep="")]]))
    
    dt2
}

cl <- function(dt) {
    # Cleans DMC2010 data, i.e. outlier removal
    #
    # Args:
    #   dt: A data frame
    #
    # Returns:
    #   A preprocessed data frame
    require(plyr)
    
    dt2 <- dt

    # Outlier Removal
    
    ## deliveryDate/Time
    outliers <- !is.na(dt2$deliveryTime) & dt2$deliveryTime < 0
    dt2[outliers, ]$deliveryTime <- NA
    dt2[outliers, ]$deliveryDate <- NA
    
    ## dateOfBirth/Age
    outliers <- with(dt2,
                     !is.na(dateOfBirth) 
                     & (dateOfBirth == as.Date("1949-11-19")
                        | customerAge > 85
                        | customerAge < 19))

    dt2[outliers, ]$dateOfBirth <- NA
    dt2[outliers, ]$customerAge <- NA

    ## creationDate
    outliers <- with(dt2, creationDate == as.Date("2011-02-16"))
    dt2[outliers, ]$creationDate <- NA
    
    # Discretization
    
    ## TODO
    
    dt2
}

im <- function(dt) {
    # Imputes missing values.
    #
    # Args:
    #   dt: A data frame
    
    dt2 <- dt

    # Color: set to "other"
    levels(dt2$color) <- c(levels(dt2$color), "other")
    if (any(is.na(dt2$color)))  # test set doesn't have NAs
        dt2[is.na(dt2$color), ]$color <- "other"
    
    # Deliverytime: mean
    dt2[is.na(dt2$deliveryTime), ]$deliveryTime <- as.integer(mean(dt2$deliveryTime, na.rm=T))

    # customerAge: mean
    dt2[is.na(dt2$customerAge), ]$customerAge <- as.integer(mean(dt2$customerAge, na.rm=T))
    
    dt2
}