

## https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2

## test code
##load libraries
install.packages('R.utils')
library(R.utils) # load bz2 file
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

## download data
if (!file.exists("stormdata.csv.bz2")) {
        fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(fileURL, "stormdata.csv.bz2")
        bunzip2("stormdata.csv.bz2", "stormdata.csv", remove=FALSE)
}

storm <- data.table::fread("stormdata.csv", fill=TRUE, header=TRUE)
head(storm)

## check names and subset data to the required varibales for analysis
names(storm)
stormdata <- storm %>% select(c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP"))
str(stormdata)

## process health data
totalInjuries <- aggregate(INJURIES ~ EVTYPE, data = stormdata, FUN = sum)
topInjuries <- totalInjuries[order(-totalInjuries$INJURIES), ] 
totalFatalities <- aggregate(FATALITIES ~ EVTYPE, data = stormdata, FUN = sum)
topFatalities <- totalFatalities[order(-totalFatalities$FATALITIES), ] 
topHealth <- merge(topInjuries, topFatalities, all.x = TRUE)
topHealth[is.na(topHealth)] <- 0
topHealth2 <- topHealth[order(-topHealth$INJURIES),][1:10,]
topHealth2 <- gather(topHealth2,key=type, value=value, INJURIES,FATALITIES)

topHealth3 <- stormdata %>% select(EVTYPE, INJURIES, FATALITIES) %>%
        group_by(EVTYPE) %>%
        summarise(INJURIES = sum(INJURIES), FATALITIES = sum(FATALITIES), .groups='drop') %>%
        arrange(desc(INJURIES), desc(FATALITIES)) %>%
        slice(1:10) %>%
        gather(key = type, value = value, INJURIES, FATALITIES)

ggplot(data=topHealth2, aes(reorder(EVTYPE, -value), value, fill=type)) +
        geom_bar(position = "dodge", stat="identity") + 
        labs(x="Storm Type", y="Count") +
        theme(axis.text.x = element_text(angle = 45, vjust=0.5)) +
        ggtitle("Total Number of Injuries and Fatalities of the top 10 storm types") 

# cost comparison of storms
cost <- function(x) {
        if (x == "H")
                100
        else if (x == "K")
                1000
        else if (x == "M")
                1e+06
        else if (x == "B")
                1e+09
        else if (x == "")
                1
        else if (x == "m")
                1e+06
        else if (x == "0")
                1
        else if (x == "5")
                1e+05
        else if (x == "6")
                1e+06
        else if (x == "4")
                10000
        else if (x == "2")
                100
        else if (x == "3")
                1000
        else if (x == "h")
                100
        else if (x == "7")
                1e+07
        else if (x == "1")
                10
        else if (x == "8")
                1e+08
        else
                0
}

econdata <- storm %>% select(c("EVTYPE","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP"))
econdata$Property <-  econdata$PROPDMG*sapply(econdata$PROPDMGEXP, FUN = cost)
econdata$Crops <- econdata$CROPDMG*sapply(econdata$CROPDMGEXP, FUN = cost)
dmgValue <- aggregate(Property~EVTYPE,data = econdata, FUN = sum, na.rm = TRUE)
dmgValue <- dmgValue[with(dmgValue,order(-Property)),]
cropValue <- aggregate(Crops~EVTYPE,data = econdata, FUN = sum, na.rm = TRUE)
cropValue <- cropValue[with(cropValue,order(-Crops)),]

topDmg <- merge(dmgValue, cropValue, all.x = TRUE)
topDmg[is.na(topDmg)] <- 0
topDmg2 <- topDmg[order(-topDmg$Property),][1:10,]
topDmg2 <- gather(topDmg2,key=type, value=value, Property,Crops)

ggplot(data=topDmg2, aes(reorder(EVTYPE, -value), value, fill=type)) +
        geom_bar(position = "dodge", stat="identity") + 
        labs(x="Storm Type", y="Cost") +
        theme(axis.text.x = element_text(angle = 45, vjust=0.5)) +
        ggtitle("Total Property and Crop Damage Cost of the top 10 storm types") 