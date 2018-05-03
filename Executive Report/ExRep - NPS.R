library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(RcppRoll)
library(plotly)

#IMPORT
directory <- "Insert dir here"
NPS2018 <- read_excel(directory, sheet=2)
NPS2017 <- read_excel(directory, sheet=10)
NPS2016 <- read_excel(directory, sheet=11)

NPS2016 <- NPS2016[-1,]
colnames(NPS2016) <- NPS2016[1,]
colnames(NPS2016)[1] <- "month"
NPS2016 <- NPS2016[-c(1, 2),]
NPS2016$month <- na.locf(NPS2016$month)
NPS2016 <- NPS2016[!is.na(NPS2016$Grupp),]
NPS2016 <- NPS2016[!is.na(NPS2016$ML),]
NPS2016$Year <- 2016

NPS2017 <- NPS2017[-1,]
colnames(NPS2017) <- NPS2017[1,]
colnames(NPS2017)[1] <- "month"
NPS2017 <- NPS2017[-c(1,2),]
NPS2017$month <- na.locf(NPS2017$month)
NPS2017 <- NPS2017[!is.na(NPS2017$Grupp),]
NPS2017 <- NPS2017[!is.na(NPS2017$ML),]
NPS2017$Year <- 2017

NPS2018 <- NPS2018[-1,]
colnames(NPS2018) <- NPS2018[1,]
colnames(NPS2018)[1] <- "month"
NPS2018 <- NPS2018[-c(1, 2),]
NPS2018$month <- na.locf(NPS2018$month)
NPS2018 <- NPS2018[!is.na(NPS2018$Grupp),]
NPS2018 <- NPS2018[!is.na(NPS2018$ML),]
NPS2018$Year <- 2018


NPSraw <- bind_rows(NPS2016, NPS2017, NPS2018)
NPS <- NPSraw[c(21, 1:10)]

NPS$month <- ifelse(NPS$month=="Jan", "01", 
                    ifelse(NPS$month=="Febr", "02", 
                           ifelse(NPS$month=="Mars", "03", 
                                  ifelse(NPS$month=="April", "04",
                                         ifelse(NPS$month=="Maj", "05", 
                                                ifelse(NPS$month=="Juni", "06", 
                                                       ifelse(NPS$month=="Juli", "07", 
                                                              ifelse(NPS$month=="Aug", "08", 
                                                                     ifelse(NPS$month=="Sept", "09", 
                                                                            ifelse(NPS$month=="Okt", "10", 
                                                                                   ifelse(NPS$month=="Nov", "11", 
                                                                                          ifelse(NPS$month=="Dec", "12",0))))))))))))

NPS$`Andel 9+10` <- as.numeric(NPS$`Andel 9+10`)
NPSMonth <- NPS%>% group_by(Year, month) %>% summarise(mean(`Andel 9+10`, na.rm=T))
colnames(NPSMonth)[3] <- "NPS"

NPSrollmean <- roll_mean(NPSMonth$NPS, 11)
NPSYearMean <- NPSMonth[11:length(NPSMonth$Year),]
NPSYearMean <- NPSYearMean$Year
NPSMonthMean <- NPSMonth[11:length(NPSMonth$month),]
NPSMonthMean <- NPSMonthMean$month

NPSRoll <- as.data.frame(cbind(NPSYearMean, NPSMonthMean, NPSrollmean))
NPSRoll <- NPSRoll %>% mutate(Date = paste(NPSYearMean, NPSMonthMean, sep="-"))
NPSRoll <- NPSRoll[c(4, 3)]
NPSRoll$NPSrollmean <- as.numeric(as.character(NPSRoll$NPSrollmean))
NPSRoll <- NPSRoll[!is.na(NPSRoll$NPSrollmean),]
NPSRoll$NPSrollmean <- round(NPSRoll$NPSrollmean, 2)
colnames(NPSRoll)[2] <- "Mean"

#dbWriteTable(con, "NPS", value = NPSRoll, overwrite=TRUE, row.names = FALSE)

last <- NPSRoll[nrow(NPSRoll),2]
lastdate <- NPSRoll[nrow(NPSRoll),1]

inmin <- min(NPSRoll[2])
inmax <- max(NPSRoll[2])
mindate<- NPSRoll[which(NPSRoll[2]==inmin)[1], 1]
maxdate <- NPSRoll[which(NPSRoll[2]==inmax)[1], 1]

#Plot
n <- ggplot(NPSRoll, aes(x=Date)) 
n <- n + geom_line(aes(y=Mean, group=1), size=1, color="blue", alpha=0.5, stat="identity")
n <- n + annotate("text", x = lastdate, y = last+2, label = last, size=3)
n <- n + annotate("text", x = mindate, y = inmin+2, label=inmin, size=3)
#n <- n + annotate("text", x = maxdate, y = inmax+2, label=inmax, size=3)
n <- n + scale_fill_manual(values=c("#0000FF"), name="", labels=c("NPS/rullande 12 mån")) 
n <- n + scale_y_continuous(limits=c(0, 100))
n <- n + theme_light()
n <- n + theme(plot.margin = margin(10, 10, 20, 25), axis.text.x = element_text(angle = 25, hjust = 1, size = 7), axis.title = element_blank(), legend.position = "None")
n <- ggplotly(n, tooltip = c("y", "x"))

#Save
save(NPSRoll, file="Insert dir here")
save(n, file="Insert dir here")





