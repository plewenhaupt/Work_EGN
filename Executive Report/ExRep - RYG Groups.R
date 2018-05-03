library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)

#       IMPORT  ################################################################
directory <- "insert dir here"
filelist <- list.files(directory)
exceldata <- lapply(filelist, function(x){read_excel(paste(directory, x, sep="/"), 2)})
exceldata <- lapply(exceldata, as.data.frame)

#       WRANGLE ################################################################
for(i in 1:length(exceldata)) {
        exceldata[[i]] <- exceldata[[i]][c("Group", "Members")]
}

date <- strsplit(filelist, ".", fixed = T)

for(i in 1:length(exceldata)) {
        exceldata[[i]][3] <- date[[i]][1]
}

for(i in 1:length(exceldata)) {
       exceldata[[i]] <- mutate(exceldata[[i]], Colour = ifelse(Members>19, "GREEN", ifelse(Members>16 & Members<20, "YELLOW", ifelse(Members<17,"RED","NA"))))
}

RYGdf <- bind_rows(exceldata)

colnames(RYGdf)[3] <- "Date"

RYGdf2 <- RYGdf %>% group_by(Date, Colour) %>% summarise(n())
RYGdf2 <- as.data.frame(RYGdf2)

lastweek <- RYGdf2[nrow(RYGdf2),1]
lastgreen <-RYGdf2[which(RYGdf2[1]==lastweek)[1], 3]
lastred <-RYGdf2[which(RYGdf2[1]==lastweek)[2], 3]
lastyellow <-RYGdf2[which(RYGdf2[1]==lastweek)[3], 3]


RYGdf2$Date <- as.Date(RYGdf2$Date)

colnames(RYGdf2)[3] <- "Count"

Totalgroup <- RYGdf2 %>% group_by(Date) %>% summarise(sum(Count))
Totalgroup <- as.data.frame(Totalgroup)
colnames(Totalgroup)[2] <- "Count"
lasttot <- Totalgroup[which(Totalgroup[1]==lastweek)[1], 2]


#       PLOT    ################################################################

o <- ggplot(RYGdf2, aes(x=Date, text = paste0("Date: ", Date)))
o <- o + geom_line(aes(y=Count, group=Colour, color=Colour), size=1)
o <- o + geom_line(data=Totalgroup, aes(y=Count, group=1), size=1)
o <- o + annotate("text", x = as.Date(lastweek), y = lastgreen+5, label = lastgreen, size=3)
o <- o + annotate("text", x = as.Date(lastweek), y = lastred+5, label=lastred, size=3)
o <- o + annotate("text", x = as.Date(lastweek), y = lastyellow+5, label=lastyellow, size=3)
o <- o + annotate("text", x = as.Date(lastweek), y = lasttot+5, label=lasttot, size=3)
o <- o + scale_colour_manual(values=c("#00CC00", "#FF0000", "#FFFF00"))
o <- o + scale_y_continuous(limits=c(0, 300)) 
o <- o + scale_x_date(date_labels = "%Y-%m")
o <- o + theme_light()
o <- o + theme(plot.margin = margin(10, 10, 20, 25), axis.text.x = element_text(hjust = 1, size = 7), axis.title = element_blank(), legend.position = "none")
o <- ggplotly(o, tooltip = c("y", "text"))


#SAVE
save(RYGdf2, file="Insert dir here")
save(Totalgroup, file="Insert dir here")
save(o, file="Insert dir here")