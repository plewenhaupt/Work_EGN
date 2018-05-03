library(tidyverse)
library(readxl)
library(lubridate)
library(ggrepel)
library(plotly)

#Remember to remove Budget and Division columns from Excel-sheet

#IMPORT
directory <- "Insert dir here"
filelist <- list.files(directory)
exceldata <- lapply(filelist, function(x){
        read_excel(paste(directory, x, sep="/"))
        })
exceldata <- lapply(exceldata, as.data.frame)

renewdf <- exceldata[[length(exceldata)]]

renewdf$`Renewal Month` <- month.abb[renewdf$`Renewal Month`]
renewdf$`Renewal Month` <- as.factor(renewdf$`Renewal Month`)

monthNow <- month(Sys.Date())
LvlsStart <- monthNow:12
monthPassed <- monthNow-1
LvlsEnd <- seq(1, monthPassed, 1)
Lvls <- c(LvlsStart, LvlsEnd)


renewdf$`Renewal Month` <-  factor(renewdf$`Renewal Month`, levels=month.abb[Lvls])
renewdf <- renewdf %>% mutate(Seniority = ifelse(SeniorityYears == 0, "First Year", ifelse(SeniorityYears == 1, "Second Year", "Three years +")))
renewdf$Seniority <- factor(renewdf$Seniority, levels=c("First Year", "Second Year", "Three years +"))

renewaldf <- renewdf %>% group_by(`Renewal Month`, Seniority) %>% summarise(n())
colnames(renewaldf) <- c("Month", "Seniority", "Count")

#month.abb[month(Sys.Date)] + seq(1, month(Sys.Date) - 1, 1) and print the result in the clamps
#levels=month.abb[c(month.abb[month(Sys.Date)]:12)]

#dbWriteTable(con, "Renewals", value = renewaldf, overwrite=TRUE, row.names = FALSE)

chartdf <- renewaldf %>% group_by(Month) %>% summarise(sum(Count))
maxl <- round(max(chartdf[,2]),-2)


#PLOT
i <- ggplot(renewaldf, aes(x=Month)) 
i <- i + geom_bar(aes(y=Count, fill=Seniority), alpha=0.5, width=.7, stat="identity")
i <- i + scale_fill_manual(values=c("blue", "dodgerblue3","deepskyblue1"))
i <- i + scale_y_continuous(limits=c(0, maxl))
i <- i + theme_light()
i <- i + theme(plot.margin = margin(10, 10, 10, 15), axis.text.x = element_text(hjust = 1, size = 7), axis.title = element_blank())
i <- ggplotly(i)

#SAVE
save(renewaldf, file="Insert dir here")
save(i, file="insert dir here")
