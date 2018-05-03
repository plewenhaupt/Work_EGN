library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)

#IMPORT
directory <- "Insert dir here"
filelist <- list.files(directory)
exceldata <- lapply(filelist, function(x){read_excel(paste(directory, x, sep="/"))})
exceldata <- lapply(exceldata, as.data.frame)

#       EXTRACT DATA    ########################################################
#Extract date
date <- lapply(exceldata, function(x){
        name <- x[1, 1]
        strsplit(name, " ")
})

date <- lapply(date, unlist)

year <- lapply(date, function(x){
        x[7]
})

dateno <- lapply(date, function(x){
        x[6]
})

dateno <- lapply(dateno, function(x){
        sub(",", "", x)
})

month <- lapply(date, function(x){
        x[5]
})

month <- lapply(month, function(x){match(x, month.name)})
month <- as.list(as.character(month))

date <- mapply(function(x, y, z){paste(x, y, z, sep = "-")}, year, month, dateno)

date <- ymd(date)

#Extract churn
onemonthchurn <- sapply(exceldata, function(x){
        och <- x[9, 6]
        as.numeric(unlist(och))
})

twelvemonthchurn <- sapply(exceldata, function(x){
        tch <- x[12, 6]
        as.numeric(unlist(tch))
})

#Extract other stuff - FEAST UPON THE DATA!
averagemembers <- sapply(exceldata, function(x){
        avm <- x[36, 6]
        as.numeric(unlist(avm))
})

#       CREATE DATAFRAME        ################################################
churndf <- cbind(date, onemonthchurn, twelvemonthchurn, averagemembers)
churndf <- data.frame(churndf)
churndf$date <- as.Date(churndf$date, origin="1970-01-01")
colnames(churndf) <- c("Date", "onemonth", "Retention", "Average_members")
churndf$onemonth <- 100 - (round(as.numeric(as.character(churndf$onemonth)), digits = 3) * 100)
churndf$Retention <- 100 - (round(as.numeric(as.character(churndf$Retention)), digits = 3) * 100)
churndf$Average_members <- round(as.numeric(as.character(churndf$Average_members)), 2)

#dbWriteTable(con, "Retention", value = churndf, overwrite=TRUE, row.names = FALSE)

ret <- churndf[nrow(churndf),3]
lastnum <- ret
lastdate <- churndf[nrow(churndf),1]

inmax <- max(churndf[3])
inmin <- min(churndf[3])
mindate<- churndf[which(churndf[3]==inmin)[1], 1]
maxdate <- churndf[which(churndf[3]==inmax)[1], 1]

#CHART THIS BADBOY
c <- ggplot(churndf, aes(x=Date, text = paste0("Date: ", Date))) 
c <- c + geom_line(aes(y=Retention), size=1, color="blue", group=1, alpha=.5)
c <- c + scale_x_date(date_labels = "%Y-%m")
#c <- c + geom_text(aes(y=churndf$twelvemonth), label=churndf$twelvemonth, size=2.5, vjust = -3)
#c <- c + scale_colour_manual(values=c("#0000FF"), name="") 
c <- c + annotate("text", x = lastdate, y = lastnum+1.5, label = ret, size=3)
c <- c + annotate("text", x = mindate, y = inmin-1.5, label=inmin, size=3)
c <- c + annotate("text", x = maxdate, y = inmax+1.5, label=inmax, size=3)
c <- c + labs(x=element_blank())
c <- c + scale_y_continuous(limits=c(0, 100)) 
c <- c + theme_light()
c <- c + theme(plot.margin = margin(10, 10, 20, 25), axis.text.x = element_text(angle = 25, hjust = 1, size = 7), axis.title = element_blank())
c <- ggplotly(c, tooltip = c("y", "text"))

#SAVE DATA FOR MARKDOWN
save(churndf, file="Insert dir here")
save(c, file="Insert dir here")




