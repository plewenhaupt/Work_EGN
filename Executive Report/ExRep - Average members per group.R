library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)

#       IMPORT  #################################################################
directory <- "Insert dir here"
filelist <- list.files(directory)
exceldata <- lapply(filelist, function(x){read_excel(paste(directory, x, sep="/"))})
exceldata <- lapply(exceldata, as.data.frame)

#       EXTRACT DATA    #########################################################
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

#Extract other stuff - FEAST UPON THE DATA!
averagemembers <- sapply(exceldata, function(x){
        avm <- x[36, 6]
        as.numeric(unlist(avm))
})

#       CREATE DF       #########################################################
avgmemdf <- cbind.data.frame(date, averagemembers)
colnames(avgmemdf) <- c("Date", "Average")
avgmemdf$Average <- round(avgmemdf$Average, 2)

#dbWriteTable(con, "average_members", value = avgmemdf, overwrite=TRUE, row.names = FALSE)


net <- avgmemdf[nrow(avgmemdf),2]
lastdate <- avgmemdf[nrow(avgmemdf),1]

inmax <- max(avgmemdf[2])
inmin <- min(avgmemdf[2])
mindate <- avgmemdf[which(avgmemdf[2]==inmin)[1], 1]
maxdate <- avgmemdf[which(avgmemdf[2]==inmax)[1], 1]

#       CHART THIS BADBOY       #################################################
u <- ggplot(avgmemdf, aes(x=Date, text = paste0("Date: ", as.Date(Date))))
u <- u + geom_line(aes(y=Average), size=1, group=1, color="blue", alpha=.5)
u <- u + theme_light()
u <- u + scale_x_date(date_labels="%Y-%m")

if(net==inmin){
        u <- u + annotate("text", x = lastdate, y = net+.2, label = net, size=3)
        u <- u + annotate("text", x = maxdate, y = inmax+.2, label=inmax, size=3)
        u <- u + scale_y_continuous(limits=c(0, 50)) 
        u <- u + theme(plot.margin = margin(10, 10, 20, 25), axis.text.x = element_text(angle=25, hjust = 1, size = 7), axis.title = element_blank(), legend.position = "none")
        u <- ggplotly(u, tooltip = c("y", "text"))    
} else {
        u <- u + annotate("text", x = lastdate, y = net+.2, label = net, size=3)
        u <- u + annotate("text", x = mindate, y = inmin-.2, label=inmin, size=3)
        u <- u + annotate("text", x = maxdate, y = inmax+.2, label=inmax, size=3)
        u <- u + scale_y_continuous(limits=c(0, 50)) 
        u <- u + theme(plot.margin = margin(10, 10, 20, 25), axis.text.x = element_text(hjust = 1, size = 7), axis.title = element_blank(), legend.position = "none")
        u <- ggplotly(u, tooltip = c("y", "text"))   
}


#SAVE DATA FOR MARKDOWN
save(avgmemdf, file="Insert dir here")
save(u, file="Insert dir here")
