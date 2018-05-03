library(tidyverse)
library(readxl)
library(plotly)
library(scales)

seq <- c(2:51)
seq2 <- c(1:50)

last_date_of_week <- function(year, week){
        strptime(paste(year, week, 5), format = "%Y %W %u")
}

#       IMPORT  ################################################################
directory <- "Insert dir here"
filelist <- list.files(directory)
exceldata <- lapply(filelist, function(y){
        lapply(seq, function(x){read_excel(paste(directory, y, sep="/"), x)})
})

#Name all dfs in the list of lists
for (i in 1:length(exceldata)){
        for (j in 1:length(exceldata[[i]])){
                names(exceldata[[i]])[j] <- colnames(exceldata[[i]][[j]])[1]
        }       
}

#Convert all to dataframe
for (i in 1:length(exceldata)){
        for (j in 1:length(exceldata[[i]])){
                exceldata[[i]][[j]] <- as.data.frame(exceldata[[i]][[j]])
        }       
}

#Extract vectors of week/year, in, out, diff
weekyear <- lapply(exceldata, function(y){
        lapply(y, function(x){
                n <- names(x[1])
        })
})

weekyear <- as.list(bind_rows(weekyear))
weekyear <- lapply(weekyear, unique)
weekyear <- lapply(weekyear, function(x){
        weekyear <- x[!is.na(x)]
})

weekyear <- lapply(weekyear, function(x){
        if(length(x)>1){
                weekyear <- max(x)
        } else {
                x
        }
})
weekyear <- bind_rows(weekyear)

#Members in
IN <- lapply(exceldata, function(y){
        lapply(y, function(x){
                inlappl <- x[1, 2] 
        })
})

IN <- as.list(bind_rows(IN))
IN <- lapply(IN, unique)
IN <- lapply(IN, function(x){
        IN <- x[!is.na(x)]
})

IN <- lapply(IN, function(x){
        if(length(x)>1){
                IN <- max(x)
        } else {
                x
        }
})
IN <- bind_rows(IN)

#Members out
OUT <- lapply(exceldata, function(y){
        lapply(y, function(x){
                inlappl <- x[3, 2] 
        })
})

OUT <- as.list(bind_rows(OUT))
OUT <- lapply(OUT, unique)
OUT <- lapply(OUT, function(x){
        OUT <- x[!is.na(x)]
})

OUT <- lapply(OUT, function(x){
        if(length(x)>1){
                OUT <- max(x)
        } else {
                x
        }
})
OUT <- bind_rows(OUT)

#Members diff
DIFF <- lapply(exceldata, function(y){
        lapply(y, function(x){
                inlappl <- x[5, 2] 
        })
})

DIFF <- as.list(bind_rows(DIFF))
DIFF <- lapply(DIFF, unique)
DIFF <- lapply(DIFF, function(x){
        DIFF <- x[!is.na(x)]
})

DIFF <- lapply(DIFF, function(x){
        if(length(x)>1){
                DIFF <- max(x)
        } else {
                x
        }
})
DIFF <- bind_rows(DIFF)

df <- bind_rows(weekyear, IN, OUT, DIFF)
dft <- as.data.frame(t(df))
dft[1] <- as.character(dft[,1])
WK <- sub(".*?uge (.*?)", "\\1", dft[,1])
WK <- as.data.frame(strsplit(WK, split= " "))
WK <- as.data.frame(t(WK))
dft <- bind_cols(dft, WK)
colnames(dft) <- c("weekyear", "iin", "out", "diff", "week", "year")
dft <- dft[c(1, 6, 5, 2, 3, 4)]

#Change classes
dft$year <- as.numeric(as.character(dft$year))
dft$week <- as.numeric(as.character(dft$week))
dft$iin <- as.numeric(as.character(dft$iin))
dft$out <- as.numeric(as.character(dft$out))
dft$diff <- as.numeric(as.character(dft$diff))

#Fix dates
dft <- dft %>% arrange(year, week)
dft$Netto <- cumsum(dft$diff)
dft$POSixt <- last_date_of_week(dft$year, dft$week)
dft$date <- as.Date(format(dft$POSixt, "%Y-%m-%d"))
dft <- dft[!is.na(dft$date),]

#       PLOT    ################################################################
t <- ggplot(dft, aes(x=POSixt, text = paste0("Date: ", as.Date(POSixt))))
t <- t + geom_line(aes(y=Netto, fill="Netto"), size=1, color="red", group=1) 
t <- t + geom_bar(aes(y=iin, fill="In"), stat ="identity", alpha=0.3)
t <- t + geom_bar(aes(y=out, fill="Out"), stat ="identity", alpha=0.3)
t <- t + scale_fill_manual(values=c("#0000FF", "#FF0000", "#330033"), guide=F) 
t <- t + geom_hline(yintercept = 0) 
t <- t + theme_light()
t <- t + theme(plot.margin = margin(10, 10, 20, 25), axis.text.x = element_text(hjust = 0.5, size = 7), axis.title = element_blank(), legend.title = element_blank())
t <- ggplotly(t, tooltip = c("y", "text"))

#SAVE DATA FOR MARKDOWN
save(dft, file="Insert dir here")
save(t, file="Insert dir here")
