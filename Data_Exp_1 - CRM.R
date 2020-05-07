#########
# TODO  #
#########
#What to do with >50 numeric variables?
#What to do with the character variables?
# - Breakdown of each (# of unique) - I already have this in the grouped dfs, but how to visualize them?


#############
# PACKAGES  #
#############
source('~/Docs/Machine Learning/package_load_ml.R')

########
# ETL  #
########
#Remove columns:

################
# IMPORT DATA  #
################
#variables <- read_csv('./european-social-survey-ess-8-ed21-201617/variables.csv')
#col_types <- variables %>% select(Type) %>% mutate(stringrepr = ifelse(Type == 'discrete', 'c', 'n'))
#typelist <- as.list(col_types$stringrepr)

orig_data <- read_excel('~/file.xlsx')

orig_data[orig_data == 'NULL'] <- NA

#Check ID length, to find new rows
id <- orig_data$Id %>% as.list()
ln <- sapply(id, nchar)
len <- tibble(id = id, ln = ln) %>% filter(ln != 36)

info <- file.info('~/file.xlsx')

bytes <- info[1,1]
megabytes <- info[1, 1]/10^6


##############################
# DATA DIMENSIONS AND TYPES  #
##############################
dim_tib <- tibble(rows = nrow(orig_data), columns = ncol(orig_data))
cols <- colnames(orig_data)
classes_grouped <- sapply(orig_data, class) %>% unlist() %>% tibble(class = .) %>% group_by(class) %>% tally()
  
  

skimdf <- skim(orig_data)

##########################
# SPLIT DATA INTO TYPES  #
##########################

data_types <- classes_grouped[!base::grepl('^POSIX',classes_grouped$class), ] %>% select(class) %>% pull(.)


is_s <- lapply(data_types, function(x){
  paste0('is.', x)
})

type_dfs <- lapply(is_s, function(x){
  orig_data %>% select_if(eval(parse(text=x)))
})

################################
# UNIQUES VALUES PER VARIABLE  #
################################

uniques <- lapply(orig_data, unique)
unique_n <- lapply(uniques, length) %>% unlist()
uniques_char_collapsed <- lapply(uniques, function(x){
  y <- sort(x) 
  c <- as.character(y)
  paste(c, collapse = ' ')
})

###################
# MISSING VALUES  #
###################
#Total values in data set
#No of missing
#No of complete

missing_totals <- tibble(Entity = c('Total dataset', 'Complete', 'Missing'), 
                         Data_points = c(pull(dim_tib[1, 1])*pull(dim_tib[1, 2]), naniar::n_complete(orig_data), naniar::n_miss(orig_data)),
                         Percent = c(100, naniar::pct_complete(orig_data), naniar::pct_miss(orig_data)))

classtib <- tibble(variable = colnames(orig_data), class = sapply(orig_data, class))

missing_summary <- naniar::miss_var_summary(orig_data) %>% dplyr::rename(missing_n = n_miss, missing_percent = pct_miss) %>% dplyr::mutate(n_complete = nrow(orig_data) - missing_n) %>% dplyr::arrange(variable)
variable_summary <- left_join(missing_summary, classtib, by = 'variable')
variable_summary$n_unique <- unique_n
variable_summary$uniques <- uniques_char_collapsed
all_na_cols <- variable_summary %>% select(variable, missing_n) %>% filter(missing_n == nrow(orig_data)) %>% select(variable) %>% unlist()
no_na_cols_data <- orig_data %>% select(-(all_na_cols))

##### ML DATASET END HERE #####

n_missing_in_vars <- naniar::miss_var_table(orig_data) %>% dplyr::rename(n_missing_in_variable = n_miss_in_var, percent_vars = pct_vars)


plot_missing_rows <- 
  naniar::gg_miss_case(orig_data, order_cases = FALSE, show_pct = TRUE) +
  theme_bw()

plot_missing_vars <- 
  naniar::gg_miss_var(orig_data, show_pct = FALSE) + 
  theme_bw() + 
  theme(axis.text.x = element_text(hjust = 1, size = 9), axis.title.y = element_blank(), axis.text.y=element_blank())

plot_missing_order_na <- function(data, seg = 200, col = NULL) {
  
  library(ggplot2)
  library(reshape)
  
  columns <- colnames(data)
  ncols <- length(columns)
  nrows <- nrow(data)
  segments <- ifelse(seg > 0, seg, 1)
  
  missing_intensity <- matrix(0, ncols, (segments + 1))
  seg_size <- nrows / segments
  
  index <- seq(from = 0, to = nrows, by = seg_size)
  if (index[length(index)] < nrows) {
    index <- c(index, nrows)
  }
  
  index <- round(index)
  
  if (length(index) < (segments + 1)) {
    segments <- segments - 1
    missing_intensity <- matrix(0, ncols, (segments + 1))
  }
  
  for (i in 1:ncols) {
    index_i <- is.na(data[, columns[i]])
    missing_intensity[i, 1] <- sum(index_i) / nrows * 100
    for (j in 1:segments) {
      start_index <- index[j] + 1
      end_index <- index[j + 1]
      index_j <- is.na(data[start_index:end_index, columns[i]])
      missing_intensity[i, (j + 1)] <- sum(index_j) / (end_index - start_index + 1) * 100
    }
  }
  
  general_rate <- round(mean(missing_intensity[, 1]), 2)
  sort_index <- sort.int(-missing_intensity[, 1], index.return = T)$ix
  missing_intensity <- data.frame(missing_intensity)
  rownames(missing_intensity) <- columns
  colnames(missing_intensity) <- c('All', c(1:segments));
  missing_intensity <- missing_intensity[sort_index,]
  rnames <- factor(rownames(missing_intensity), levels = rev(rownames(missing_intensity)), ordered = F)
  
  missing_intensity <- cbind(Columns = rnames, missing_intensity)
  missvalues <- melt(missing_intensity, id = c('Columns'))
  
  new.palette <- colorRampPalette(c('royalblue4', 'seagreen3', 'yellow', 'orangered', 'black'))(100)
  
  if (!is.null(col))
    new.palette = col
  
  ggplot(missvalues, aes(x = variable, y = Columns, fill = value)) +
    geom_tile() +
    xlab('Segments') + 
    ggtitle(paste0('Missing Value Rate (', general_rate, '%)')) +
    scale_fill_gradientn(name = 'Missing\nvalue', colours = new.palette) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 5), axis.ticks.y = element_blank())
}

plot_missing_order_col <- function(data, seg = 200, col = NULL) {
  
  library(ggplot2)
  library(reshape)
  
  columns <- colnames(data)
  ncols <- length(columns)
  nrows <- nrow(data)
  segments <- ifelse(seg > 0, seg, 1)
  
  missing_intensity <- matrix(0, ncols, (segments + 1))
  seg_size <- nrows / segments
  
  index <- seq(from = 0, to = nrows, by = seg_size)
  if (index[length(index)] < nrows) {
    index <- c(index, nrows)
  }
  
  index <- round(index)
  
  if (length(index) < (segments + 1)) {
    segments <- segments - 1
    missing_intensity <- matrix(0, ncols, (segments + 1))
  }
  
  for (i in 1:ncols) {
    index_i <- is.na(data[, columns[i]])
    missing_intensity[i, 1] <- sum(index_i) / nrows * 100
    for (j in 1:segments) {
      start_index <- index[j] + 1
      end_index <- index[j + 1]
      index_j <- is.na(data[start_index:end_index, columns[i]])
      missing_intensity[i, (j + 1)] <- sum(index_j) / (end_index - start_index + 1) * 100
    }
  }
  
  general_rate <- round(mean(missing_intensity[, 1]), 2)
  sort_index <- sort.int(-missing_intensity[, 1], index.return = T)$ix
  missing_intensity <- data.frame(missing_intensity)
  rownames(missing_intensity) <- columns
  colnames(missing_intensity) <- c('All', c(1:segments));
  missing_intensity <- missing_intensity[sort_index,]
  rnames <- factor(rownames(missing_intensity), levels = rev(rownames(missing_intensity)), ordered = F)
  
  missing_intensity <- cbind(Columns = rnames, missing_intensity)
  missvalues <- melt(missing_intensity, id = c('Columns')) %>% mutate(Columns = as.character(Columns)) %>% arrange(Columns)
  
  new.palette <- colorRampPalette(c('royalblue4', 'seagreen3', 'yellow', 'orangered', 'black'))(100)
  
  if (!is.null(col))
    new.palette = col
  
  ggplot(missvalues, aes(x = variable, y = Columns, fill = value)) +
    geom_tile() +
    xlab('Segments') + 
    ggtitle(paste0('Missing Value Rate (', general_rate, '%)')) +
    scale_fill_gradientn(name = 'Missing\nvalue', colours = new.palette) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 5), axis.ticks.y = element_blank())
}

missing_plot_na_orig <- plot_missing_order_na(orig_data)
missing_plot_na_no_nas <- plot_missing_order_na(no_na_cols_data)

missing_plot_cols_orig <- plot_missing_order_col(orig_data)
missing_plot_cols_no_nas <- plot_missing_order_col(no_na_cols_data)

######################
# SAVE OUTPUT STEP 1 #
######################
save(megabytes, file = "megabytes.RData")
save(dim_tib, file = "dim_tib.RData")
save(classes_grouped, file = "classes_grouped.RData")
save(skimdf, file = "skimdf.RData")
save(missing_totals, file = "missing_totals.RData")
save(missing_summary, file = "missing_summary.RData")
save(variable_summary, file = "variable_summary.RData")
save(n_missing_in_vars, file = "n_missing_in_vars.RData")
save(plot_missing_vars, file = "plot_missing_vars.RData")
save(missing_plot_na_orig, file = "missing_plot_na_orig.RData")
save(missing_plot_na_no_nas, file = "missing_plot_na_no_nas.RData")
save(missing_plot_cols_orig, file = "missing_plot_cols_orig.RData")
save(missing_plot_cols_no_nas, file = "missing_plot_cols_no_nas.RData")

rmarkdown::render("Data_Exp1.Rmd")

beep(5)

#######################
# DISTRIBUTION PLOTS  #
#######################

#Plot layout
lay <- rbind(c(1,1,1,1,1),
             c(2,2,2,2,2),
             c(2,2,2,2,2),
             c(2,2,2,2,2),
             c(2,2,2,2,2),
             c(2,2,2,2,2),
             c(2,2,2,2,2))

#number of rows and columns in PDF
n_rows <- 5
n_cols <- 3

##### Histograms of numeric data #####
######################################
num <- match("numeric", data_types)

if(!is.na(num)){
#Create dfs of each column, grouped and tallied
num_grouped <- lapply(seq_along(type_dfs[[num]]), function(x){
  colname <- names(type_dfs[[num]][x])
  df <- type_dfs[[num]][x] %>% group_by(eval(parse(text=colname))) %>% tally()
  colnames(df)[1] <- colname
  df[is.na(df)] <- 'Empty'
  return(df)
})

#Create histograms
hist_num <- lapply(num_grouped, function(x){
  colname <- colnames(x[1])
  histograms_numeric <- ggplot(data = x, aes(x = eval(parse(text=colname)))) + 
    geom_bar(aes(y = n), stat = 'identity') +
    theme_bw() +
    theme(axis.text.x = element_text(hjust = 1, size = 7, angle = 35), axis.text.y = element_text(size = 7), axis.title = element_blank(), legend.title = element_blank(), legend.position = "None")
})

num_vis <- lapply(1:length(hist_num), function(x) {
  arrangeGrob(textGrob(paste0(colnames(num_grouped[[x]][1]), ' (', x, ')')),
              hist_num[[x]],
              layout_matrix = lay)
})
num_grid <- marrangeGrob(grobs = num_vis, ncol = n_cols, nrow = n_rows)
ggsave(filename = "num_grid.pdf", plot = num_grid)

} else {
  print('No numeric columns!')
}

##### Histograms of character data #####
######################################
char <- match("character", data_types)

if(!is.na(char)){
  #Create dfs of each column, grouped and tallied
  char_grouped <- lapply(seq_along(type_dfs[[char]]), function(x){
    colname <- names(type_dfs[[char]][x])
    df <- type_dfs[[char]][x] %>% group_by(eval(parse(text=colname))) %>% tally()
    colnames(df)[1] <- colname
    df[is.na(df)] <- 'Empty'
    return(df)
  })
  
  
#Create histograms
  hist_char <- lapply(char_grouped, function(x){
    colname <- colnames(x[1])
    histograms_character <- ggplot(data = x, aes(x = eval(parse(text=colname)))) + 
                          geom_bar(aes(y = n), stat = 'identity')  +
                          theme_bw() +
                          theme(axis.text.x = element_text(hjust = 1, size = 7, angle = 35), axis.text.y = element_text(size = 7), axis.title = element_blank(), legend.title = element_blank(), legend.position = "None")
  })
  
  char_vis <- lapply(1:length(hist_char), function(x) {
    arrangeGrob(textGrob(paste0(colnames(char_grouped[[x]][1]), ' (', x, ')')),
                hist_char[[x]],
                layout_matrix = lay)
  })
  char_grid <- marrangeGrob(grobs = char_vis, ncol = n_cols, nrow = n_rows)
  
  ggsave(filename = "char_grid.pdf", plot = char_grid)
  
} else {
  print('No character columns!')
}

##### Histograms of factor data ######
######################################
factor <- match("factor", data_types)

if(!is.na(factor)){
  #Create dfs of each column, grouped and tallied
  factor_grouped <- lapply(seq_along(type_dfs[[factor]]), function(x){
    colname <- names(type_dfs[[factor]][x])
    df <- type_dfs[[factor]][x] %>% group_by(eval(parse(text=colname))) %>% tally()
    colnames(df)[1] <- colname
    df[is.na(df)] <- 'Empty'
    return(df)
  })
  
  #Create histograms
  hist_fact <- lapply(factor_grouped, function(x){
    colname <- colnames(x[1])
    histograms_factor <- ggplot(data = x, aes(x = eval(parse(text=colname)))) + 
      geom_bar(aes(y = n), stat = 'identity')  +
      theme_bw() +
      theme(axis.text.x = element_text(hjust = 1, size = 7, angle = 35), axis.text.y = element_text(size = 7), axis.title = element_blank(), legend.title = element_blank(), legend.position = "None")
  })
} else {
  print('No factor columns!')
}

##### Histograms of logical data ######
######################################
logical <- match("logical", data_types)

if(!is.na(logical)){
  #Create dfs of each column, grouped and tallied
  logical_grouped <- lapply(seq_along(type_dfs[[logical]]), function(x){
    colname <- names(type_dfs[[logical]][x])
    df <- type_dfs[[logical]][x] %>% group_by(eval(parse(text=colname))) %>% tally()
    colnames(df)[1] <- colname
    df[is.na(df)] <- 'Empty'
    return(df)
  })
  
  #Create histograms
  hist_log <- lapply(logical_grouped, function(x){
    colname <- colnames(x[1])
    histograms_log <- ggplot(data = x, aes(x = eval(parse(text=colname)))) + 
      geom_bar(aes(y = n), stat = 'identity')  +
      theme_bw() +
      theme(axis.text.x = element_text(hjust = 1, size = 7, angle = 35), axis.text.y = element_text(size = 7), axis.title = element_blank(), legend.title = element_blank(), legend.position = "None")
  })
} else {
  print('No logical columns!')
}

############################
# COVARIANCE OF VARIABLES  #
############################

### NUMERIC ###
#Find standard deviation of numeric variables. If SD = 0, then exclude from correlation matrix.

s <- lapply(type_dfs[[num]], sd, na.rm = TRUE) %>% bind_rows() %>% t() %>% tibble(variable = rownames(.), SD = .) %>% filter(SD != 0)
num_vars <- type_dfs[[num]] %>% select(s$variable)
n_missing_in_num_vars <- naniar::miss_var_table(num_vars) %>% dplyr::rename(n_missing_in_variable = n_miss_in_var, percent_vars = pct_vars)

#Small dataset (variables(columns) < 40)
########################################
if(ncol(num_vars) < 50){
  #Correlogram
  corr_matrix <- cor(num_vars, method = 'spearman')
  col<- colorRampPalette(c("red", "white", "blue"))(20)
  corr_plot <- corrplot(corr_matrix, type="lower", col=col, tl.cex = .5, tl.col="black", tl.srt=45, na.label = ".")
  
  corr_matrix_hmisc <- Hmisc::rcorr(as.matrix(num_vars), type = "spearman")
  
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  
  corrdf <- flattenCorrMatrix(corr_matrix_hmisc$r, corr_matrix_hmisc$P) %>% arrange(row)
  sig_corr <- corrdf %>% filter(p < 0.05) %>% filter(cor < -0.3 | cor > 0.3)

  flattenNMatrix <- function(nmat) {
    ut <- upper.tri(nmat)
    data.frame(
      row = rownames(nmat)[row(nmat)[ut]],
      column = rownames(nmat)[col(nmat)[ut]],
      cor  =(nmat)[ut]
    )
  }
  
  n_corrdf <- flattenNMatrix(corr_matrix_hmisc$n) %>% arrange(row) %>% na.omit %>% tibble()
  
}

### CATEGORICAL ###

#################################
# PRINCIPAL COMPONENT ANALYSIS  #
#################################
#N.B. PCA can require some data cleaning
pc <- pca(num_vars, nPcs=2, method="ppca") 



#######################
# SAVE OUTPUT STEP 2  #
#######################

save(corr_matrix, file = "corr_matrix.RData")
save(corr_matrix_hmisc, file = "corr_matrix_hmisc.RData")
save(col, file = "col.RData")
save(corrdf, file = "corrdf.RData")
save(n_corrdf, file = "n_corrdf.RData")








