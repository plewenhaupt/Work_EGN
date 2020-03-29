#############
# PACKAGES  #
#############

source('~/Docs/Machine Learning/package_load_ml.R')

###################################
# MODEL 2 - fm_atlas_servicestatus#
###################################

rf_data_orig <- no_na_cols_data %>% 
  dplyr::select(fm_pin, fm_age, gendercode, fm_profilestrength, fm_physicalcondition, fm_interestrating, fm_interestedfulltime, fm_interestedparttime, fm_ngo_member, fm_atlas_servicestatus) %>%
  dplyr::filter(fm_pin != 'Anonym') %>% dplyr::mutate(TRM = ifelse(!is.na(fm_atlas_servicestatus), 'x', NA)) %>% dplyr::select(-fm_atlas_servicestatus)

rf_data_orig <- rf_data_orig %>% dplyr::mutate(fm_pin = gsub('-', '', fm_pin), gendercode = as.character(gendercode), fm_interestedfulltime = as.character(fm_interestedfulltime), fm_interestedparttime = as.character(fm_interestedparttime), fm_profilestrength = as.numeric(fm_profilestrength), fm_age = as.numeric(fm_age), fm_physicalcondition = as.character(fm_physicalcondition), fm_ngo_member = as.character(fm_ngo_member)) %>% dplyr::select(-fm_pin)

rf_data_x <- rf_data_orig %>% dplyr::filter(TRM == 'x')

rf_data_0 <- rf_data_orig %>% dplyr::filter(is.na(TRM))
rf_data_0 <- dplyr::sample_n(rf_data_0, nrow(rf_data_x))

rf_data <- dplyr::bind_rows(rf_data_x, rf_data_0)

rf_data[is.na(rf_data)] <- 0
rf_data$TRM <- as.factor(rf_data$TRM)

inTrain <- caret::createDataPartition(y = rf_data$TRM, p = .75, list = FALSE)

training <- rf_data[inTrain,]
training <- as.matrix(training)
xgb_traning <- xgb.DMatrix(training)
test <- rf_data[-inTrain,]
test <- as.matrix(test)

parametersGrid <-  expand.grid(eta = 0.3, 
                               colsample_bytree = 1,
                               max_depth = 6,
                               nrounds = 100,
                               gamma = 0,
                               min_child_weight = 1,
                               subsample = 1 
                                )

ControlParamteres <- trainControl(method = "cv",
                                  number = 4,
                                  savePredictions = TRUE,
                                  classProbs = FALSE, 
                                  verboseIter = TRUE
)

modelxgboost <- train(TRM~., 
                      data = training,
                      method = "xgbTree",
                      trControl = ControlParamteres,
                      tuneGrid=parametersGrid)

pred <- predict(modelxgboost, test)


