#############
# PACKAGES  #
#############
source('~/Docs/Machine Learning/package_load_ml.R')

###################################
# MODEL 2 - fm_atlas_servicestatus#
###################################


conf_mat <- fit$finalModel$confusion.matrix
pred_error <- fit$finalModel$prediction.error

atlas <- orig_data %>% dplyr::group_by(fm_atlas_servicestatus) %>% dplyr::tally()

#Predict on test data
pred <- predict(fit, newdata = test)

test$pred <- pred

result <- test %>% dplyr::group_by(TRM, pred) %>% tally()
result <- result %>% 
  group_by(TRM) %>% 
  mutate(CountT = sum(n)) %>% 
  ungroup %>% 
  mutate(percent_num = n/CountT, percent_char = percent(percent_num), percent = percent_num*100, percent_labels = paste0(percent_char, " (", n, ")"))


