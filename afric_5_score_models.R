
#### Score model

score_model = function(train, test, pred_col) {

train[, pred := train[, get(pred_col)]]
test[, pred := test[, get(pred_col)]]
  
PRROC_obj <- roc.curve(scores.class0 = train$pred, weights.class0=train$fatal_cnt,
                       curve=TRUE)

plot(PRROC_obj)

ROCit_obj <- rocit(score=train$pred,class=train$fatal_cnt)
roc_values = plot(ROCit_obj, values = T)

test[, fatal_pred := ifelse(pred >= roc_values$`optimal Youden Index point`[4],1,0)]
test[, .N, keyby = .(fatal_pred, fatal_cnt)] %>% 
  dcast.data.table(fatal_pred~fatal_cnt)

train_AUC = roc_values$AUC

ROCit_obj_test <- rocit(score=test$pred,class=test$fatal_cnt)
roc_values_test = plot(ROCit_obj_test, values = T)

binomial_deviance = function(true, pred){
  mean(2*(-true * log(pred)- (1 - true)* log(1 - pred)))
}

test_AUC = roc_values_test$AUC
bin_dev = binomial_deviance(train$fatal_cnt, train$pred)
bin_dev_test = binomial_deviance(test$fatal_cnt, test$pred)

results = data.table(train_AUC = train_AUC,
                     test_AUC = test_AUC,
                     bin_dev = bin_dev,
                     bin_dev_test= bin_dev_test,
                     model = pred_col)

ROC = ROCit_obj_test
return(list(results = results, roc = ROC))

}

glm_scores = score_model(train, test, "pred_GLM")
NN_scores = score_model(train, test, "pred_NN")
NN_NE_scores = score_model(train, test, "pred_NN_NE")


# plot the first, then the second, then add the legend
plot(glm_scores$roc, col = c("green","blue"), 
     legend = FALSE, YIndex = FALSE)
lines(NN_scores$roc$TPR ~ NN_scores$roc$FPR,  col = "red", lwd = 2)
lines(NN_NE_scores$roc$TPR ~ NN_NE_scores$roc$FPR,  col = "purple", lwd = 2)
legend("bottomright", col = c(1,2),
       c("GLM ROC", "NN ROC", "NN with NE ROC"), lwd = 2)

all_results = rbind(glm_scores$results,NN_scores$results, NN_NE_scores$results)