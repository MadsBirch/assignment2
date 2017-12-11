data = data_BackUp

#--------Create null list for results --------
m_comp2 = data.frame()

features = list("range","sd", "IQR", "nsyll", "npause", "RR", "DET", "NRLINE", "maxL", "L", "ENTR", "rENTR", "LAM", "TT", "dur..s.", "phonationtime..s.", "speechrate..nsyll.dur.", "speechrate..nsyll.dur.", "articulation.rate..nsyll...phonationtime.", "ASD..speakingtime.nsyll.")

#data_BackUp = data
#----------------Loop----------------
for (f in features){
  #Empty data frame
  feat_results = data.frame()
  for (f2 in features){
    if (f != f2)
      for (k in folds){
        #Change the name of the ith variable in f to "f", e.g. range --> f
        setnames(data, f, "f")
        setnames(data, f2, "f2")
        #------ Split into training and test data ------ 
        #Create training dataset, data not in fold k
        data_train=subset(data,!(SubjectN %in% k))
        #Create test dataset, data in fold k
        data_test=subset(data,SubjectN %in% k)
        
        
        
        #------ train model - apply model to data_train ------
        #Model uses the variable renamed to f above
        logis_model = glmer(Diagnosis ~ f + f2 + (1|Study), family = binomial, data_train, control = glmerControl(calc.derivs = FALSE))
        
        #Get predicted value for training data, and round to 0 or 1
        predict_train = predict(logis_model, data_train, type = 'response')
        predict_train = ifelse(predict_train > 0.5, 1, 0)
        
        #Make confusion matrix and other stats
        cm_train = confusionMatrix(data = predict_train, reference = data_train$Diagnosis)
        
        roc_train = roc(response = data_train$Diagnosis, predictor = predict_train)
        auc_train = auc(roc_train)
        
        #------ test the model - test model on data_test (last quarter) ------
        #Make predictions based on modeVIS
        predict_test=predict(logis_model, data_test, type = 'response', allow.new.levels = TRUE)
        predict_test = ifelse(predict_test > 0.5, 1, 0)
        
        cm_test = confusionMatrix(data = predict_test, reference = data_test$Diagnosis)
        
        roc_test = roc(response = data_test$Diagnosis, predictor = predict_test)
        auc_test = auc(roc_test)
        
        #------ save the performance ------ 
        #Save all the variables in one row
        one_row = data.frame(acc_train = cm_train$overall[1], acc_test = cm_test$overall[1], sens_train = cm_train$byClass[1], sens_test = cm_test$byClass[1], spec_train = cm_train$byClass[2], spec_test = cm_test$byClass[2], auc_train = auc_train, auc_test = auc_test,  npv_train = cm_train$byClass[3], npv_test = cm_test$byClass[3], ppv_train = cm_train$byClass[4], ppv_test = cm_test$byClass[4])
        
        #Add the row to the data set
        feat_results = rbind(feat_results, one_row)
        
        #rename the variable back to "f", e.g. f --> range
        setnames(data, "f", f)
        setnames(data, "f2", f2)
      }
    #Take mean of the test stats
    feat_results2 = data.frame(f1 = f, f2 = f2, acc_train = mean(feat_results$acc_train), acc_test = mean(feat_results$acc_test), sens_train = mean(feat_results$sens_train), sens_test =  mean(feat_results$sens_test), spec_train = mean(feat_results$spec_train), spec_test =  mean(feat_results$spec_test), auc_train =  mean(feat_results$auc_train), auc_test =  mean(feat_results$auc_test),  npv_train =  mean(feat_results$npv_train), npv_test =  mean(feat_results$npv_test), ppv_train =  mean(feat_results$ppv_train), ppv_test =  mean(feat_results$ppv_test))
    m_comp2 = rbind(m_comp2, feat_results2)
    }
}
