
## Fitting and plotting classification models for image

get_classification = function(no_folds, no_repeats, treatment, combdata, monodata){
  
  ## Defining data to use for the rest of the function
  if(treatment == "combination"){
    dat = combdata
  } else if(treatment == "monotherapy"){
    dat = monodata
  }
  
  
  
  # Looking at data balance
  
  
  balance_tab = table(dat$response)
  
  
  
  # Training accuracy
  
  ## Initialising lists to store balanced accuracies
  glm_acc = glm_balacc = lasso_acc = lasso_balacc = rf_acc = rf_balacc = svm_acc = svm_balacc = xgb_acc = xgb_balacc = list()
  
  ## Setting seed for reproducibility
  set.seed(2021)
  
  ## Performing cross-validation
  for(repeats in 1:no_repeats){
    
    ## Defining folds to use
    cvIndex = caret::createFolds(dat$response, k = no_folds, returnTrain = TRUE)
    
    ## Initialising list of confusion matrices
    glm_conmat = lasso_conmat = rf_conmat = svm_conmat = xgb_conmat = list()
    
    for(fold in 1:no_folds){
      
      ## Training data
      train_dat = dat %>% .[cvIndex[[fold]], ] %>% dplyr::select(-imageid, -batch)
      train_x = train_dat %>% dplyr::select(-response)
      train_y = train_dat %>% pull(response)
      
      ## Testing data
      test_dat = dat %>% .[-cvIndex[[fold]], ] %>% dplyr::select(-imageid, -batch)
      test_x = test_dat %>% dplyr::select(-response)
      test_y = test_dat %>% pull(response) 
      
      
      
      ## Fitting models
      glm_model = suppressWarnings(
        glm(response ~ ., 
            data = train_dat, 
            family = binomial(link = "logit"))
      )
      
      
      lasso_train = na.omit(train_dat)
      cv_lasso = cv.glmnet(x = lasso_train %>% dplyr::select(-response) %>% unlist() %>% matrix(nrow = nrow(lasso_train)),
                           y = as.numeric(lasso_train$response) - 1, 
                           alpha = 1, family = "binomial") # Obtaining best lambda using CV
      
      lasso_model = suppressWarnings(
        glmnet(x = lasso_train %>% dplyr::select(-response) %>% unlist() %>% matrix(nrow = nrow(lasso_train)),
               y = as.numeric(lasso_train$response) - 1,
               alpha = 1, family = "binomial", lambda = cv_lasso$lambda.min
        )
      )
      
      
      rf_model = suppressWarnings(
        randomForest::randomForest(response ~ .,
                                   data = janitor::clean_names(train_dat)) # Need to clean names for randomforest to work
      )
      
      
      svm_model = suppressWarnings(
        svm(train_x, train_y)
      )
      
      
      xgb_model = suppressWarnings(
        xgboost(data = unlist(train_x) %>% matrix(nrow = nrow(train_x)),
                label = as.numeric(train_y) - 1, # xgb needs label to be numeric 0 < y < 1
                max_depth = 2, eta = 1, nthread = 1, nrounds = 2, objective = "binary:logistic")
      )
      
      
      ## Predictions on test data
      glm_pred = suppressWarnings(
        predict(glm_model, test_x, type = "response") %>% 
          {ifelse(. > 0.5, "Responder", "Non-responder")} %>%  # Cutoff at probability =  0.5
          as.factor()
      )
      
      
      lasso_test = na.omit(test_dat)
      lasso_pred = suppressWarnings(
        predict(lasso_model, newx = unlist(lasso_test %>% select(-response)) %>% matrix(nrow = nrow(lasso_test))) %>% 
          {ifelse(. > 0.5, "Responder", "Non-responder")} %>%  # Cutoff at probability =  0.5
          as.factor()
      )
      
      
      rf_pred = suppressWarnings(
        predict(rf_model, janitor::clean_names(test_x), type = "response")
      )
      
      
      svm_pred = suppressWarnings(
        predict(svm_model, test_x)
      )
      
      
      xgb_pred = suppressWarnings(
        predict(xgb_model, unlist(test_x) %>% matrix(nrow = nrow(test_x))) %>% 
          {ifelse(. > 0.5, "Responder", "Non-responder")} %>%  # Cutoff at probability =  0.5
          as.factor()
      )
      
      
      ## Confusion matrices
      glm_conmat[[fold]] = caret::confusionMatrix(data = glm_pred, 
                                                  reference = test_y)
      
      lasso_conmat[[fold]] = caret::confusionMatrix(data = lasso_pred, 
                                                  reference = lasso_test$response)
      
      rf_conmat[[fold]] = caret::confusionMatrix(data = rf_pred,
                                                 reference = test_y)
      
      svm_conmat[[fold]] = caret::confusionMatrix(data = svm_pred,
                                                  reference = test_y)
      
      xgb_conmat[[fold]] = caret::confusionMatrix(data = xgb_pred,
                                                  reference = test_y)
      
      cat(paste("Completed: fold", fold))
    }
    
    
    ## Accuracies
    glm_acc[[repeats]] = sapply(glm_conmat, function(x) x$overall[["Accuracy"]]) %>% mean()
    lasso_acc[[repeats]] = sapply(lasso_conmat, function(x) x$overall[["Accuracy"]]) %>% mean()
    rf_acc[[repeats]] = sapply(rf_conmat, function(x) x$overall[["Accuracy"]]) %>% mean()
    svm_acc[[repeats]] = sapply(svm_conmat, function(x) x$overall[["Accuracy"]]) %>% mean()
    xgb_acc[[repeats]] = sapply(xgb_conmat, function(x) x$overall[["Accuracy"]]) %>% mean()
    
    ## Balanced accuracies
    glm_balacc[[repeats]] = sapply(glm_conmat, function(x) x$byClass[["Balanced Accuracy"]]) %>% mean()
    lasso_balacc[[repeats]] = sapply(lasso_conmat, function(x) x$byClass[["Balanced Accuracy"]]) %>% mean()
    rf_balacc[[repeats]] = sapply(rf_conmat, function(x) x$byClass[["Balanced Accuracy"]]) %>% mean()
    svm_balacc[[repeats]] = sapply(svm_conmat, function(x) x$byClass[["Balanced Accuracy"]]) %>% mean()
    xgb_balacc[[repeats]] = sapply(xgb_conmat, function(x) x$byClass[["Balanced Accuracy"]]) %>% mean()
    
    cat(paste("Completed: repeat", repeats))
  }
  
  
  
  # Plotting
  
  
  
  ## Accuracy
  
  
  ## Getting data into appropriate form
  plot_data_acc = unlist(glm_acc) %>% 
    cbind(unlist(lasso_acc),
          unlist(rf_acc),
          unlist(svm_acc),
          unlist(xgb_acc)) %>% 
    `colnames<-`(c("glm", "lasso", "rf", "svm", "xgb")) %>% 
    as.data.frame() %>% 
    pivot_longer(cols = 1:5, names_to = "method", values_to = "value")
  
  ## Boxplot accuracies
  training_acc_p = plot_data_acc %>% 
    ggplot() +
    aes(x = method, y = value, fill = method) +
    labs(x = "", y = "Accuracy",
         title = paste0(str_to_title(treatment), ": Model Accuracy on Training Data"),
         subtitle = paste0("Repeated CV; n = ", no_repeats, ", k = ", no_folds, "\n", balance_tab[2], " responders & ", balance_tab[1], " non-responders")) +
    scale_x_discrete(labels = c("Logistic Regression", "Logistic Regression\n(Lasso)", "RandomForest", "SVM", "XGBoost")) +
    geom_boxplot() +
    # geom_jitter(width = 0.2, size = 0.2) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    theme_classic() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  
  
  ## Balanced Accuracy
  
  
  ## Getting data into appropriate form
  plot_data_balacc = unlist(glm_balacc) %>% 
    cbind(unlist(lasso_balacc),
          unlist(rf_balacc),
          unlist(svm_balacc),
          unlist(xgb_balacc)) %>% 
    `colnames<-`(c("glm", "lasso", "rf", "svm", "xgb")) %>% 
    as.data.frame() %>% 
    pivot_longer(cols = 1:5, names_to = "method", values_to = "value")
  
  ## Boxplot balanced accuracies
  training_balacc_p = plot_data_balacc %>% 
    ggplot() +
    aes(x = method, y = value, fill = method) +
    labs(x = "", y = "Balanced Accuracy",
         title = paste0(str_to_title(treatment), ": Model Balanced Accuracy on Training Data"),
         subtitle = paste0("Repeated CV; n = ", no_repeats, ", k = ", no_folds, "\n", balance_tab[2], " responders & ", balance_tab[1], " non-responders")) +
    scale_x_discrete(labels = c("Logistic Regression", "Logistic Regression\n(Lasso)", "RandomForest", "SVM", "XGBoost")) +
    geom_boxplot() +
    # geom_jitter(width = 0.2, size = 0.2) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    theme_classic() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  return(
    list(balance_tab = balance_tab,
         training_acc_p = training_acc_p,
         training_balacc_p = training_balacc_p)
  )
}