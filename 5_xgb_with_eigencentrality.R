#This script is for xgb with eigencentrality



set.seed(15)

data_list = list()
model_list = list()

#exit data
DF_Small = read_xlsx("DF_Small.xlsx")

M = DF_startup

#M= M%>%
#  select(-`INDUSTRY CLASSIFICATION`)

#dummy variables

M = dummy_cols(M, select_columns = "Group_country_portfolio",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "INDUSTRY CLASSIFICATION",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "PORTFOLIO COMPANY REGION",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "STAGE",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "PRIMARY INDUSTRY",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "Group_city_portfolio",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "PORTFOLIO COMPANY STATUS",remove_most_frequent_dummy = T,remove_selected_columns = T)



#merge stage after D
M$stage_after_D = rowSums(M[,c("STAGE_Series E","STAGE_Series F","STAGE_Series G","STAGE_Series H","STAGE_Series I",
                               "STAGE_Series J","STAGE_Series K","STAGE_Series L")])



M = M %>%
  select(-`STAGE_Series E`,
         -`STAGE_Series F`,
         -`STAGE_Series G`,
         -`STAGE_Series H`,
         -`STAGE_Series I`,
         -`STAGE_Series J`,
         -`STAGE_Series K`,
         -`STAGE_Series L`
         #-`INDUSTRY CLASSIFICATION_Unknown`
  )


Mdum = M
#Prepare data for XGB
for (i in 0:16){
  datetest = as.Date("2005-08-30") %m+% years(i)
  
  
  # 5years for training, labels on 2 years
  date_top = datetest %m+% years(2)
  date_low = datetest %m-% years(5)
  DF_startup1 = Mdum %>%
    filter(date_low<=`DEAL DATE`&`DEAL DATE`<date_top)
  
  
  DF_before_date = DF_startup1 %>%
    filter(`DEAL DATE`<= datetest)
  
  DF_after_date = DF_startup1 %>%
    filter(`DEAL DATE`> datetest)
  
  list_company_BD = unique(DF_before_date$`PORTFOLIO COMPANY ID`)
  list_company_AD = unique(DF_after_date$`PORTFOLIO COMPANY ID`)
  #labels
  DF_before_date = DF_before_date %>%
    mutate(labels = case_when(DF_before_date$`PORTFOLIO COMPANY ID`%in% list_company_AD ~ 1,
                              
                              !(DF_before_date$`PORTFOLIO COMPANY ID`%in% list_company_AD) ~ 0
    ))
  
  #Create exit dummy
  DF_Small_BD = DF_Small %>%
    filter(`DEAL DATE`<= datetest)
  
  exit_comp = DF_Small_BD$`PORTFOLIO COMPANY ID`
  
  
  DF_before_date= DF_before_date %>% 
    mutate( Sold_company = case_when(`PORTFOLIO COMPANY ID` %in% exit_comp ~1,
                                     !(`PORTFOLIO COMPANY ID` %in% exit_comp) ~0))
  
  
  
  
  #edge list startup ---> investor
  edge_list = DF_before_date %>%
    select(starts_with('IINV'), `PORTFOLIO COMPANY ID`)%>%
    tidyr::pivot_longer(cols = -`PORTFOLIO COMPANY ID`, names_to = "Investor_Index", values_to = "Investor") %>%
    filter(!is.na(Investor))%>%
    select(-Investor_Index)
  #graph
  graph = graph_from_data_frame(edge_list, directed = T)
  
  #Eigencentrality
  eigenvector_centrality = eigen_centrality(graph)
  eigenvector_scores = eigenvector_centrality$vector
  
  #Match with ID
  DF_before_date$`PORTFOLIO COMPANY ID` <- as.character(DF_before_date$`PORTFOLIO COMPANY ID`)
  
  DF_before_date = merge(DF_before_date, data.frame(`PORTFOLIO COMPANY ID` = V(graph)$name, EigenvectorCentrality = eigenvector_scores),
                         by.x = "PORTFOLIO COMPANY ID", by.y = "PORTFOLIO.COMPANY.ID", all.x = TRUE)
  
  
  #drop columns
  M = DF_before_date[, !grepl("^IINV", names(Mdum))]
  #Create diff_days variable
  M = M %>% mutate(DIFF_Days = as.numeric(difftime(datetest, `DEAL DATE`, units = "days"))) %>%
    select(-`DEAL DATE`)
  M = na.omit(M)
  colnames(M) = make.names(colnames(M))
  
  #test and training set by ID
  l = unique(M$PORTFOLIO.COMPANY.ID)
  l = data.frame(VectorColumn = l)
  l_split = initial_split(l, prop = .7)
  l_train = training(l_split)
  l_test  = testing(l_split)
  
  
  M_train = M %>%
    filter(PORTFOLIO.COMPANY.ID %in% l_train$VectorColumn)%>%
    select(-PORTFOLIO.COMPANY.ID)
  M_test = M %>%
    filter(PORTFOLIO.COMPANY.ID %in% l_test$VectorColumn)%>%
    select(-PORTFOLIO.COMPANY.ID)
  
  
  #store data
  data_name_train = paste("M_train", i, sep = "_")
  data_name_test = paste("M_test", i, sep = "_")
  data_list[[data_name_train]] = M_train
  data_list[[data_name_test]] = M_test
  
}

#grid for (small) tuning ! eta cannot be too small
hyper_grid = expand.grid(
  eta = c( .1,.2,.3),
  max_depth = c( 5, 7),
  min_child_weight = c(1, 3, 5),
  colsample_bytree = c(.8, 1)
  
)


results_matrix <- matrix(0, nrow = nrow(hyper_grid), ncol = 17)

#Tuning of xgb
for(i in 0:16) {
  
  #drop labels
  M =data_list[[paste0("M_train_", i)]]
  M_matrix = M%>%
    select(-labels)%>%
    data.matrix(M)
  
  
  set.seed(15)
  
  for (j in 1:nrow(hyper_grid)) {
    params = list(
      eta = hyper_grid$eta[j],
      max_depth = hyper_grid$max_depth[j],
      min_child_weight = hyper_grid$min_child_weight[j],
      colsample_bytree = hyper_grid$colsample_bytree[j]
    )
    
    
    m = xgboost(
      data = M_matrix,
      label = M$labels,
      nrounds = 100,
      objective = "binary:logistic",  
      verbose = 0,
      eval_metric = "error",
      params = params,
      early_stopping_rounds = 10
      
    )
    predictions = predict(m, M_matrix)
    pred= ifelse(predictions >(0.5-i*0.0064), 1, 0)
    
    #evaluation metric
    f0.5 = f_meas_vec(
      truth= as.factor(M$labels),
      estimate= as.factor(pred),
      beta =0.5,
      event_level = "second")
    
    results_matrix[j, i+1] <- f0.5
  }
  
  
  
}

#store results

write.csv(results_matrix, file = "~/Bachelor_Arbeit/data/results_Xgb_prob_with.csv", row.names = FALSE)

max_row = apply(results_matrix,2,which.max)


selected_rows <- hyper_grid[max_row, , drop = FALSE]






#5 folds cross-validation for xgb

for (i in 0:16) {
  M =data_list[[paste0("M_train_", i)]]
  M_matrix = M%>%
    select(-labels)%>%
    data.matrix(M)
  m = xgb.cv(
    data = M_matrix,
    label = M$labels,
    eta=selected_rows$eta[i+1],
    nrounds = 1000,
    nfold = 5,
    objective = "binary:logistic",  
    verbose = 0,
    eval_metric = "error",
    early_stopping_rounds = 10,
    max_depth=selected_rows$max_depth[i+1],
    min_child_weight= selected_rows$min_child_weight[i+1],
    colsample_bytree= selected_rows$colsample_bytree[i+1]
  )
  data_name = paste("m", i, sep = "")
  model_list[[data_name]] = m
}


#Define optimal rounds, with cross validation

optimal_rounds =c()

for (i in 0:16) {
  m = model_list[[paste0("m", i)]]
  optimal_rounds[i+1]= which.min(m$evaluation_log$test_error_mean)
  
}


#final model

for (i in 0:16) {
  M =data_list[[paste0("M_train_", i)]]
  M_matrix = M%>%
    select(-labels)%>%
    data.matrix(M)
  m = xgboost(
    data = M_matrix,
    label = M$labels,
    nrounds = optimal_rounds[i+1],
    objective = "binary:logistic",  
    verbose = 0,
    eval_metric = "error",
    max_depth=selected_rows$max_depth[i+1],
    min_child_weight= selected_rows$min_child_weight[i+1],
    colsample_bytree= selected_rows$colsample_bytree[i+1],
    eta=selected_rows$eta[i+1]
    
  )
  data_name = paste("m", i, sep = "")
  model_list[[data_name]] = m
}



#display results ! positive class = "1"
result_xgb = data.frame()


for (i in 0:16){
  
  m = model_list[[paste0("m", i)]]
  M = data_list[[paste0("M_test_", i)]]
  dtest <- M%>%
    select(-labels)%>%
    data.matrix(M)
  
  pred = predict(m,dtest)
  pred= ifelse(pred >(0.5-i*0.0064), 1, 0)
  conma = caret::confusionMatrix(as.factor(pred), as.factor(M$labels),positive = "1")
  model_name <- paste("Model", i, sep = "_")  # Create a unique column name for each model
  result_xgb[model_name, "Accuracy"] <- conma$overall["Accuracy"]
  result_xgb[model_name, "Kappa"] <- conma$overall["Kappa"]
  result_xgb[model_name, "Precision"] <- conma$byClass["Precision"]
  result_xgb[model_name, "Recall"] <- conma$byClass["Recall"]
  result_xgb[model_name, "Specificity"] <- conma$byClass["Specificity"]
  result_xgb[model_name, "F1"] <- conma$byClass["F1"]
  result_xgb[model_name, "Balanced Accuracy"] <- conma$byClass["Balanced Accuracy"]
  result_xgb[model_name, "f0.5"] <- f_meas_vec(
    truth= as.factor(M$labels),
    estimate= as.factor(pred),
    beta =0.5,
    event_level = "second")
}


#store results
result_mean_xgb_with_eig = as.matrix(colMeans(result_xgb))

write.csv(result_mean_xgb_with_eig, file = "~/Bachelor_Arbeit/data/Results/result_mean_xgb_with_eig.csv", row.names = T)
write.csv(result_xgb, file = "~/Bachelor_Arbeit/data/Results/result_xgb_with.csv", row.names = T)
