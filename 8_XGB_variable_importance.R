#Variables importance
#Same data preparation as for XGB_with_eigencentrality
#Important to run the XGB_with_eigencentrality before
set.seed(15)

data_list = list()
model_list = list()

DF_Small = read_xlsx("DF_Small.xlsx")

M = DF_startup

#M= M%>%
#  select(-`INDUSTRY CLASSIFICATION`)



M = dummy_cols(M, select_columns = "Group_country_portfolio",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "INDUSTRY CLASSIFICATION",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "PORTFOLIO COMPANY REGION",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "STAGE",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "PRIMARY INDUSTRY",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "Group_city_portfolio",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "PORTFOLIO COMPANY STATUS",remove_most_frequent_dummy = T,remove_selected_columns = T)




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

#Same preparation but some features names changes because it is more understandable in the plots

for (i in 0:16){
  datetest = as.Date("2005-08-30") %m+% years(i)
  
  
  
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
  
  DF_before_date = DF_before_date %>%
    mutate(labels = case_when(DF_before_date$`PORTFOLIO COMPANY ID`%in% list_company_AD ~ 1,
                              
                              !(DF_before_date$`PORTFOLIO COMPANY ID`%in% list_company_AD) ~ 0
    ))
  
  
  DF_Small_BD = DF_Small %>%
    filter(`DEAL DATE`<= datetest)
  
  exit_comp = DF_Small_BD$`PORTFOLIO COMPANY ID`
  
  #Exit_company instead of Sold_company in others scripts
  DF_before_date= DF_before_date %>% 
    mutate( Exit_company = case_when(`PORTFOLIO COMPANY ID` %in% exit_comp ~1,
                                     !(`PORTFOLIO COMPANY ID` %in% exit_comp) ~0))
  
  
  
  
  
  edge_list = DF_before_date %>%
    select(starts_with('IINV'), `PORTFOLIO COMPANY ID`)%>%
    tidyr::pivot_longer(cols = -`PORTFOLIO COMPANY ID`, names_to = "Investor_Index", values_to = "Investor") %>%
    filter(!is.na(Investor))%>%
    select(-Investor_Index)
  
  graph = graph_from_data_frame(edge_list, directed = T)
  
  eigenvector_centrality = eigen_centrality(graph)
  eigenvector_scores = eigenvector_centrality$vector
  
  
  DF_before_date$`PORTFOLIO COMPANY ID` <- as.character(DF_before_date$`PORTFOLIO COMPANY ID`)
  
  DF_before_date = merge(DF_before_date, data.frame(`PORTFOLIO COMPANY ID` = V(graph)$name, EigenvectorCentrality = eigenvector_scores),
                         by.x = "PORTFOLIO COMPANY ID", by.y = "PORTFOLIO.COMPANY.ID", all.x = TRUE)
  
  
  
  M = DF_before_date[, !grepl("^IINV", names(Mdum))]
  M = M %>% mutate(DIFF_Days = as.numeric(difftime(datetest, `DEAL DATE`, units = "days"))) %>%
    select(-`DEAL DATE`)
  M = na.omit(M)
  colnames(M) = make.names(colnames(M))
  
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
  
  
  
  data_name_train = paste("M_train", i, sep = "_")
  data_name_test = paste("M_test", i, sep = "_")
  data_list[[data_name_train]] = M_train
  data_list[[data_name_test]] = M_test
  
}

#load the tuning results
results_matrix = read_csv("results_Xgb_prob_with.csv")


max_row = apply(results_matrix,2,which.max)


selected_rows <- hyper_grid[max_row, , drop = FALSE]



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



#could also be taken from the other script
optimal_rounds =c()

for (i in 0:16) {
  m = model_list[[paste0("m", i)]]
  optimal_rounds[i+1]= which.min(m$evaluation_log$test_error_mean)
  
}


#The models

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
all_importance <- list()
#compute the importance
for (i in 0:16) {
  m <- model_list[[paste0("m", i)]]
  M <- data_list[[paste0("M_train_", i)]] %>%
    select(-labels)
  importance_matrix <- xgb.importance(feature_names = colnames(M), model = m)
  all_importance[[i + 1]] <- importance_matrix
}

combined_importance <- bind_rows(all_importance, .id = "Model")

#plot the importance
aggregated_importance <- combined_importance %>%
  group_by(Feature) %>%
  summarise(Mean_Gain = mean(Gain), Mean_Cover = mean(Cover), Mean_Frequency = mean(Frequency))%>%
  select(-Mean_Cover,-Mean_Frequency)%>%
  top_n(19, wt = Mean_Gain) %>%
  arrange(desc(Mean_Gain))

ggplot(aggregated_importance, aes(x = reorder(Feature, Mean_Gain), y = Mean_Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Feature", y = "Average Importance", title = "Aggregated Feature Importance (XGBoost)")

#Not necessary but nice to see, top 5 variables importance each year

for (i in 0:16) {
  m <- model_list[[paste0("m", i)]]
  M =data_list[[paste0("M_train_", i)]]%>%
    select(-labels)
  importance_matrix <- xgb.importance(feature_names = colnames(M), model = m)
  top_5 <- head(importance_matrix, 5)
  print(top_5)
  
  
}
