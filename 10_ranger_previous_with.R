#This script is for the models trained on data with previous prediction as feature


setwd("~/Bachelor_Arbeit/data")
set.seed(175)

data_list = list()
model_list = list()

#load exit data
DF_Small = read_xlsx("DF_Small.xlsx")
#preparing data for the model, same as in other script
for (i in 0:16){
  datetest = as.Date("2005-08-30") %m+% years(i)
  
  
  
  date_top = datetest %m+% years(2)
  date_low = datetest %m-% years(5)
  DF_startup1 = DF_startup %>%
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
  
  
  DF_before_date= DF_before_date %>% 
    mutate( Sold_company = case_when(`PORTFOLIO COMPANY ID` %in% exit_comp ~1,
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
  
  
  
  M = DF_before_date[, !grepl("^IINV", names(DF_startup))]
  M = na.omit(M)%>% mutate(DIFF_Days = as.numeric(difftime(datetest, `DEAL DATE`, units = "days")))%>%
    select(-`DEAL DATE`)
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
grid = expand_grid( max.depth = 10,
                    num.trees = 100,
                    min.node.size = seq(2,12,by = 2),
                    mtry = seq(4,19,by = 5)
)

#Extract the results of the tuning in ranger_with_eigencentrality
results_matrix = read_csv("results_ranger_grid.csv")

max_row = apply(results_matrix,2,which.max)

selected_rows <- grid[max_row, , drop = FALSE]
#train and display the results

for(i in 1:17) {
  M = data_list[[paste0("M_train_", i-1)]]
  m = ranger(
    formula = labels ~ .,
    data    = M,
    num.trees = selected_rows$num.trees[i],
    max.depth = selected_rows$max.depth[i],
    min.node.size =selected_rows$min.node.size[i],
    mtry = selected_rows$mtry[i],
    importance = "impurity",
    classification = T,
    probability = T
    
  )
  data_name = paste("m", i-1, sep = "")
  model_list[[data_name]] = m
  
}

result_ranger=data.frame()

for (i in 0:16){
  M = data_list[[paste0("M_test_", i)]]
  m = model_list[[paste0("m", i)]]
  pred= predict(m,M)
  pred= ifelse(pred$predictions[,2] >(0.5-i*0.0064), 1, 0)
  
  conma = caret::confusionMatrix(as.factor(pred), as.factor(M$labels),positive = "1")
  model_name <- paste("Model", i, sep = "_")  # Create a unique column name for each model
  result_ranger[model_name, "Accuracy"] <- conma$overall["Accuracy"]
  result_ranger[model_name, "Kappa"] <- conma$overall["Kappa"]
  result_ranger[model_name, "Precision"] <- conma$byClass["Precision"]
  result_ranger[model_name, "Recall"] <- conma$byClass["Recall"]
  result_ranger[model_name, "Specificity"] <- conma$byClass["Specificity"]
  result_ranger[model_name, "F1"] <- conma$byClass["F1"]
  result_ranger[model_name, "Balanced Accuracy"] <- conma$byClass["Balanced Accuracy"]
  result_ranger[model_name, "f0.5"] <- f_meas_vec(
    truth= as.factor(M$labels),
    estimate= as.factor(pred),
    beta =0.5,
    event_level = "second")
  
  
}

#Create a data with a new feature from last predictions
#Feature is the mean of the prediction from 2 years ago (weighted 2 times)
#and the prediction made on the data from 3 years ago (weighted 1 time)
data_list_previous = list()

for(i in 0:13){
  M = data_list[[paste0("M_train_", i+3)]]
  m_1 = model_list[[paste0("m", i)]]
  m_2 = model_list[[paste0("m", i+1)]]
  
  pred1 = predict(m_1,M)
  pred2 = predict(m_2,M)
  
  mean_previous = rowMeans(cbind(
    pred1$predictions[, 1],pred1$predictions[, 1],
    pred2$predictions[, 1]))
  M = cbind(M,mean_previous)
  data_name_train = paste("M_train", i+3, sep = "_")
  data_list_previous[[data_name_train]] = M
  
  M = data_list[[paste0("M_test_", i+3)]]
  m_1 = model_list[[paste0("m", i)]]
  m_2 = model_list[[paste0("m", i+1)]]
  
  pred1 = predict(m_1,M)
  pred2 = predict(m_2,M)
  
  mean_previous = rowMeans(cbind(
    pred1$predictions[, 1],pred1$predictions[, 1],
    pred1$predictions[, 1]))
  M = cbind(M,mean_previous)
  
  data_name_test = paste("M_test", i+3, sep = "_")
  data_list_previous[[data_name_test]] = M
}





#Train model on new data with previous prediction as a future

model_list_previous = list()

for (i in 0:13){
  M = data_list_previous[[paste0("M_train_", i+3)]]
  m = ranger(
    formula = labels ~ .,
    data    = M,
    num.trees = selected_rows$num.trees[i+4],
    max.depth = selected_rows$max.depth[i+4],
    min.node.size =selected_rows$min.node.size[i+4],
    mtry = selected_rows$mtry[i+3],
    importance = "impurity",
    classification = T,
    probability=T
    
  )
  data_name = paste("m", i+3, sep = "")
  model_list_previous[[data_name]] = m
}


#Display the results
result_ranger_with_prev = data.frame()


for (i in 3:16){
  M = data_list_previous[[paste0("M_test_", i)]]
  m = model_list_previous[[paste0("m", i)]]
  pred= predict(m,M)
  pred= ifelse(pred$predictions[,2] >(0.5-i*0.0064), 1, 0)
  
  conma = caret::confusionMatrix(as.factor(pred), as.factor(M$labels), positive = "1")
  model_name <- paste("Model", i, sep = "_")  # Create a unique column name for each model
  result_ranger_with_prev[model_name, "Accuracy"] <- conma$overall["Accuracy"]
  result_ranger_with_prev[model_name, "Kappa"] <- conma$overall["Kappa"]
  result_ranger_with_prev[model_name, "Precision"] <- conma$byClass["Precision"]
  result_ranger_with_prev[model_name, "Recall"] <- conma$byClass["Recall"]
  result_ranger_with_prev[model_name, "Specificity"] <- conma$byClass["Specificity"]
  result_ranger_with_prev[model_name, "F1"] <- conma$byClass["F1"]
  result_ranger_with_prev[model_name, "Balanced Accuracy"] <- conma$byClass["Balanced Accuracy"]
  result_ranger_with_prev[model_name, "f0.5"] <- f_meas_vec(
    truth= as.factor(M$labels),
    estimate= as.factor(pred),
    beta =0.5,
    event_level = "second")
}

#Store the results and calculate the difference with the results without the additional feature
result_mean_ranger_previous = as.matrix(colMeans(result_ranger_with_prev))

write.csv(result_mean_ranger_previous, file = "~/Bachelor_Arbeit/data/Results/result_mean_ranger_previous.csv", row.names = T)
write.csv(result_ranger_with_prev, file = "~/Bachelor_Arbeit/data/Results/result_ranger_with_prev.csv", row.names = T)

diff = result_ranger_with_prev-result_ranger[4:nrow(result_ranger),]
diff=  as.data.frame(colMeans(diff))
write.csv(diff, file = "~/Bachelor_Arbeit/data/Results/diff_previous.csv", row.names = T)




# (not necessary) see the variables importance

for (i in 3:16) {
  m <- model_list_previous[[paste0("m", i)]]
  model_name <- paste("Model", i, sep = "_")
  var_importance <- m$variable.importance
  top_5_vars <- head(arrange(data.frame(Variable = names(var_importance), Importance = var_importance), desc(Importance)), 5)
  cat("Top 5 variables for", model_name, ":\n")
  print(top_5_vars)
  cat("\n")

}

