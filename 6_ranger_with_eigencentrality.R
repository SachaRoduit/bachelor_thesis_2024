#This script is for the ranger with eigencentrality



setwd("~/Bachelor_Arbeit/data")
set.seed(175)

data_list = list()
model_list = list()


#load exit data
DF_Small = read_xlsx("DF_Small.xlsx")

#Preparing data for ranger, same as without eigencentrality but the feature isn't drop
for (i in 0:16){
  datetest = as.Date("2005-08-30") %m+% years(i)
  
  
  #5 years training, 2 years labels
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
  
  #labels
  DF_before_date = DF_before_date %>%
    mutate(labels = case_when(DF_before_date$`PORTFOLIO COMPANY ID`%in% list_company_AD ~ 1,
                              
                              !(DF_before_date$`PORTFOLIO COMPANY ID`%in% list_company_AD) ~ 0
    ))
  
  #dummy for exit event
  DF_Small_BD = DF_Small %>%
    filter(`DEAL DATE`<= datetest)
  
  exit_comp = DF_Small_BD$`PORTFOLIO COMPANY ID`
  
  
  DF_before_date= DF_before_date %>% 
    mutate( Sold_company = case_when(`PORTFOLIO COMPANY ID` %in% exit_comp ~1,
                                     !(`PORTFOLIO COMPANY ID` %in% exit_comp) ~0))
  
  
  
  #edge list startup -->investor
  
  edge_list = DF_before_date %>%
    select(starts_with('IINV'), `PORTFOLIO COMPANY ID`)%>%
    tidyr::pivot_longer(cols = -`PORTFOLIO COMPANY ID`, names_to = "Investor_Index", values_to = "Investor") %>%
    filter(!is.na(Investor))%>%
    select(-Investor_Index)
  
  #Directed graph
  graph = graph_from_data_frame(edge_list, directed = T)
  #Eigencentrality computation
  eigenvector_centrality = eigen_centrality(graph)
  eigenvector_scores = eigenvector_centrality$vector
  
  #Match by ID
  DF_before_date$`PORTFOLIO COMPANY ID` <- as.character(DF_before_date$`PORTFOLIO COMPANY ID`)
  
  DF_before_date = merge(DF_before_date, data.frame(`PORTFOLIO COMPANY ID` = V(graph)$name, EigenvectorCentrality = eigenvector_scores),
                         by.x = "PORTFOLIO COMPANY ID", by.y = "PORTFOLIO.COMPANY.ID", all.x = TRUE)
  
  
  #Drop unused columns
  M = DF_before_date[, !grepl("^IINV", names(DF_startup))]
  #Create diff_days variable
  M = na.omit(M)%>% mutate(DIFF_Days = as.numeric(difftime(datetest, `DEAL DATE`, units = "days")))%>%
    select(-`DEAL DATE`)
  colnames(M) = make.names(colnames(M))
  
  #split train and test set by ID
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

#(not necessary) remove unused data
rm(DF_after_date,DF_before_date,DF_Small_BD,edge_list,eigenvector_centrality,graph,l, l_split,
   l_test,l_train,DF_startup1)

#Create a grid for (small) tuning
grid = expand_grid( max.depth = 10,
                    num.trees = 100,
                    min.node.size = seq(2,12,by = 2),
                    mtry = seq(4,19,by = 5)
)

results_matrix <- matrix(0, nrow = nrow(grid), ncol = 17)

#ranger tuning
for (i in 0:16) {
  for(j in 1:nrow(grid)) {
    M = data_list[[paste0("M_train_", i)]]
    m = ranger(
      formula = labels ~ .,
      data    = M,
      num.trees = grid$num.trees[j],
      max.depth = grid$max.depth[j],
      min.node.size =grid$min.node.size[j],
      mtry = grid$mtry[j],
      importance = "none",
      classification = T,
      probability = T
      
    )
    
    pred= ifelse(m$predictions[,2] >(0.5-i*0.0064), 1, 0)
    
    #metric for evaluation
    f0.5 = f_meas_vec(
      truth= as.factor(M$labels),
      estimate= as.factor(pred),
      beta =0.5,
      event_level = "second")
    
    results_matrix[j, i+1] <- f0.5
    
  }
  
}
#store results

write.csv(results_matrix, file = "~/Bachelor_Arbeit/data/results_ranger_grid.csv", row.names = FALSE)




max_row = apply(results_matrix,2,which.max)

selected_rows <- grid[max_row, , drop = FALSE]

#train final model
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


#display results !positive class = "1"
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


#store results
result_mean_ranger = as.matrix(colMeans(result_ranger))

write.csv(result_mean_ranger, file = "~/Bachelor_Arbeit/data/Results/result_mean_ranger_with.csv", row.names = T)
write.csv(result_ranger, file = "~/Bachelor_Arbeit/data/Results/result_ranger_with.csv", row.names = T)
