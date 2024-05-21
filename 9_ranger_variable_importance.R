#Variable importance
#Important to run ranger_with_eigencentrality before


setwd("~/Bachelor_Arbeit/data")
set.seed(175)

data_list = list()
model_list = list()

DF_Small = read_xlsx("DF_Small.xlsx")
#Same data preparation but some features names have been changed to be better understand in the plots
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

#load the result of the tuning
results_matrix = read_csv("results_ranger_grid.csv")

max_row = apply(results_matrix,2,which.max)

selected_rows <- grid[max_row, , drop = FALSE]


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


#Compute the Importance
all_importance <- bind_rows(lapply(0:16, function(i) {
  m <- model_list[[paste0("m", i)]]
  tibble(Variable = names(m$variable.importance), 
         Importance = m$variable.importance, 
         Model = paste0("Model_", i))
}), .id = "Model")

aggregated_importance <- all_importance %>%
  group_by(Variable) %>%
  summarize(AggregateImportance = mean(Importance),
            sd = sd(Importance))


#Plot the importance
ggplot(aggregated_importance, aes(x = reorder(Variable, AggregateImportance), y = AggregateImportance)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Flips the axes for better readability
  labs(x = "Feature", y = "Average Importance", title = "Aggregated Variable Importance (Random Forest)")

#(not necessary) See the top 5 most important variable

for (i in 0:16) {
  m <- model_list[[paste0("m", i)]]
  model_name <- paste("Model", i, sep = "_")
  var_importance <- m$variable.importance
  top_5_vars <- head(arrange(data.frame(Variable = names(var_importance), Importance = var_importance), desc(Importance)), 5)
  cat("Top 5 variables for", model_name, ":\n")
  print(top_5_vars)
  cat("\n")
}

