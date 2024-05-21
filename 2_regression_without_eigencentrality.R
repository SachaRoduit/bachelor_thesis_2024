
#This script is for the regression without the eigencentrality feature.




set.seed(175)
data_list = list()
model_list = list()

#This is the data with the exit event of the company
DF_Small = read_xlsx("DF_Small.xlsx")

M = DF_startup

#This feature has to be dropped because of a too high correlation with primary industry
M= M%>%
  select(-`INDUSTRY CLASSIFICATION`)

#Dummy variables creation

M = dummy_cols(M, select_columns = "Group_country_portfolio",remove_most_frequent_dummy = T,remove_selected_columns = T)
#M = dummy_cols(M, select_columns = "INDUSTRY CLASSIFICATION",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "PORTFOLIO COMPANY REGION",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "STAGE",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "PRIMARY INDUSTRY",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "Group_city_portfolio",remove_most_frequent_dummy = T,remove_selected_columns = T)
M = dummy_cols(M, select_columns = "PORTFOLIO COMPANY STATUS",remove_most_frequent_dummy = T,remove_selected_columns = T)


#Stage after Stage D are merged

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

#Preparing the data for analysis

for (i in 0:16){
  #reference date for the first 5 year time window
  datetest = as.Date("2005-08-30") %m+% years(i)
  
  
  #5 Years for training, 2 Years for labels
  date_top = datetest %m+% years(2)
  date_low = datetest %m-% years(5)
  DF_startup1 = Mdum %>%
    filter(date_low<=`DEAL DATE`&`DEAL DATE`<date_top)
  
  #training data
  DF_before_date = DF_startup1 %>%
    filter(`DEAL DATE`<= datetest)
  #labels data
  DF_after_date = DF_startup1 %>%
    filter(`DEAL DATE`> datetest)
  #list of unique company
  list_company_BD = unique(DF_before_date$`PORTFOLIO COMPANY ID`)
  list_company_AD = unique(DF_after_date$`PORTFOLIO COMPANY ID`)
  #labels creation
  DF_before_date = DF_before_date %>%
    mutate(labels = case_when(DF_before_date$`PORTFOLIO COMPANY ID`%in% list_company_AD ~ 1,
                              
                              !(DF_before_date$`PORTFOLIO COMPANY ID`%in% list_company_AD) ~ 0
    ))
  
  #adding the dummy variable if a startup had a exit event or not being carful to not add future information
  DF_Small_BD = DF_Small %>%
    filter(`DEAL DATE`<= datetest)
  
  exit_comp = DF_Small_BD$`PORTFOLIO COMPANY ID`
  
  
  DF_before_date= DF_before_date %>% 
    mutate( Sold_company = case_when(`PORTFOLIO COMPANY ID` %in% exit_comp ~1,
                                     !(`PORTFOLIO COMPANY ID` %in% exit_comp) ~0))
  
  
  
  #Creating an edge list with Startup- Investor
  
  edge_list = DF_before_date %>%
    select(starts_with('IINV'), `PORTFOLIO COMPANY ID`)%>%
    tidyr::pivot_longer(cols = -`PORTFOLIO COMPANY ID`, names_to = "Investor_Index", values_to = "Investor") %>%
    filter(!is.na(Investor))%>%
    select(-Investor_Index)
  
  #Directed graph creation
  graph = graph.data.frame(edge_list, directed = T)
  
  # Eigencentrality calculation
  eigenvector_centrality = eigen_centrality(graph)
  eigenvector_scores = eigenvector_centrality$vector
  
  #Matching with ID to get the new feature "EigenvectorCentrality"
  DF_before_date$`PORTFOLIO COMPANY ID` <- as.character(DF_before_date$`PORTFOLIO COMPANY ID`)
  
  DF_before_date = merge(DF_before_date, data.frame(`PORTFOLIO COMPANY ID` = V(graph)$name, EigenvectorCentrality = eigenvector_scores),
                         by.x = "PORTFOLIO COMPANY ID", by.y = "PORTFOLIO.COMPANY.ID", all.x = TRUE)
  
  #Drop columns with unique investors and features "EigenvectorCentrality", DEAL DATE
  
  M = DF_before_date[, !grepl("^IINV", names(Mdum))]
  #Create diff_days variable
  M = M %>% mutate(DIFF_Days = as.numeric(difftime(datetest, `DEAL DATE`, units = "days")))%>%
    select(-`EigenvectorCentrality`,-`DEAL DATE`)
  M = na.omit(M)
  colnames(M) = make.names(colnames(M))
  
  #Splitting the data for test and training by company ID
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
  
  #Store Data
  
  data_name_train = paste("M_train", i, sep = "_")
  data_name_test = paste("M_test", i, sep = "_")
  data_list[[data_name_train]] = M_train
  data_list[[data_name_test]] = M_test
  
}


#Regression

for (i in 0:16) {
  M = data_list[[paste0("M_train_", i)]]
  m = glm(
    formula = labels ~ .,
    data    = M)
  data_name = paste("m", i, sep = "")
  model_list[[data_name]] = m
}

result_reg = data.frame()

#Results !!!!manually set the positive class

for (i in 0:16){
  M = data_list[[paste0("M_test_", i)]]
  m = model_list[[paste0("m", i)]]
  pred= predict(m,M)
  pred= ifelse(pred >(0.5-i*0.0064), 1, 0)
  
  conma = caret::confusionMatrix(as.factor(pred), as.factor(M$labels),positive = "1")
  model_name <- paste("Model", i, sep = "_")  # Create a unique column name for each model
  result_reg[model_name, "Accuracy"] <- conma$overall["Accuracy"]
  result_reg[model_name, "Kappa"] <- conma$overall["Kappa"]
  result_reg[model_name, "Precision"] <- conma$byClass["Precision"]
  result_reg[model_name, "Recall"] <- conma$byClass["Recall"]
  result_reg[model_name, "Specificity"] <- conma$byClass["Specificity"]
  result_reg[model_name, "F1"] <- conma$byClass["F1"]
  result_reg[model_name, "Balanced Accuracy"] <- conma$byClass["Balanced Accuracy"]
  result_reg[model_name, "f0.5"] <- f_meas_vec(
    truth= as.factor(M$labels),
    estimate= as.factor(pred),
    beta =0.5,
    event_level= "second")
}


#Drop data created in process (not necessary)
rm(M,M_test,M_train,Mdum,m,l_train,l_test,l_split,l,graph,edge_list,eigenvector_centrality,
   DF_after_date,DF_before_date,DF_Small_BD,DF_Small,conma,DF_startup1)
rm(data_name,data_name_test,data_name_train,date_low,date_top,datetest,eigenvector_scores,
   exit_comp,i,list_company_AD,list_company_BD,model_name,pred)

#Means over all years
result_mean_reg = as.matrix(colMeans(result_reg))

#Store data in csv
write.csv(result_mean_reg, file = "~/Bachelor_Arbeit/data/Results/result_mean_reg_without_eig.csv", row.names = T)
write_csv(result_reg, file= "~/Bachelor_Arbeit/data/Results/result_reg_without_eig.csv")