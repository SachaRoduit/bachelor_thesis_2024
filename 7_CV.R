#Cross-validation for Random Forest


set.seed(175)

data_list = list()
model_list = list()

DF_Small = read_xlsx("DF_Small.xlsx")
#Preparing the data same as in ranger_with but keep the ID
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
  
  graph = graph.data.frame(edge_list, directed = T)
  
  eigenvector_centrality = eigen_centrality(graph)
  eigenvector_scores = eigenvector_centrality$vector
  
  
  DF_before_date$`PORTFOLIO COMPANY ID` <- as.character(DF_before_date$`PORTFOLIO COMPANY ID`)
  
  DF_before_date = merge(DF_before_date, data.frame(`PORTFOLIO COMPANY ID` = V(graph)$name, EigenvectorCentrality = eigenvector_scores),
                         by.x = "PORTFOLIO COMPANY ID", by.y = "PORTFOLIO.COMPANY.ID", all.x = TRUE)
  
  
  
  M = DF_before_date[, !grepl("^IINV", names(DF_startup))]
  M = na.omit(M)
  colnames(M) = make.names(colnames(M))
  
  l = unique(M$PORTFOLIO.COMPANY.ID)
  l = data.frame(VectorColumn = l)
  l_split = initial_split(l, prop = .7)
  l_train = training(l_split)
  l_test  = testing(l_split)
  #keep the ID
  M_train = M %>%
    filter(PORTFOLIO.COMPANY.ID %in% l_train$VectorColumn)
  
  M_test = M %>%
    filter(PORTFOLIO.COMPANY.ID %in% l_test$VectorColumn)%>%
    select(-PORTFOLIO.COMPANY.ID)
  
  
  data_name_train = paste("M_train", i, sep = "_")
  data_name_test = paste("M_test", i, sep = "_")
  data_list[[data_name_train]] = M_train
  data_list[[data_name_test]] = M_test
  
}
#The grid will be replaced by the tuned value of ranger_with
grid = expand_grid( max.depth = 10,
                    num.trees = 100,
                    min.node.size = seq(2,12,by = 2),
                    mtry = seq(4,19,by = 5)
)
results_matrix = read_csv("results_ranger_grid.csv")


max_row = apply(results_matrix,2,which.max)

selected_rows <- grid[max_row, , drop = FALSE]


#Manual cross validation
sd = c()
mean = c()
for(j in 0:16){
  M <- data_list[[paste0("M_train_", j)]]
  
  
  data_list_CV = list()
  
  #Create 5 folds out of the training data
  for (i in 1:5){
    
    l = sample(unique(M$PORTFOLIO.COMPANY.ID))
    n=length(l)/5
    testid = l[((i-1)*n):(i*n)]
    trainid = l[!l %in% testid]
    CVtrain = M %>%
      filter(PORTFOLIO.COMPANY.ID %in% trainid)%>%
      select(-PORTFOLIO.COMPANY.ID)
    CVtest = M %>%
      filter(PORTFOLIO.COMPANY.ID %in% testid)%>%
      select(-PORTFOLIO.COMPANY.ID)
    data_name_train = paste("CVtrain", i, sep = "_")
    data_name_test = paste("CVtest", i, sep = "_")
    data_list_CV[[data_name_train]] = CVtrain
    data_list_CV[[data_name_test]] = CVtest
    
  }
  
  
  #Performe the cross validation
  
  fcv=c()
  
  for(i in 1:5) {
    M = data_list_CV[[paste0("CVtrain_", i)]]
    m = ranger(
      formula = labels ~ .,
      data    = M,
      num.trees = selected_rows$num.trees[j+1],
      max.depth = selected_rows$max.depth[j+1],
      min.node.size =selected_rows$min.node.size[j+1],
      mtry = selected_rows$mtry[j+1],
      importance = "none",
      classification = T,
      probability = T
      
    )
    
    M= data_list_CV[[paste0("CVtest_", i)]]
    pred= predict(m,M)
    pred= ifelse(pred$predictions[,2] >(0.5-i*0.0066), 1, 0)
    
    
    fcv[i] <- f_meas_vec(
      truth= as.factor(M$labels),
      estimate= as.factor(pred),
      beta =0.5,
      event_level = "second")
    
  }
  
  sd[j+1] =sd(fcv)
  mean[j+1]=mean(fcv)

}

#view if the results are stable across the different validation
view(mean)
view(sd)

#Yes they are!

