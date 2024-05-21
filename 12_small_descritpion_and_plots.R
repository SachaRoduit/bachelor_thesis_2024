
#Small statistics
tail(na.omit(DF_startup$`DEAL DATE`))
head(na.omit(DF_startup$`DEAL DATE`))



unique(DF_Small$`EXIT TYPE`)




NAMatrix = cbind(lapply(lapply(DF_startup, is.na),sum))



view(DF_startup%>%
       group_by(STAGE)%>%
       summarise(n()))

data_list_des = list()
DF_Small = read_xlsx("DF_Small.xlsx")
#Preparation of the data without train and test data
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
  
  graph = graph_from_data_frame(edge_list, directed = F)
  
  eigenvector_centrality = eigen_centrality(graph)
  eigenvector_scores = eigenvector_centrality$vector
  
  
  DF_before_date$`PORTFOLIO COMPANY ID` <- as.character(DF_before_date$`PORTFOLIO COMPANY ID`)
  
  DF_before_date = merge(DF_before_date, data.frame(`PORTFOLIO COMPANY ID` = V(graph)$name, EigenvectorCentrality = eigenvector_scores),
                         by.x = "PORTFOLIO COMPANY ID", by.y = "PORTFOLIO.COMPANY.ID", all.x = TRUE)
  
  
  
  M = DF_before_date[, !grepl("^IINV", names(DF_startup))]
  M = na.omit(M)
  data_name = paste("M", i, sep = "_")
  data_list_des[[data_name]] = M
  
}


#Difference between 1st 5 year span to the last
length(data_list_des[["M_0"]]$`PORTFOLIO COMPANY ID`)
length(data_list_des[["M_16"]]$`PORTFOLIO COMPANY ID`)


#percentage of labels = "1"
percentage_1 = numeric(17)

for (i in 0:16) {
  M = data_list_des[[paste0("M_", i)]]
  num_of_1_labels= (sum(M$labels == 1)) / nrow(M) * 100
  percentage_1[i + 1] = num_of_1_labels
  
  
}

#Plot Figure 1
plot(percentage_1, type = "l", col = "blue", lwd = 2, xlab = "Years", ylab = "% of further deals", main = "Decreasing rate of further deals")
regression_model <- lm(percentage_1 ~ seq_along(percentage_1))
abline(regression_model, col = "red", lwd = 2)

#Regression (the slope is what determine the treshold by the models)
summary(regression_model)



#Create a plot with the percentage of labels = "1" and the montly difference to the datetest

datalistmonth = list()
for (i in 0:16){
  datetest = as.Date("2005-08-30") %m+% years(i)
  M = data_list_des[[paste0("M_", i)]]
  M = M %>% mutate(DIFF_MONTHS = round(as.numeric(difftime(datetest, `DEAL DATE`, units = "days")) / 30.4375, 0))
  
  
  datalistmonth[[paste0("M_", i)]] <- M
  
}


results_df <- data.frame()


for (i in 0:16) {
  M <- datalistmonth[[paste0("M_", i)]]
  
  percentages <- M %>%
    group_by(`DIFF_MONTHS`) %>%
    summarise(
      total = n(),
      percent_1 = sum(labels == 1) / total
    )
  
  percentages$Dataset <- factor(paste0("M_", i), levels = paste0("M_", 0:16))
  
  results_df <- rbind(results_df, percentages)
}

results_df %>%
  group_by(Dataset)%>%
  ggplot( aes(x = DIFF_MONTHS, y = percent_1)) +
  labs(y="% of further deals",x="Monthly difference from most recent deal ")+
  geom_point() + # add the points
  geom_smooth(method = "lm", se = FALSE)+
  theme_minimal()






