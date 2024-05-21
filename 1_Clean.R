#This is the first script to run it is processing the raw data recieved from prequin

#loading data
setwd("~/Bachelor_Arbeit/data")
#Main data
DF_startup = readRDS("DF_startup.RData")
#Investor data
DF_INV = readRDS("DF_INV.RData")

#dropping features
DF_startup = DF_startup %>%
  select(
    #too many NA
    -`FINANCIAL ADVISORS - SELLERS`,
    -`FINANCIAL ADVISORS - BUYERS`,
    -`LEGAL ADVISORS - BUYERS`,
    -`LEGAL ADVISORS - SELLERS`,
    -`BOUGHT FROM / SELLERS (FIRMS)`,
    -`BOARD REPRESENTATIVES`,
    -`ACQUIRED SHARE (%)`,
    -`PORTFOLIO COMPANY WEBSITE`,
    -`LEAD PARTNERS`,
    
    -`COMPANY REVENUE (CURR. MN)`,
    -`ENTRY REVENUE MULTIPLE`,
    -`EBITDA (CURR. MN)`,
    -`ENTRY EBITDA MULTIPLE`,
    
    #all information in `DEAL SIZE (USD MN)`, just * by the exchange rate
    -`DEAL SIZE (EUR MN)`,
    -`DEAL SIZE (CURR. MN)`,
    
    #specific to us
    -`INVESTOR STATE/ COUNTY`,
    -`PORTFOLIO COMPANY STATE/ COUNTY`,
  )


#Cleaning of startup's city name 

DF_startup = DF_startup %>%
  mutate(
    `PORTFOLIO COMPANY CITY` = str_to_lower(`PORTFOLIO COMPANY CITY`),   
    `PORTFOLIO COMPANY CITY` = str_trim(`PORTFOLIO COMPANY CITY`),
    `PORTFOLIO COMPANY CITY` = str_remove_all(`PORTFOLIO COMPANY CITY`, "[[:punct:]]"))

#length(unique(DF_startup$`PORTFOLIO COMPANY CITY`)) 5020
#length(unique(DF_startup$PORTFOLIO_COMPANY_CITY_CLEAN)) 4953




#Cleaning of investors city name 

DF_startup = DF_startup %>%
  mutate(
    `INVESTOR CITY` = str_to_lower(`INVESTOR CITY`),   
    `INVESTOR CITY` = str_trim(`INVESTOR CITY`),
    `INVESTOR CITY` = str_remove_all(`INVESTOR CITY`, "[[:punct:]]"))

#length(unique(DF_startup$`INVESTOR CITY`)) 78214
#length(unique(DF_startup$INVESTOR_CITY_CLEAN)) 78121


# Company name cleaning but the Company ID will be preffered for data manipulation

DF_startup = DF_startup %>%
  mutate(
    `PORTFOLIO COMPANY` = str_to_lower(`PORTFOLIO COMPANY`),   
    `PORTFOLIO COMPANY` = str_trim(`PORTFOLIO COMPANY`),
    `PORTFOLIO COMPANY` = str_remove_all(`PORTFOLIO COMPANY`, "[[:punct:]]"))



#Writing the date as.date

DF_startup$`DEAL DATE`= as.Date(DF_startup$`DEAL DATE`)

#Creating a columns month and year

DF_startup = DF_startup %>%
  mutate(month = format(`DEAL DATE`, "%m"), year = format(`DEAL DATE`, "%Y"))

#Only completed deals are kept

DF_startup = DF_startup %>%
  filter(year >= "2000" )
DF_startup = DF_startup %>%
  filter(`DEAL STATUS` == "Completed" )

#As we will deal only with data from 2000 to 2023 before,
#Startup with funding year before 1995 are dropped

DF_startup = DF_startup %>%
  filter(`YEAR ESTABLISHED` >= "1995" )


#Deal size are replaced by the mean of all deals size according to stage


DF_startup <- DF_startup %>%
  group_by(STAGE) %>%
  mutate(mean_deal_size = mean(`DEAL SIZE (USD MN)`, na.rm = TRUE)) %>%
  ungroup()


#to see the effect without this imputation you can skip this part
#In each model before splitting the data into test and trainset its omit all NA value

DF_startup$`DEAL SIZE (USD MN)` <- ifelse(is.na(DF_startup$`DEAL SIZE (USD MN)`),
                                          DF_startup$mean_deal_size,
                                          DF_startup$`DEAL SIZE (USD MN)`)

#Run this code to see strong differencies between the stage
####
#DF_startup%>%
#  group_by(STAGE) %>%
#  summarise(m=mean(mean_deal_size),s=sd(mean_deal_size))
####





#Drop the help variable
DF_startup %<>%
  select(-mean_deal_size)





#Creating new variables
# last_deal_days: Days since the last deal

DF_startup <- DF_startup %>%
  arrange(`PORTFOLIO COMPANY ID`, `DEAL DATE`) %>%
  group_by(`PORTFOLIO COMPANY ID`) %>%
  mutate(last_deal_days = c(0, diff(`DEAL DATE`))) %>%
  ungroup()

#since_first_deal: Days since the first deal

DF_startup <- DF_startup %>%
  arrange(`PORTFOLIO COMPANY ID`, `DEAL DATE`) %>%
  group_by(`PORTFOLIO COMPANY ID`) %>%
  mutate(since_first_deal = cumsum(c(0, diff(`DEAL DATE`)))) %>%
  ungroup()

#amount_recieved: Total money already recieved by the startup

DF_startup <- DF_startup %>%
  arrange(`PORTFOLIO COMPANY ID`, `DEAL DATE`) %>%
  group_by(`PORTFOLIO COMPANY ID`) %>%
  mutate(amount_recieved = cumsum(`DEAL SIZE (USD MN)` )) %>%
  ungroup()

#Startup with already 1000 are dropped 
DF_startup = DF_startup %>%
  filter(amount_recieved <= 1000.000 )


#foreign_inv: Startup that recieved a foreign investement on the last deals
DF_startup[c("invcountry", "invcountry2" )] = str_split_fixed(DF_startup$`INVESTOR COUNTRY`, ',', 2)

DF_startup= DF_startup %>% 
  mutate(foreign_inv = ifelse(invcountry2 != "" | invcountry != `PORTFOLIO COMPANY COUNTRY`,1, 0))

#uninv: startup that recieved investement from unspecidfied investor (mentioned in the deal description or have an NA as investor)

DF_startup = DF_startup %>%
  mutate(uninv = case_when(
    str_detect(`DEAL DESCRIPTION`, "unspecified investor")|
      is.na(`INVESTORS / BUYERS (FIRMS)`) ==T ~ 1,
    TRUE ~ 0
  ))
DF_startup$`INVESTORS / BUYERS (FIRMS)` = tidyr::replace_na(DF_startup$`INVESTORS / BUYERS (FIRMS)`,"unspecified investor")





#From Investor data, seperating the investor name when it has , within

V = DF_INV %>%
  select(`INVESTORS / BUYERS (FIRMS)`)%>%
  filter(grepl(",",DF_INV$`INVESTORS / BUYERS (FIRMS)`))

V[c("Company","after ,","with two ,", "with 3 ,")] = str_split_fixed(V$`INVESTORS / BUYERS (FIRMS)`, ',', 4)

#tested no company name has more , than 3

l_1 = unique(V$`after ,`)
l_2 = unique(V$`with two ,`)
l_3 = unique(V$`with 3 ,`)


l = c(l_1 ,l_2, l_3)

#In main data, separating the Investor columns, which contains different Investors 
#separated by a ,(no more than 75)

DF_startup[c("IINV1", "IINV2","IINV3", "IINV4","IINV5", 
             "IINV6","IINV7", "IINV8", "IINV9","IINV10",
             "IINV11","IINV12", "IINV13", "IINV14","IINV15",
             "IINV16","IINV17", "IINV18", "IINV19","IINV20",
             "IINV21","IINV22", "IINV23", "IINV24","IINV25",
             "IINV26","IINV27", "IINV28", "IINV29","IINV30",
             "IINV31","IINV32", "IINV33", "IINV34","IINV35",
             "IINV36","IINV37", "IINV38", "IINV39","IINV40",
             "IINV41","IINV42", "IINV43", "IINV44","IINV45",
             "IINV46","IINV47", "IINV48", "IINV49","IINV50",
             "IINV51","IINV52", "IINV53", "IINV54","IINV55",
             "IINV56","IINV57", "IINV58", "IINV59","IINV60",
             "IINV61","IINV62", "IINV63", "IINV64","IINV65",
             "IINV66","IINV67", "IINV68", "IINV69","IINV70",
             "IINV71","IINV72", "IINV73", "IINV74","IINV75")] = str_split_fixed(DF_startup$`INVESTORS / BUYERS (FIRMS)`, ',', 75)

#Dropping if it is not a new investor but a part of the same investor's name

DF_startup = DF_startup %>%
  mutate(across(starts_with('IINV'), ~ifelse( .x %in% l,NA,.x
  )))

#n_inv: number of investor for a specific deal

DF_startup = DF_startup %>%
  mutate(n_inv = 75-rowSums(is.na(select(., starts_with('IINV')))))



#Group_city_portfolio: City of the startup, classified in 4 category more 10000,1000,100 and less than 100 startup in the cities

summary_data= DF_startup %>% 
  group_by(`PORTFOLIO COMPANY CITY`) %>%
  summarise(num= n())%>%
  mutate( Group_city_portfolio = case_when(num>=10000 ~"very_active",
                                           num>=1000 ~"active",
                                           num>=100 ~"low_active",
                                           num<100 ~"very_low_active",))%>%
  select(-num)



DF_startup = DF_startup %>%
  left_join(summary_data, by = "PORTFOLIO COMPANY CITY")





#USD_currency: If the money is in USD or not 

summary_data= DF_startup %>% 
  group_by(`DEAL CURRENCY`) %>%
  summarise(num= n())%>%
  mutate( USD_currency = case_when(num>=100000 ~1,
                                   num< 100000 ~0,))%>%
  select(-num)

#Example of a help boxplot that help in the decision
hist(summary_data$USD_currency)


DF_startup = DF_startup %>%
  left_join(summary_data, by = "DEAL CURRENCY")
hist(DF_startup$USD_currency)




#Group_country_portfolio: Country of the portfolio company, classified in 4 categories: USA, China, Active countries and Low activity countries.

summary_data= DF_startup %>% 
  group_by(`PORTFOLIO COMPANY COUNTRY`) %>%
  summarise(num= n())%>%
  mutate( Group_country_portfolio = case_when(num>=100000 ~"USA",
                                              num>=60000 ~"CHINA",
                                              num>=1000 ~"Active",
                                              num<1000 ~"low_activity",))%>%
  select(-num)


DF_startup = DF_startup %>%
  left_join(summary_data, by = "PORTFOLIO COMPANY COUNTRY")



#Year_since_funding: Number of years since the fundings of the startup
DF_startup = DF_startup %>%
  mutate(Year_since_funding = as.numeric(DF_startup$year) - as.numeric(DF_startup$`YEAR ESTABLISHED`) )





#Drop the last unused variables, Text analysis of deal description would be possible in further research

DF_startup = DF_startup%>%
  select(-month,-year,-`YEAR ESTABLISHED`,-`DEAL STATUS`,-`DEAL ID`,
         -`DEAL DESCRIPTION`,-`DEAL CURRENCY`,
         -`PORTFOLIO COMPANY`,-BACKGROUND,-invcountry,-invcountry2,-`INVESTOR CITY`,
         -`INVESTORS / BUYERS (FIRMS)`,-`INVESTOR COUNTRY`,-`INVESTORS / BUYERS (FUNDS)`,
         -`INVESTOR REGION`,-`SUB-INDUSTRIES`,-`INDUSTRY VERTICALS`,-`PORTFOLIO COMPANY CITY`,-`PORTFOLIO COMPANY COUNTRY`)




rm(summary_data,V,l_1,l_2,l_3)





