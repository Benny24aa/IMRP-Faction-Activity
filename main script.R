library(readxl)
library(dplyr)
library(fs)
library(purrr)
library(data.table)
library(tidyr)
library(janitor)
library(stringr)
library(readr)
library(lubridate)

today_date <- Sys.Date()
over_month_zero_hour <- today_date %m-% months(1)
over_month_zero_hour = strptime(as.character(over_month_zero_hour), "%Y-%m-%d")
over_month_zero_hour = format(over_month_zero_hour,"%d-%m-%Y")



Faction_Roster <- read.csv("C:/Users/harle/OneDrive/Desktop/SAPD Files 2024/PD roster.csv") %>% 
  clean_names() 

#### Error Calculations
Faction_Roster_Activity <- Faction_Roster %>% 
 mutate(playtime_2_weeks = gsub(" hours", "", playtime_2_weeks)) %>% 
  mutate(playtime_2_weeks = as.numeric(playtime_2_weeks), playtime_2_weeks) %>% 
  mutate(
    Activity_Zero = playtime_2_weeks == 0,
    Activity_Under_20 = playtime_2_weeks > 0 & playtime_2_weeks <=20,
    Activity_Between_20_40 = playtime_2_weeks >20 & playtime_2_weeks <= 40,
    Activity_Between_40_80 = playtime_2_weeks >40 & playtime_2_weeks <= 80,
    Activity_above_80 = playtime_2_weeks > 80
  ) %>% 
  mutate(Activity_Type = case_when(
    Activity_Zero == TRUE ~ "Inactive",
    Activity_Under_20 == TRUE ~ "Improvement Required",
    Activity_Between_20_40 == TRUE ~ "Average",
    Activity_Between_40_80 == TRUE ~ "Above Average",
    Activity_above_80 == TRUE ~ "Get a life"
  )) %>% 
  select(-Activity_Zero, - Activity_Under_20, - Activity_Between_20_40, - Activity_Between_40_80, - Activity_above_80) %>% 
  mutate(name = gsub("<a0><a0>", "", name)) ### Cleans strange issue with naming 

  Faction_Roster_Activity$Activity_Type <- Faction_Roster_Activity$Activity_Type %>% 
    replace_na('Bug Row')
  
  #### Data cleaned of rows which comes from copying and pasting from ucp
  
  Faction_Roster_Activity <- Faction_Roster_Activity %>% 
    filter(Activity_Type != "Bug Row") %>% 
    filter(tier != "Tier")

  
  Faction_Roster_Activity_Tier_0_Count <- Faction_Roster_Activity %>% 
    filter(tier == "Tier 0") %>% 
    select(tier, Activity_Type) %>% 
    group_by(tier, Activity_Type)|>
    summarise(count=n(), .groups = 'drop') %>% 
    select(-tier) %>% 
    rename(`Activity Type` = Activity_Type ,`Number of Players` = count )
  
  Faction_Roster_Activity_Tier_1_Count <- Faction_Roster_Activity %>% 
    filter(tier == "Tier 1") %>% 
    select(tier, Activity_Type) %>% 
    group_by(tier, Activity_Type)|>
    summarise(count=n(), .groups = 'drop') %>% 
    select(-tier)%>% 
    rename(`Activity Type` = Activity_Type ,`Number of Players` = count )
  
  Faction_Roster_Activity_Tier_2_Count <- Faction_Roster_Activity %>% 
    filter(tier == "Tier 2") %>% 
    select(tier, Activity_Type) %>% 
    group_by(tier, Activity_Type)|>
    summarise(count=n(), .groups = 'drop') %>% 
    select(-tier)%>% 
    rename(`Activity Type` = Activity_Type ,`Number of Players` = count )
  
  Faction_Roster_Activity_Tier_3_Count <- Faction_Roster_Activity %>% 
    filter(tier == "Tier 3") %>% 
    select(tier, Activity_Type) %>% 
    group_by(tier, Activity_Type)|>
    summarise(count=n(), .groups = 'drop') %>% 
    select(-tier)%>% 
    rename(`Activity Type` = Activity_Type ,`Number of Players` = count )

#### How many people are of a certain tier analysis
  
  Faction_Roster_Count <- Faction_Roster_Activity %>% 
    select(tier)%>% 
    group_by(tier)|>
    summarise(count=n(), .groups = 'drop') %>% 
    rename(total_members = count) 
  
  Faction_Roster_List_Tier0 <- Faction_Roster_Activity %>% 
    select(name, tier, rank) %>% 
    filter(tier =='Tier 0') %>% 
    select(-tier) %>% 
    rename(Name = name, Rank = rank )
 
   Faction_Roster_List_Tier1 <- Faction_Roster_Activity %>% 
    select(name, tier, rank) %>% 
    filter(tier =='Tier 1') %>% 
     select(-tier)%>% 
     rename(Name = name, Rank = rank )
  
  
#### Zero Hour Report - Three Month Inactive

Faction_Zero_Hour_4_Weeks <- Faction_Roster_Activity %>% 
  filter(Activity_Type == "Inactive") %>% 
  group_by(tier)|>
  summarise(count=n(), .groups = 'drop') 
  
Faction_Zero_Hour_4_Weeks <- right_join(Faction_Roster_Count, Faction_Zero_Hour_4_Weeks, by = 'tier')

Faction_Zero_Hour_4_Weeks <- Faction_Zero_Hour_4_Weeks %>% 
  mutate(Percentage = count/total_members * 100)

#### Report by activity type

Faction_Roster_Activity_Breakdown <- Faction_Roster_Activity %>% 
  select(tier, Activity_Type) %>% 
  group_by(tier, Activity_Type) %>% 
  summarise(count=n(), .groups = 'drop') 
  
Faction_Roster_Activity_Breakdown <- right_join(Faction_Roster_Count, Faction_Roster_Activity_Breakdown, by = 'tier')%>% 
  mutate(Percentage = count/total_members * 100) %>% 
  select(-total_members)

Faction_Roster_Count_For_Table <- Faction_Roster_Activity %>% 
  select(tier)%>% 
  group_by(tier)|>
  summarise(count=n(), .groups = 'drop') %>% 
  rename(`Total Members` = count, `Tier` = tier)

#### Data Conditions
`Activity Types` <- c('Inactive', 'Improvement required', 'Average', 'Above Average', 'Get a Life')
`Description` <- c('Zero Hours', 'Greater than 0 but less/equal to 20', 'Greater than 20 but less/equal to 40', 'Greater than 40 but less/equal to 80', 'Above 80')
Condition_Table <- data.frame(`Activity Types`, `Description`)

PD_Salaries <- read.csv("C:/Users/harle/OneDrive/Desktop/SAPD Files 2024/IMRP-Faction-Activity/pays.csv")
PD_Salaries <- PD_Salaries %>% 
  mutate(Name = gsub(' ', '', Name)) %>% 
  mutate(Name = gsub("([[:lower:]])(?=[[:upper:]])", "\\1 ", Name, perl = TRUE), Name) %>% 
  mutate(Pay = as.numeric(Pay)) %>% 
  rename(name = Name) %>% 
  mutate(name = gsub('Mc ', 'Mc', name)) %>% 
  mutate(name = gsub('Samuelerrari', 'Samuel Ferrari', name)) %>% 
  mutate(name = gsub('Raphael Tin Toretto', 'Raphael TinToretto', name))

Pay_To_Roster_Info <- left_join(PD_Salaries, Faction_Roster_Activity, by = 'name' )

Pay_To_Roster_Info_Total <- Pay_To_Roster_Info %>% 
mutate(Total_Pay = Pay*playtime_2_weeks)

Pay_Leaderboard <- Pay_To_Roster_Info_Total %>% 
  select(name, Total_Pay)

Pay_Leaderboard <- Pay_Leaderboard[order(Pay_Leaderboard$Total_Pay, decreasing = TRUE),]

Pay_By_Tier_Group <- Pay_To_Roster_Info_Total %>% 
  select(tier, Total_Pay) %>% 
  group_by(tier) %>% 
  summarise(Total_Pay = sum(Total_Pay), .groups = 'drop')