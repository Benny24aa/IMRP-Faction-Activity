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


#### How many people are of a certain tier analysis
  
  Faction_Roster_Count <- Faction_Roster_Activity %>% 
    select(tier)%>% 
    group_by(tier)|>
    summarise(count=n(), .groups = 'drop') %>% 
    rename(total_members = count)
  
  
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

rmarkdown::render(
  "activity report.Rmd")

