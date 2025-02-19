#----------------------------------------------------------------------------------------------
# Extract Impact teams data from ona.io
#---------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(RCurl)
library(data.table)
library(readxl)

# GET DATA 
# =============================================================================
# rmr_raw <- getURL("https://api.ona.io/api/v1/data/479881?format=csv")
# rmr_raw <- read.csv(text = rmr_raw)
# rmr_raw <- IT_Reports
# dim(rmr_raw)

rmr_raw <- fread("data/IMPACTT-approach/raw/IMPACTT_Approach_Rapid_Meeting_Report.csv")
dependency_raw <- fread("data/IMPACTT-approach/raw/impact_team_dependency_level_ass.csv")

xls_choices <- read_excel("data/IMPACTT-approach/raw/IMPACTT_Approach_Rapid_Meeting_Report_V1_2024.xlsx",
                          sheet = "choices")

country <- xls_choices %>%
  filter(`list name` == "country") %>%
  select(c(1:3),  - `list name`) %>%
  distinct() %>%
  rename(country = label)

county <- xls_choices %>%
  filter(`list name` == "county_region") %>%
  select(c(1:3), - `list name`) %>%
  distinct() %>%
  rename(county = label)

impactt_teams <- xls_choices %>%
  filter(`list name` == "impact_team") %>%
  select(c(1:3),  - `list name`) %>%
  distinct()%>%
  rename(team = label)

rmr_raw <- rmr_raw %>%
  left_join(country, by = c(`meeting__details/country` = "name")) %>%
  left_join(county, by = c(`meeting__details/county_region` = "name")) %>%
  left_join(impactt_teams, by = c(`meeting__details/impact_team` = "name" ))

glimpse(rmr_raw)

fwrite(rmr_raw, "data/IMPACTT-approach/raw/IMPACTT_Approach_Rapid_Meeting_Report_Clean.csv")

#******************************************************************************
# DATA VISUALIZATION INDICATORS
#=============================================================================
rmr_raw$today <- as_date(rmr_raw$today)
min(rmr_raw$today)
max(rmr_raw$today)

rmr_raw$quarter <- format(rmr_raw$today, "%Y%m")

rmr_raw$action_items_implemented[rmr_raw$action_items_implemented=="n/a"]<- "999"

rmr_raw$action_items_implemented <- as.numeric(rmr_raw$action_items_implemented)

# calculate composite score
rmr_wide <- rmr_raw %>%
  mutate(agenda_complete = `preparations/agenda_complete`,
         RCA_conducted = `materials_data/RCA_conducted`,
         action_plan_documented = `materials_data/action_plan_documented`,
         action_items_implemented = `materials_data/action_items_implemented`
         ) %>%
  mutate(Month = today - as.POSIXlt(today)$mday + 1,
         agenda_complete = case_when(agenda_complete == 1 ~ 1, TRUE ~ 0) ,
         RCA_conducted = case_when(RCA_conducted == 1 ~ 1, TRUE ~ 0),
         action_plan_documented = case_when(action_plan_documented == 1 ~ 1, TRUE ~ 0),
         action_items_implemented = case_when(action_items_implemented == 1 ~ 1,
                                              action_items_implemented == 2 ~ 0,
                                              action_items_implemented == 3 ~ 0,
                                              TRUE ~ 0)
  ) %>%
  mutate(CompScore = agenda_complete + 
           RCA_conducted + action_plan_documented + action_items_implemented)


# write_excel_csv(rmr_raw, "IT Portal/data/rmr_raw.csv", na = "")



## Impact team processes
process_indicators <- rmr_wide %>%
  group_by(country, county, team) %>%
  summarise(agenda_complete = mean(agenda_complete, na.rm = TRUE),
            RCA_conducted = mean(RCA_conducted, na.rm = TRUE),
            action_plan_documented = mean(action_plan_documented, na.rm = TRUE),
            action_items_implemented = mean(action_items_implemented, na.rm = TRUE),
            compositeScore = mean(CompScore, na.rm = TRUE)
  )
glimpse(process_indicators)


# percent of meetings held - assuming monthly
meetings_count <- rmr_wide %>%
  group_by(country, county, team) %>%
  summarise(firstMeet = min(today),
            lastMeet = max(today),
            noHeld = n())%>%
  ungroup() %>%
  mutate(expectLastMeet = ymd((Sys.Date() - months(1)))) %>%
  mutate(noExpected = interval(firstMeet, expectLastMeet) %/% months(1),
         percMeetsHeld = ifelse(noHeld/ noExpected > 1, 1, noHeld/ noExpected)) %>%
  select(country, county, team, percMeetsHeld )
glimpse(meetings_count)

# combine the summary (composite score and meetings held)
overall_it_processes <- meetings_count %>%
  left_join(process_indicators)
glimpse(overall_it_processes)


##############################################
FinalITScores <- overall_it_processes %>%
  rowwise() %>%
  mutate(overall_Composite = sum(agenda_complete, RCA_conducted, 
                                 action_plan_documented, action_items_implemented,
                                 percMeetsHeld,
                                 na.rm=TRUE))
glimpse(FinalITScores)
####################################################



FinalITScores_pivot <- FinalITScores %>%
  pivot_longer(-c(country, county, team), names_to = "Attribute") %>%
  mutate(LongITprocess =  case_when(
    Attribute == "action_plan_documented" ~ "Action plan documented",
    Attribute == "agenda_complete" ~ "Agenda with all IT components",
    Attribute == "action_items_implemented" ~ "Action plan implemented",
    Attribute == "RCA_conducted" ~ "RCA Conducted",
    Attribute == "compositeScore" ~ "IT Process score",
    Attribute == "overall_Composite" ~ "Overall IT Process score",
    Attribute == "percMeetsHeld" ~ "Percent of meetings held",
    TRUE ~ NA_character_
  ),
  ScaledScore = case_when( Attribute == "compositeScore" ~ value/4,
                           Attribute == "overall_Composite" ~ value/5,
                           TRUE ~ value
  ),
  Category = case_when(ScaledScore <= 0.5 ~ "0-50%",
                       ScaledScore <= 0.75 ~ "51-75%",
                       ScaledScore <= 1 ~ "76-100%",
                       TRUE ~ NA_character_)
  )

glimpse(FinalITScores_pivot)

fwrite(FinalITScores_pivot, 
          "data/IMPACTT-approach/clean/ProcessIndicators_Clean.csv")

