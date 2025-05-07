#----------------------------------------------------------------------------------------------
# Extract Impact teams data from ona.io
#---------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(RCurl)
library(data.table)
library(readxl)
library(janitor)
library(air)

# GET DATA 
# =============================================================================
# rmr_raw <- getURL("https://api.ona.io/api/v1/data/479881?format=csv")
# rmr_raw <- read.csv(text = rmr_raw)
# rmr_raw <- IT_Reports
# dim(rmr_raw)

rmr_raw <- fread("data/IMPACTT-approach/raw/IMPACTT_Approach_Rapid_Meeting_Report.csv")

# dependency_raw <- fread("data/IMPACTT-approach/raw/impact_team_dependency_level_ass.csv")


### Dependency data
#==========================================================================================
dependency_one <- fread("data/IMPACTT-approach/raw/impact_team_dependency_level_ass.csv") %>%
  janitor::clean_names() %>%
  select(-(date_of_completion)) %>%
  mutate(
    created = as.Date(created),
    completed = as.Date(completed),
    changed = as.Date(changed),
  ) %>%
  mutate(
    year_created = year(created)
  )

tabyl(dependency_one$year_created)
dim(dependency_one)

dependency_two <- fread("data/IMPACTT-approach/raw/impact_team_dependency_level_assessment_April 2025.csv") %>%
  janitor::clean_names() %>%
  select(-(date_of_completion)) %>%
  mutate(
    created = as.Date(lubridate::mdy_hm(created)),
    completed = as.Date(lubridate::mdy_hm(completed)),
    changed = as.Date(lubridate::mdy_hm(changed))
  ) %>%
  mutate(
    year_created = year(created)
  )

tabyl(dependency_two$year_created)
dim(dependency_two)

dependency_all <- bind_rows(dependency_one, dependency_two)

dependency_all_min <- dependency_all %>%
  select(
    submission_id,
    created, completed, changed, year_created,
    submitted_by_title, submitted_to_entity_title,
    name_of_impact_team_sub_county_district_id, 
    name_of_impact_team_sub_county_district_title,
    name_s_of_person_s_completing_form,
    period_of_review_title, year_of_review_title,
    response : comments_if_any
  ) %>%
  # Convert to character
  mutate(section_score = as.character(section_score),
         section_score_2 = as.character(section_score_2),
         section_score_3 = as.character(section_score_3),
         overall_score = as.character(overall_score)
  ) 

dependency_all_min_unpivot <- dependency_all_min %>%
  pivot_longer(cols = c(response : comments_if_any),
               names_to = "Attribute",
               values_to = "Value")%>%
  # Dependency Score
  mutate(
    dependency_score = case_when(
      Value == "High dependency" ~ 3,
      Value == "Medium dependency" ~ 2,
      Value == "Low dependency" ~ 1,
      TRUE ~ NA_integer_
    )
  )

fwrite(dependency_all_min_unpivot, 
       "data/IMPACTT-approach/clean/dependency/Dependency_long.csv")


dependency_reference <- dependency_all_min_unpivot %>%
    distinct(Attribute) %>%
    mutate(Index = row_number()) %>%
    mutate(
    Score = case_when(
      Attribute == "dependency" ~ "Technical",
      Attribute == "dependency_2" ~ "Coordination",
      Attribute == "dependency_3" ~ "Financial",
      Attribute == "overall_dependency" ~ "Overall",
      TRUE ~ Attribute
    ),
    Class = case_when(
      grepl("response", Score) ~ "Response",
      grepl("comments", Score) ~ "Comments",
      grepl("score", Score) ~ "Section Score",
      TRUE ~ "Dependency"
    )
  )


dependency_stacked <- dependency_all_min_unpivot %>%
  group_by(t)

fwrite(dependency_reference, 
       "data/IMPACTT-approach/clean/Dependency/dependency_reference.csv")


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

rmr_clean <- rmr_raw %>%
  select(258:274, 1 : 257)

colnames(rmr_clean) <- sub(".*/", "", colnames(rmr_clean))

fwrite(rmr_clean, "data/IMPACTT-approach/raw/IMPACTT_Approach_Rapid_Meeting_Report_Clean.csv")

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



### Trend deep dive
#===============================================================================

# Impact team processes
process_indicators_trend <- rmr_wide %>%
  mutate(date_of_meeting = `meeting__details/date_of_meeting`) %>%
  group_by(country, county, team, date_of_meeting) %>%
  summarise(`Agenda Complete` = mean(agenda_complete, na.rm = TRUE),
            `RCA Conducted` = mean(RCA_conducted, na.rm = TRUE),
            `Action Plan Documented` = mean(action_plan_documented, na.rm = TRUE),
            `Action Items Implemented` = mean(action_items_implemented, na.rm = TRUE),
            `Composite Score` = mean(CompScore, na.rm = TRUE) / 4
  )
glimpse(process_indicators_trend)


fwrite(process_indicators_trend, 
       "data/IMPACTT-approach/clean/ProcessIndicators_Clean_Trend.csv")


# Impact team common implementation challenges
challenges_main <- rmr_wide %>%
  mutate(date_of_meeting = `meeting__details/date_of_meeting`) %>%
  select(country, county, team, date_of_meeting, `challenges_successes/challenges_encountered/1` : `challenges_successes/other_challenges`) %>%
  pivot_longer(
    cols = c(`challenges_successes/challenges_encountered/1` : `challenges_successes/other_challenges`)
  ) %>%
  mutate(
    name = str_replace(name, "challenges_successes/", "")
  ) %>%
  mutate(map_challenge = case_when(
    name == "challenges_encountered/1" ~ "Inadequate preparation for the meeting",
    name == "challenges_encountered/2" ~ "IT Process gaps",
    name == "challenges_encountered/3" ~ "Skills gap; connecting to virtual meeting",
    name == "challenges_encountered/4" ~ "Skills gap; data analysis, presentation/ITT utilization",
    name == "challenges_encountered/5" ~ "Skills gap; minutes taking",
    name == "challenges_encountered/6" ~ "Skills gap; review of supply chain indicators",
    name == "challenges_encountered/7" ~ "Skills gap; root cause analysis",
    name == "challenges_encountered/8" ~ "Time management/constraints",
    name == "challenges_encountered/9" ~ "Unavailability of data",
    name == "challenges_encountered/10" ~ "Unavailability of key members",
    name == "challenges_encountered/11" ~ "Other challenges",
    name == "challenges_encountered/12" ~ "Lack of a projector",
    name == "other_challenges_encountered" ~ "Other challenges",
    name == "challenges_encountered_comments" ~ "Comments",
    TRUE ~ NA_character_  
  )) 

fwrite(challenges_main, 
       "data/IMPACTT-approach/clean/challenges_main.csv")








