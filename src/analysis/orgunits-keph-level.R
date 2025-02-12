# Organisational units
library(datimutils)
source("config/credentials_all.R")
loginToDATIM(username = username,
             password = password,
             base_url = base.url)



OrgUnitsGroupSets <- datimutils::getMetadata("organisationUnitGroupSets")
glimpse(OrgUnitsGroupSets)

OrgUnitsGroups <- datimutils::getMetadata("organisationUnitGroups")
glimpse(OrgUnitsGroups)

OrgUnitsGroups %>% filter(grepl("KEPH", name))



keph1 <- datimutils::getMetadata("organisationUnitGroups/axUnguN4QDh/organisationUnits", 
                                    fields = "id,level,parent, code,name") %>%
  mutate(
    Facility_Level = "KEPH Level 1",
    Facility_id = "axUnguN4QDh"
  )
glimpse(keph1)

keph2 <- datimutils::getMetadata("organisationUnitGroups/tvMxZ8aCVou/organisationUnits", 
                                 fields = "id,level,parent, code,name") %>%
  mutate(
    Facility_Level = "KEPH Level 2",
    Facility_id = "tvMxZ8aCVou"
  )
glimpse(keph2)

keph3 <- datimutils::getMetadata("organisationUnitGroups/wwiu1jyZOXO/organisationUnits", 
                                 fields = "id,level,parent, code,name") %>%
  mutate(
    Facility_Level = "KEPH Level 3",
    Facility_id = "wwiu1jyZOXO"
  )
glimpse(keph3)


keph4 <- datimutils::getMetadata("organisationUnitGroups/hBZ5DRto7iF/organisationUnits", 
                                 fields = "id,level,parent, code,name") %>%
  mutate(
    Facility_Level = "KEPH Level 4",
    Facility_id = "hBZ5DRto7iF"
  )
glimpse(keph4)


keph5 <- datimutils::getMetadata("organisationUnitGroups/d5QX71PY5t0/organisationUnits", 
                                 fields = "id,level,parent, code,name") %>%
  mutate(
    Facility_Level = "KEPH Level 5",
    Facility_id = "d5QX71PY5t0"
  )
glimpse(keph5)


keph6 <- datimutils::getMetadata("organisationUnitGroups/FpY8vg4gh46/organisationUnits", 
                                 fields = "id,level,parent, code,name") %>%
  mutate(
    Facility_Level = "KEPH Level 6",
    Facility_id = "FpY8vg4gh46"
  )
glimpse(keph6)




OrgUnits <- bind_rows(keph1, keph2, keph3, keph3, keph4, keph5)

glimpse(OrgUnits)


# level 6
level_6 <- OrgUnits %>%
  filter(level == 6) %>%
  transmute(
    community.id = id,
    community.parent.id = parent.id,
    community = name
  )
glimpse(level_6)

# level 5
level_5 <- OrgUnits %>%
  filter(level == 5) %>%
  transmute(
    facility.id = id,
    facility.parent.id = parent.id,
    facility = name
  )
glimpse(level_5)

# level 4
level_4 <- OrgUnits %>%
  filter(level == 4) %>%
  transmute(
    ward.id = id,
    ward.parent.id = parent.id,
    ward = name
  )
glimpse(level_4)

# level 4
level_3 <- OrgUnits %>%
  filter(level == 3) %>%
  transmute(
    subcounty.id = id,
    subcounty.parent.id = parent.id,
    subcounty = name
  )
glimpse(level_3)

# level 4
level_2 <- OrgUnits %>%
  filter(level == 2) %>%
  transmute(
    county.id = id,
    county.parent.id = parent.id,
    county = name
  )
glimpse(level_2)

# level 4
level_1 <- OrgUnits %>%
  filter(level == 1) %>%
  transmute(
    country.id = id,
    country.parent.id = parent.id,
    country = name
  )

add_facility <- OrgUnits %>%
  left_join(level_5, by = c("parent.id" = "facility.id")) %>%
  mutate(
    facility.parent.id = case_when(is.na(facility.parent.id) & level == 5 ~ parent.id,
                                   TRUE ~ facility.parent.id),
    facility = case_when(is.na(facility) & level == 5 ~ name,
                         TRUE ~ facility),
    
  )
add_facility |> 
  filter(is.na(facility)) |> 
  group_by(level) |> 
  tally()

add_ward <- add_facility %>%
  left_join(level_4, by = c("facility.parent.id" = "ward.id")) %>%
  mutate(
    ward.parent.id = case_when(is.na(ward.parent.id) & level == 4 ~ parent.id,
                               TRUE ~ ward.parent.id),
    ward = case_when(is.na(ward) & level == 4 ~ name,
                     TRUE ~ ward),
    
  )

add_ward |> 
  filter(is.na(ward)) |> 
  group_by(level) |> 
  tally()

add_subcounty <- add_ward %>%
  left_join(level_3, by = c("ward.parent.id" = "subcounty.id")) %>%
  mutate(
    subcounty.parent.id = case_when(is.na(subcounty.parent.id) & level == 3 ~ parent.id,
                                    TRUE ~ subcounty.parent.id),
    subcounty = case_when(is.na(subcounty) & level == 3 ~ name,
                          TRUE ~ subcounty),
    
  )
add_subcounty |> 
  filter(is.na(subcounty)) |> 
  group_by(level) |> 
  tally()


add_county <- add_subcounty %>%
  left_join(level_2, by = c("subcounty.parent.id" = "county.id")) %>%
  mutate(
    county.parent.id = case_when(is.na(county.parent.id) & level == 2 ~ parent.id,
                                 TRUE ~ county.parent.id),
    county = case_when(is.na(county) & level == 2 ~ name,
                       TRUE ~ county),
    
  )
add_county |> 
  filter(is.na(county)) |> 
  group_by(level) |> 
  tally()



clean_orgs <- add_county  |> 
  mutate(
    ward = str_replace_all(ward, " Ward", ""),
    subcounty = str_replace_all(subcounty, " Sub County", ""),
    county = str_replace_all(county, " County", "")
  )
glimpse(clean_orgs)


fwrite(clean_orgs, paste0(project_path,"metadata/orgunits.csv"))

dbWriteTable(con, "orgunits", clean_orgs, overwrite = TRUE, row.names = FALSE)
