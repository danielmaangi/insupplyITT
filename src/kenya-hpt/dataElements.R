library(datimutils)

username<-"Maangi"
password<-"kiMaNi:1991"
base.url<-"https://hiskenya.org/"

login <- function(username,password,base.url) {
  url<-paste0(base.url,"api/me")
  r<-GET(url,authenticate(username,password))
  
  if(r$status == 200L) { print("Logged in successfully!")} 
  else {print("Could not login")}
}
login(username,password,base.url)

datimutils::loginToDATIM(
  username = username,
  password = password,
  base_url = base.url
)

HPTdataElements <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:ppdqEN91qZs", 
                                      fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  filter(valueType != "DATE") |>
  mutate(
    name = str_replace(name, "’’", "''"),
    name = str_replace(name, "’", "'"),
    name = str_replace(name, "½", "1/2"),
    
    shortName = str_replace(shortName, "’’", "''"),
    shortName = str_replace(shortName,  "’", "'"),
    shortName = str_replace(shortName, "½", "1/2"),
    
    ShortDisplayName = str_replace(name, "MOH 647_", "")
  ) |>
  transmute(
    id = id,
    displayName = name,
    Programme = "HPT",
    Commodity = shortName,
    ShortDisplayName = ShortDisplayName
  ) 

write_csv(HPTdataElements,
          "G:/.shortcut-targets-by-id/1-I-AM6fIfll60idw-jnzME08IVKzNdfj/MLE_DATA/Dashboards/Production/HPT/dataModel/DHIS2/hiskenya/download/HPT_DEs.csv")
