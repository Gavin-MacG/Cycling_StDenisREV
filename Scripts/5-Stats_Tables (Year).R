# This script uses yearly imputed sensor data and calculates :
# - Yearly Ridership statistics, per sensor
# -- Weekend vs Weekday
# -- High season (March 13th to November 7th, inclusive.) vs Low season (November 8th to March 12th, inclusive)
# - Yearly Percentage change due to COVID, per sensor and per group
# - Expected changes in ridership due to cOVID for parallel sensors, if rEV was not in place

#################################################################
# Install packages and load data ------------------------------------------
#################################################################

required_packages <- c("tidyverse", 
                       "openxlsx") 

for(Package in required_packages){
  if(!require(Package,character.only = TRUE)) { 
    install.packages(Package, dependencies=TRUE)
  }
  library(Package,character.only = TRUE)
}

load(file = "DataTreated/SENSORS_imp_df(day).rda")

################################################################
# Combine St-Denis REV sensors into clusters -------------------------------------------------------
################################################################

# Combine Northbound and Southbound St-Denis REV sensors into one observation
combine_and_sum <- function(df1, df2, Name) {
  combined_df <- bind_rows(df1, df2) %>%
    group_by(Date) %>%
    summarise(Pass = sum(Pass, na.rm = TRUE))%>%
    mutate(Longitude = first(df1$Longitude), 
           Latitude = first(df1$Latitude))
  
  return(combined_df)
}

# Combine REV north and south counts 
ClusterA_Rachel <- combine_and_sum(Sensor_List_Final$REV_StDenis_DuluthNB, Sensor_List_Final$REV_StDenis_RachelSB,"Cluster_A_Rachel")
ClusterB_Carrieres <- combine_and_sum(Sensor_List_Final$REV_StDenis_CarrieresNB, Sensor_List_Final$REV_StDenis_CarrieresSB,"Cluster_B_Carrieres") 
ClusterC_Castelnau <- combine_and_sum(Sensor_List_Final$REV_StDenis_CastelnauNB, Sensor_List_Final$REV_StDenis_CastelnauSB,"Cluster_C_Castelnau")
ClusterD_Sauve<- combine_and_sum(Sensor_List_Final$REV_Berri_SauveSB, Sensor_List_Final$REV_Lajeunesse_SauveNB,"Cluster_D_Sauve")

# Add new Cluster data frames to the list
Sensor_List_Final$ClusterA_Rachel <- ClusterA_Rachel
Sensor_List_Final$ClusterB_Carrieres <- ClusterB_Carrieres
Sensor_List_Final$ClusterC_Castelnau <- ClusterC_Castelnau
Sensor_List_Final$ClusterD_Sauve<- ClusterD_Sauve

#################################################################
# Keep only sensors with adequate quality data ------------------------------------------
#################################################################

# Reduces list to 21 sensors. Criteria for inclusion: 
# - At least 2018 to 2022 data
# - No data quality issues

# Sort these into groups: 
Control <- c("Berri1",
             "CoteSainteCatherine_Stuart",
             "Maisonneuve_Marcil",
             "Maisonneuve_Peel",
             "NotreDame",
             "Parc_Duluth",
             "Rachel_HoteldeVille",
             "Rachel_Papineau",
             "ReneLevesque_Wolfe",
             "University_Milton",
             "Viger_SaintUrbain")

Parallel <- c("Boyer_Everett",
              "Boyer_Rosemont",
              "Brebeuf_Rachel",
              "ChristopheColomb_Louvain",
              "SaintLaurent_Bellechasse",
              "SaintUrbain")

REV <- c("ClusterA_Rachel",
         "ClusterB_Carrieres",
         "ClusterC_Castelnau",
         "ClusterD_Sauve")

keep_list <- c(REV, Parallel, Control)

Sensor_List_Reduced <- Sensor_List_Final[names(Sensor_List_Final) %in% keep_list]

# Function to create Name and Group columns
create_name <- function(df_list) {
  for (name in names(df_list)) {
    df_list[[name]]$Name <- name
    if (name %in% Control) {
      df_list[[name]]$Group <- "Control"
    } else if (name %in% Parallel) {
      df_list[[name]]$Group <- "Parallel"
    } else if (name %in% REV) {
      df_list[[name]]$Group <- "REV"
    } else {
      df_list[[name]]$Group <- "Unknown"
    }
  }
  return(df_list)
}

Sensor_List_Reduced <- create_name(Sensor_List_Reduced)

###############################################################
# Calculate Overall statistics ------------------------------------------------------
###############################################################

Yearly_Stats <- bind_rows(Sensor_List_Reduced) %>% 
  as.data.frame() %>%
  mutate(Season = ifelse(
    (month(Date) >= 3 & month(Date) <= 11) &
      !(month(Date) == 3 & day(Date) < 13) &
      !(month(Date) == 11 & day(Date) > 7), "High", "Low"),
    Week = ifelse(wday(Date) %in% c(1, 7), "Wend", "Wday"),
    Year = year(Date)) %>%
  group_by(Name, Year) %>%
  summarise(
    Group = first(Group),
    Season = first(Season),
    Longitude = first(Longitude),
    Latitude = first(Latitude),
    Wday = sum(Pass[Week == "Wday"]),
    Wend = sum(Pass[Week == "Wend"]),
    Low = sum(Pass[Season == "Low"]),
    High = sum(Pass[Season == "High"]),
    Pass = sum(Pass)/1000,
    .groups = 'drop') %>% # prevent the creation of a grouped tibble
  mutate(Wend_Pct = round((Wend/(Wday+Wend))*100,2),
         Low_Pct = round((Low/(Low+High))*100,2),
         Change = if_else(Year == min(Year), 0, round(((Pass - lag(Pass)) / lag(Pass)) * 100, 2))) %>% 
  select(Year, Name, Wend_Pct, Low_Pct, Change, Pass, Wday, Wend, High, Low, Group, Longitude, Latitude) %>%
  filter(Year >= 2018)%>%
  arrange(Name, Year)

###############################################################
# Overall COVID change and ridership stats ------------------------------------------------------------
###############################################################

# Prepare calculations
Yearly_Stats_Pivot  <- Yearly_Stats  %>%
  select(Year, Name, Pass, Group) %>%
  pivot_wider(names_from = Year, values_from = Pass)%>%
  mutate(
    PreCov = round((`2018`+`2019`)/2,0),
    Covid2020 = round(((`2020`-PreCov)/ abs(PreCov))*100,1),
    Covid2021 = round(((`2021`-PreCov)/abs(PreCov))*100,1),
    Covid2022 = round(((`2022`-PreCov)/abs(PreCov))*100,1),
    Covid2023 = round(((`2023`-PreCov)/abs(PreCov))*100,1))%>%
  arrange(Group, Name)

# Ridership per sensor
Table1_Ridership <- Yearly_Stats_Pivot %>% 
  mutate(`2018`= round(`2018`),
         `2019`= round(`2019`),
         `2020`= round(`2020`),
         `2021`= round(`2021`),
         `2022`= round(`2022`),
         `2023`= round(`2023`)) %>% 
  select(Name, Group,`2018`, `2019`, `2020`, `2021`,`2022`, `2023`)

write.xlsx(Table1_Ridership, file = "Outputs/Tables/Table1_Ridership (Year).xlsx", sheetName = "Sheet1")

# Percent change due to COVID per sensor
Table2_CovidDrop <- Yearly_Stats_Pivot %>% 
  select(Name, Group, PreCov, Covid2020, Covid2021, Covid2022,Covid2023) %>% 
  mutate(`2020`= Covid2020,
         `2021`= Covid2021,
         `2022`= Covid2022,
         `2023`= Covid2023) %>% 
 select(Name, Group,PreCov,`2020`, `2021`,`2022`, `2023`) %>% 
 filter(Group != "REV")

###############################################################
# Weighted average drops by group ------------------------------------------------------------
###############################################################

# Summarise ridership across sensors for each group
Yearly_W_Change <- Yearly_Stats %>% 
  filter(Group != "REV")%>%
  group_by(Group, Year) %>%
  summarise(Pass = sum(Pass)) %>% 
  pivot_wider(names_from = Year, values_from = Pass)%>%
  mutate(PreCov = round((`2018` + `2019`) / 2, 0),
         `2020` = round(((`2020` - PreCov) / abs(PreCov)) * 100, 2),
         `2021` = round(((`2021` - PreCov) / abs(PreCov)) * 100, 2),
         `2022` = round(((`2022` - PreCov) / abs(PreCov)) * 100, 2))%>%
  select(Group, `2020`, `2021`, `2022`)

# Maisonneuve_Peel has missing datat for 2023 and must be excluded
Yearly_W_Change_2023 <- Yearly_Stats %>% 
  filter(Group != "REV",
         Name != "Maisonneuve_Peel")%>%
  group_by(Group, Year) %>%
  summarise(Pass = sum(Pass)) %>% 
  pivot_wider(names_from = Year, values_from = Pass)%>%
  mutate(PreCov = round((`2018` + `2019`) / 2, 0),
         `2023` = round(((`2023` - PreCov) / abs(PreCov)) * 100, 2))%>%
  select(Group,`2023`)

Yearly_W_Change <- full_join(Yearly_W_Change, Yearly_W_Change_2023)%>%
  ungroup()

# combine individual and group statistics of ridership change due to COVID
Table2_CovidDrop <- bind_rows(Table2_CovidDrop,Yearly_W_Change) %>% 
  arrange(Group)
  

write.xlsx(Table2_CovidDrop, file = "Outputs/Tables/Table2_CovidDrop (Year).xlsx", sheetName = "Sheet1")

#####################################################################
# Pearson correlation test to measure difference of changes between groups
######################################################################

Pearsontest <-Yearly_W_Change %>%
  select(Group, `2020`, `2021`, `2022`, `2023`) %>%
  column_to_rownames("Group") %>% 
  t() %>%                         
  as.data.frame()  

cor.test(Pearsontest$Control, Pearsontest$Parallel, method = 'pearson')

#####################################################################
# Calculate drops and lost per parallel sensor
######################################################################

# Using Control group yearly weighted average drops:
# Calculate what the change in ridership that would be expected of parallel sensors if REV was not in place
ExpectedDrops <-  Yearly_Stats_Pivot %>% 
  filter(Group == "Parallel")%>%
  select(Name, `2020`, `2021`, `2022`, `2023`, PreCov) %>% 
  mutate(Expected_2020 = PreCov*0.7132,
         Expected_2021 = PreCov*0.867,
         Expected_2022 = PreCov*0.9765,
         Expected_2023 = PreCov*1.0343)%>%
  mutate(Expected_2020 = Expected_2020-PreCov,
         Expected_2021 = Expected_2021-PreCov,
         Expected_2022 = Expected_2022-PreCov,
         Expected_2023 = Expected_2023-PreCov,
         Observed_2020 = `2020`-PreCov,
         Observed_2021 = `2021`-PreCov,
         Observed_2022 = `2022`-PreCov,
         Observed_2023 = `2023`-PreCov,)%>%
  select(Name, Observed_2020, Expected_2020, Observed_2021, Expected_2021, Observed_2022, Expected_2022,Observed_2023, Expected_2023)


write.xlsx(ExpectedDrops , file = "ExpectedDrops_ll.xlsx", sheetName = "Sheet1", col.names = T, row.names = T, append = F)
