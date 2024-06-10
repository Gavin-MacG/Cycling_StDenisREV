# This script uses imputed sensor data and :
# - Removes imputed sensors that lack complete data for 2018 and 2019, and are not leisure sensors, reducing the dataset from 48 to 11 sensors.
# - Removes imputed sensors that are parallel to St-Denis REV
# - Uses the mean ridership of 2018 and 2019 as Pre-COVID reference period in order to reduce outlier bias
# - Calculates weighted percentage changes in overall monthly ridership due to COVID-19 
# - Applies a shapiro-Wilk test to see if the Monthly changes are normally distributed

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

#################################################################
# Keep only Control sensors ------------------------------------------
#################################################################

# Keep only the sensors that follow these criteria 
# - Have full data for 2018 and 2019
# - Are not leisure cycling routes 
# - Are not parallel to St-Denis REV
# This removes 37 sensors, so the count changes from N=48 to N=11

keep_list <- c("Berri1", 
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

Sensor_List_Reduced <- Sensor_List_Final[names(Sensor_List_Final) %in% keep_list]

################################################################
# Combine sensor dataframes and summarize ridership by Month -------------------------------------------------------
################################################################

# Create Name column (Will be used to exclude sensor with partial data)
create_name <- function(df_list) {
  for (name in names(df_list)) {
    df_list[[name]]$Name <- name
  }
  return(df_list)
}

Sensor_List_Reduced <- create_name(Sensor_List_Reduced)

# Combine dfframes and summarize by Month
Sensors_Test <- bind_rows(Sensor_List_Reduced) %>% 
  as.data.frame() %>%
  mutate(Year = year(Date), Month = month(Date, label = TRUE)) %>%
  filter(Year >= 2018) %>% 
  group_by(Name, Year, Month) %>%
  summarise(Pass = sum(Pass),.groups = 'drop') 

################################################################
# Calculate COVID Changes --------------------------------------------------
################################################################

calculate_changes <- function(df) {
  
  with_peel <- df %>%
    pivot_wider(names_from = Year, values_from = Pass) %>%
    mutate(PreCov = round((`2019` + `2018`) / 2),
           Change2020 = round(((`2020` - PreCov) / abs(PreCov)) * 100, 2),
           Change2021 = round(((`2021` - PreCov) / abs(PreCov)) * 100, 2),
           Change2022 = round(((`2022` - PreCov) / abs(PreCov)) * 100, 2)) %>%
    select(Name, Month, Change2020, Change2021, Change2022)
  
  # Maisonneuve_Peel has incomplete data for 2023 : 
  # it must be removed for this year
  no_peel <- df %>%
    filter(Name != "Maisonneuve_Peel") %>%
    pivot_wider(names_from = Year, values_from = Pass) %>%
    mutate(PreCov = round((`2019` + `2018`) / 2),
           Change2023 = round(((`2023` - PreCov) / abs(PreCov)) * 100, 2)) %>%
    select(Name, Month, Change2023)
  
  result <- with_peel %>%
    left_join(no_peel, by = c("Name", "Month"))
  
  return(result)
}

result <- calculate_changes(Sensors_Test)

################################################################
# Apply Shapiro Wilk Test to verify normal distribution of Covid Changes --------------------------------------------------
################################################################

Normal_Distribution <- result %>%
  group_by(Month) %>%
  summarise(
    isNormal_2020 = shapiro.test(Change2020)$p.value > 0.05,
    isNormal_2021 = shapiro.test(Change2021)$p.value > 0.05,
    isNormal_2022 = shapiro.test(Change2022)$p.value > 0.05,
    isNormal_2023 = shapiro.test(Change2023)$p.value > 0.05,
    ShapiroWilk_2020 = round(shapiro.test(Change2020)$p.value,3),
    ShapiroWilk_2021 = round(shapiro.test(Change2021)$p.value,3),
    ShapiroWilk_2022 = round(shapiro.test(Change2022)$p.value,3),
    ShapiroWilk_2023 = round(shapiro.test(Change2023)$p.value,3)
  )

########################################################
# Save outputs  -------------------------------------------------
########################################################

write.xlsx(Normal_Distribution , file = "Outputs/Tables/Change_COVID (ShapiroWilk Test).xlsx", sheetName = "Sheet1")
