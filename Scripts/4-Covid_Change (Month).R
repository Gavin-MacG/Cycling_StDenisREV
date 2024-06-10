# This script uses imputed sensor data and :
# - Removes imputed sensors that lack complete data for 2018 and 2019, and are not leisure sensors, reducing the dataset from 48 to 11 sensors.
# - Removes imputed sensors that are parallel to St-Denis REV
# - Uses the mean ridership of 2018 and 2019 as Pre-COVID reference period in order to reduce outlier bias
# - Calculates weighted percentage changes in overall monthly ridership due to COVID-19 
# - Plots the results
# - Exports the results as an excel table

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
Sensors_Month <- bind_rows(Sensor_List_Reduced) %>% 
  as.data.frame() %>%
  mutate(Year = year(Date), Month = month(Date, label = TRUE)) %>%
  filter(Year >= 2018) %>% 
  group_by(Name, Year, Month) %>%
  summarise(Pass = sum(Pass),.groups = 'drop') 

################################################################
# Calculate weighted monthly COVID Changes --------------------------------------------------
################################################################

calculate_changes <- function(df) {
  
  with_peel <- df %>%
    group_by(Year, Month) %>% 
    summarise(Pass = sum(Pass)) %>% 
    pivot_wider(names_from = Year, values_from = Pass) %>%
    mutate(PreCov = round((`2019` + `2018`) / 2),
           Change2020 = round(((`2020` - PreCov) / abs(PreCov)) * 100, 2),
           Change2021 = round(((`2021` - PreCov) / abs(PreCov)) * 100, 2),
           Change2022 = round(((`2022` - PreCov) / abs(PreCov)) * 100, 2)) %>%
    select(Month, Change2020, Change2021, Change2022)
  
  # Maisonneuve_Peel has incomplete data for 2023 :
  # it must be removed for this year
  no_peel <- df %>%
    filter(Name != "Maisonneuve_Peel") %>%
    group_by(Year, Month) %>% 
    summarise(Pass = sum(Pass)) %>% 
    pivot_wider(names_from = Year, values_from = Pass) %>%
    mutate(PreCov = round((`2019` + `2018`) / 2),
           Change2023 = round(((`2023` - PreCov) / abs(PreCov)) * 100, 2)) %>%
    select(Month, Change2023)
  
  result <- with_peel %>%
    left_join(no_peel, by = c("Month"))
  
  return(result)
}

Change_Covid_Month <- calculate_changes(Sensors_Month)


########################################################
# Plot the weighted Monthly Changes in ridership -------------------------------------------------
########################################################

# Re-pivot results for plot function
Monthly_Long <- Change_Covid_Month %>%
  pivot_longer(
    cols = starts_with("Change"),  
    names_to = "Year",  
    values_to = "Change",  
    names_prefix = "Change")

# plot results
ggplot(data = Monthly_Long, aes(x = Month, y = Change, group = Year, color = as.factor(Year))) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  scale_color_manual(values = c("2020" = "darkgreen",  
                                "2021" = "#76b947",  
                                "2022" = "#4da9b7",  
                                "2023" = "#2952a3")) +
  theme_minimal() +
  labs(title = "Monthly Change from pre-COVID period",
       x = "Month",
       y = "Change in ridership (%)",
       color = "Year")


ggsave(filename = "Outputs/Graphs/Change_COVID(Month).jpg", width = 6, height = 6)

########################################################
# Save outputs  -------------------------------------------------
########################################################

write.xlsx(Change_Covid_Month , file = "Outputs/Tables/Change_COVID(Month).xlsx", sheetName = "Sheet1", col.names = T, row.names = T, append = F)
save(Change_Covid_Month, file = "DataTreated/Change_Covid_Month.rda")
