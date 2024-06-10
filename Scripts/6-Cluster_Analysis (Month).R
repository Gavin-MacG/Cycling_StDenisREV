# This script uses imputed sensor data and :
# - Clusters parallel and REV St-Denis sensors together
# - Doubles St-Urbain counts in order to estimate both north- and southbound ridership
# - Applies an algorithm that estimates:
# -- The amount of change in ridership to a parallel sensor expected by covid
# -- The amount of actual change in ridership in parallel sensors
# -- The amount of this change attributed to the REV stealing its ridership
# -- The amount of REV ridership that is due to pent up demand, as opposed to being stolen from parallel sensors
# - Plots the results
# - Exports the results as an excel table

#################################################################
# Install packages and load data ------------------------------------------
#################################################################

required_packages <- c("tidyverse", 
                       "openxlsx",
                       "ggpubr",    #ggarrange function for plots
                       "ggfortify", #ggplot extention for time series objects
                       "patchwork") # patchwork function for plots

for(Package in required_packages){
  if(!require(Package,character.only = TRUE)) { 
    install.packages(Package, dependencies=TRUE)
  }
  library(Package,character.only = TRUE)
}

load(file = "DataTreated/SENSORS_imp_df(day).rda")
load(file = "DataTreated/Change_Covid_Month.rda")

##########################################################################
# Prepare data  ---------------------------------------------------
##########################################################################

# Pivot Covid Change table to long format
Lost_Table <- Change_Covid_Month %>%
  pivot_longer(
    cols = starts_with("Change"),  
    names_to = "Year",  
    values_to = "Lost_Pct",  
    names_prefix = "Change") %>% 
  arrange(Year, Month)%>%
  mutate(Lost_Pct = 1 + (Lost_Pct/100),
         Month = as.character(Month))

# Change the ridership column name to the name of the sensor to facilitate sorting
# remove geodata
modify_dataframes <- function(df_list) {
  for (name in names(df_list)) {
    names(df_list[[name]])[2] <- name
    df_list[[name]] <- subset(df_list[[name]], select = -Longitude)
    df_list[[name]] <- subset(df_list[[name]], select = -Latitude)
  }
  return(df_list)
}
Sensor_List_Final <- modify_dataframes(Sensor_List_Final)


# Combine sensor dataframes and keep only post 2018
# Also: Double St-Urbain bike path numbers.
# - This path has only south-bound numbers, to get an estimate of northbound, it has been doubled. 
Sensors_df <- reduce(Sensor_List_Final, full_join, by = "Date")
Sensors <- Sensors_df %>% 
  filter(Date >= "2018-01-01") %>% 
  mutate(Year = as.character(year(Date)), Month = month(Date, label = TRUE)
         ,SaintUrbain = SaintUrbain*2) # double St-Urbain

##########################################################################
# Create clusters for each REV sensor group and parallel sensors---------------------------------------------------------
##########################################################################

ClusterA_Rachel <- Sensors %>%
  mutate(REV_NS = REV_StDenis_RachelSB + REV_StDenis_DuluthNB,
         ll_NS = Brebeuf_Rachel + SaintUrbain) %>%
  select(Year, Month, REV_NS, ll_NS)

ClusterB_Bellechasse <- Sensors %>%
  mutate(REV_NS = REV_StDenis_CarrieresSB + REV_StDenis_CarrieresNB,
         ll_NS = Boyer_Rosemont + SaintLaurent_Bellechasse) %>%
  select(Year, Month, REV_NS, ll_NS)

ClusterC_Castelnau <- Sensors %>%
  mutate(REV_NS = REV_StDenis_CastelnauSB + REV_StDenis_CastelnauNB,
         ll_NS = Boyer_Everett) %>%
  select(Year, Month, REV_NS, ll_NS)

ClusterD_Sauve <- Sensors %>%
  mutate(REV_NS = REV_Berri_SauveSB + REV_Lajeunesse_SauveNB,
         ll_NS = ChristopheColomb_Louvain) %>%
  select(Year, Month, REV_NS, ll_NS)



##################################################
# Calculate ridership ---------------------------------------------------------
#########################################################

cluster_names <- c("ClusterA_Rachel" ,
                   "ClusterB_Bellechasse",
                   "ClusterC_Castelnau",
                   "ClusterD_Sauve")


for (df_name in cluster_names) {
  df <- get(df_name)
  
  # Get the monthly average passages for each path
  df <- df %>%
    group_by(Year, Month) %>%
    summarise(REV = sum(REV_NS),
              ll = sum(ll_NS),
              .groups = 'drop') %>% 
    ungroup() %>% 
    select(Year, Month, REV, ll) %>%
    arrange(Year, Month)
  
  # Calculate monthly pre-covid mean of ll for 2018 and 2019
  means_2018_2019 <- df %>%
    filter(Year %in% c(2018, 2019)) %>%
    group_by(Month) %>%
    summarise(PreCov = round(mean(ll, na.rm = TRUE)), .groups = 'drop')
  
  # Combine the pre-covid mean to the main data
  df2 <- df %>%
    left_join(means_2018_2019, by = "Month") %>%
    distinct(Year, Month, .keep_all = TRUE)
  
  # Join the Lost Table
  df2 <- df2 %>%
    full_join(Lost_Table, by = c("Year", "Month"))
  
  # Calculate statistics
  df3 <- df2 %>%
    mutate(
      Drop = ll - PreCov,
      Lost = round(PreCov-(PreCov*Lost_Pct)),
      Displaced = pmax(0, if_else(Drop < 0, (abs(Drop) - Lost), 0)),
      #Captured = Displaced/5, 
      PentUp = REV - Displaced,
      #Generated = PentUp - Captured
    ) %>%
    select(
      Year,Month,PreCov,ll,Drop,Lost_Pct,Lost,Displaced,REV,PentUp
    ) %>%
    mutate(Month = factor(Month, 
                          ordered = TRUE, 
                          levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>% 
    arrange(Year, Month)#%>%
    #filter(!Year <= ("2019"))
  assign(df_name, df3, envir = .GlobalEnv)
}

##################################################################
# Create table of results (Yearly) ------------------------------------------------------------
##################################################################

yearly_table <- function(df) {
  df_name <- deparse(substitute(df))
  df %>%
    group_by(Year) %>%
    summarise(
      Displaced = sum(Displaced, na.rm = TRUE),
      PentUp = sum(PentUp, na.rm = TRUE),
      Total = sum(REV, na.rm = TRUE)
    ) %>%
    mutate(Name = df_name)
}

ClusterA_year<- yearly_table(ClusterA_Rachel)
ClusterB_year<- yearly_table(ClusterB_Bellechasse)
ClusterC_year<- yearly_table(ClusterC_Castelnau)
ClusterD_year<- yearly_table(ClusterD_Sauve)

ClusterAnalysisTable <- bind_rows(ClusterA_year, ClusterB_year, ClusterC_year, ClusterD_year) %>% 
  filter(Year != "2018", Year != "2019", Year != "2020") %>% 
  pivot_wider(names_from = Year, values_from = c(Total, Displaced, PentUp)) %>% 
  select(Name, Displaced_2021, PentUp_2021, Total_2021, Displaced_2022, PentUp_2022, Total_2022,Displaced_2023, PentUp_2023, Total_2023)

  
write.xlsx(ClusterAnalysisTable, file = "Outputs/Tables/Table4_ClusterAnalysis(Year).xlsx", sheetName = "Sheet1")
  
##################################################################
# Plot full results ------------------------------------------------------------
##################################################################

plot_results <- function(df) {
  dataframe_name <- deparse(substitute(df))
  df <- df %>% mutate(Date = make_date(year = Year, month = Month, day = 1))
  earliest_date <- min(df$Date[!is.na(df$REV_NS) | !is.na(df$ll_NS) | !is.na(df$Drop) | !is.na(df$PentUp)], na.rm = TRUE)
  ggplot_object <- ggplot(df, aes(x=Date)) + 
    geom_area(aes(y=REV, fill="Displaced"), alpha=0.5) +  
    geom_area(aes(y=PentUp, fill="PentUp"), alpha=0.5) + 
    geom_line(aes(y=ll, color="ll"), size=1) +  
    geom_line(aes(y=Drop, color="Drop"), size=1) +  
    geom_hline(yintercept = 0, color = "grey", size = 1) +
    labs(x=NULL, y=NULL, color=NULL, fill=NULL) +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
          axis.text.y = element_text(size = 8), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          plot.title = element_text(size = 10)) + 
    scale_y_continuous(labels = scales::label_number(), limits=c(-200000, 400000), breaks=seq(-200000, 400000, by=100000)) +
    scale_color_manual(values=c("ll" = "black", "Drop" = "red3"),  
                       labels=c("ll" = "Parallel", "Drop" = "Drop")) +  
    scale_fill_manual(values=c("Displaced" = "gold1", "PentUp" = "springgreen3"),  
                      labels=c("Displaced" = "Displaced", "PentUp" = "PentUp")) +  
    ggtitle(dataframe_name) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  return(ggplot_object)
}

plot_ClusterA <-plot_results(ClusterA_Rachel)
plot_ClusterB <-plot_results(ClusterB_Bellechasse)
plot_ClusterC <-plot_results(ClusterC_Castelnau)
plot_ClusterD <-plot_results(ClusterD_Sauve)

combined_plot <- (plot_ClusterA + plot_ClusterB) / 
  (plot_ClusterC + plot_ClusterD) +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

print(combined_plot)
# Save the combined plot to a file
ggsave("Outputs/Graphs/Cluster_Analysis(Full).jpg", combined_plot, width = 6, height = 6)

##################################################################
# Plot just REV -----------------------------------------------------------
##################################################################

plot_area <- function(df) {
  dataframe_name <- deparse(substitute(df))
  df <- df %>% mutate(Date = make_date(year = Year, month = Month, day = 1))
  earliest_date <- min(df$Date[!is.na(df$REV_NS) | !is.na(df$ll_NS) | !is.na(df$Drop) | !is.na(df$PentUp)], na.rm = TRUE)
  ggplot_object <- ggplot(df, aes(x=Date)) + 
    geom_area(aes(y=REV, fill="Displaced"), alpha=1) + 
    geom_area(aes(y=PentUp, fill="PentUp"), alpha=1) +  
    geom_line(aes(y=REV, color="REV"), size=0.5) +  
    geom_hline(yintercept = 0, color = "grey", size = 1) +
    labs(x="Date", y="Pass", color="Line Type", fill="Area Type") + 
    theme_minimal() +
    labs(x=NULL, y=NULL, color=NULL, fill=NULL) +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
          axis.text.y = element_text(size = 8), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          plot.title = element_text(size = 10)) + 
    scale_y_continuous(labels = scales::label_number(), limits=c(0, 250000), breaks=seq(0, 250000, by=100000)) +
    scale_fill_manual(values=c("Displaced" = "gold1", "PentUp" = "springgreen3"),
                      labels=c("Displaced", "PentUp")) +  
    scale_color_manual(values=c("REV" = "black"),  
                       labels=c("REV")) +
    scale_x_date(limits = as.Date(c("2020-06-01", NA)), date_breaks = "1 year", date_labels = "%Y") + # Show only years on x-axis
    ggtitle(dataframe_name) +
    theme(legend.title = element_blank())
  
  return(ggplot_object)
}

area_ClusterA <-plot_area(ClusterA_Rachel)
area_ClusterB <-plot_area(ClusterB_Bellechasse)
area_ClusterC <-plot_area(ClusterC_Castelnau)
area_ClusterD <-plot_area(ClusterD_Sauve)

combined_area <- (area_ClusterA + area_ClusterB) / 
  (area_ClusterC + area_ClusterD) +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

print(combined_area)

ggsave("Outputs/Graphs/Cluster_Analysis(Area).jpg", combined_area, width = 6, height = 6)
