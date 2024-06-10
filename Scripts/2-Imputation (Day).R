# This script uses the consolidated sensor data and :
# - Splits each sensor into its own dataframe,
# - Organizes these dataframes into a list,
# - Summarizes all the data by day (time data is lost),
# - Plots the daily sensor data and saves it to the Graphs folder
# - Sets the start of each series as january first of the first complete year
# - Removes problematic sensors with bad data quality ()
# - Transforms abnormal data and outliers to NA
# - Imputes NA values (seasonal split imputation, by week and month)  
# - Plots the imputed daily sensor data and saves it to the Graphs folder
# - Saves the final imputed data as a dataframe list and as Time Series data

# These operations reduce sensor count from N=62 to N=48

#################################################################
# Install packages and load data ------------------------------------------
#################################################################

required_packages <- c("tidyverse", 
                       "imputeTS", # na_seasplit imputation functions
                       "forecast") #msts function to create time series object
                      
for(Package in required_packages){
  if(!require(Package,character.only = TRUE)) { 
    install.packages(Package, dependencies=TRUE)
  }
  library(Package,character.only = TRUE)
}

load(file = "DataTreated/SENSORS.rda")

#################################################################
# Split sensors, summarize observations by day and convert all 0 to NA----------------------------
#################################################################

Sensor_Split <- split(SENSORS, SENSORS$Name)

summarize_by_day <- function(df) {
  df %>% 
    group_by(Date) %>%
    summarize(Pass = sum(Pass, na.rm = TRUE)) %>%
    complete(Date = seq(min(Date), max(Date), by = "day")) %>% # complete missing observations
    mutate(Pass = ifelse(Pass == 0, NA, Pass)) %>% # convert 0 to NA
    arrange(Date) %>%
    as.data.frame()
}

Sensor_List <- map(Sensor_Split, summarize_by_day)

#################################################################
# Visual inspection : plot the raw data  --------------------------------------------------------
#################################################################

plot_pass <- function(df, df_name) {
  p <- ggplot(df, aes(x = Date, y = Pass)) +
    geom_line() +
    labs(title = df_name, x = "Date", y = "Pass") +
    theme_minimal() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # Year label at the beginning of every year
    theme(plot.title = element_text(face = "bold", size = 20),
          axis.text.x = element_text(face = "bold", size = 15),
          axis.text.y = element_text(face = "bold", size = 11))
  file_path <- paste0("Outputs/Graphs/Raw/Raw_", df_name, ".jpg")
  ggsave(file_path, plot = p, device = "jpg", width = 17, height = 6)
  return(p)
}

df_names <- names(Sensor_List)
plots <- list()

for (i in seq_along(Sensor_List)) {
  df <- Sensor_List[[i]]
  df_name <- df_names[i]
  plot <- plot_pass(df, df_name)
  print(plot)
}

#################################################################
# Clean data  : remove problematic sensors (14) -----------------------
#################################################################

# Remove sensors with too many data quality issues (4): 
quality_issues <- c("MetroLaurier",
                    "Gouin_Lajeunesse",
                    "SaintAntoine",
                    "Remembrance")

Sensor_List <- Sensor_List[!names(Sensor_List) %in% quality_issues]

# Remove sensors with insufficient data to run imputation(10): 
# at least two non-NA data points required per target day
# none of the following sensors have data for pre-covid periods
insufficient_data_issues <- c("BordduLacversest", 
                              "BordduLacversouest",
                              "Maisonneuve_Berri",
                              "Maisonneuve_Papineau",
                              "Maisonneuve_Plessis",
                              "McGill_William",
                              "Querbes_StRoch",
                              "REV_4Peel_NotreDame",
                              "Villeray",
                              "Wellington_Charlevoix") 

Sensor_List <- Sensor_List[!names(Sensor_List) %in% insufficient_data_issues]

#################################################################
# Clean data  : remove abnormalities -----------------------
#################################################################

# Remove data preceding 2016 for PontJacquesCartier
Sensor_List[["PontJacquesCartier"]] <- Sensor_List[["PontJacquesCartier"]][Sensor_List[["PontJacquesCartier"]]$Date >= as.Date("2016-01-01"), ]

# Define time periods with abnormal data
abnormal_data <- list(
  CoteSainteCatherine_Stuart = c("2016-08-29", "2016-10-06"),
  ChristopheColomb_Louvain   = c("2023-06-22", "2023-12-31"),
  Maisonneuve_Peel           = c("2015-08-25", "2015-11-09"),
  NotreDame                  = c("2020-11-20", "2021-04-19"),
  ReneLevesque_Wolfe    = list(c("2018-06-09", "2019-07-23"), 
                               c("2022-05-04", "2022-06-14")),
  Valois_laFontaine          = c("2022-01-01", "2023-01-01"),
  Viger_SaintUrbain          = c("2022-07-05", "2023-10-16")
)

# Function to set all data within abnormal ranges to NA
set_abnormal_to_na <- function(sensor_name, abnormal_ranges) {
  sensor_data <- Sensor_List[[sensor_name]]
  for (range in abnormal_ranges) {
    sensor_data$Pass[sensor_data$Date >= as.Date(range[1]) & sensor_data$Date <= as.Date(range[2])] <- NA
  }
  Sensor_List[[sensor_name]] <<- sensor_data
}

# Apply abnormal to NA function
lapply(names(abnormal_data), function(name) {
  ranges <- if (is.list(abnormal_data[[name]])) abnormal_data[[name]] else list(abnormal_data[[name]])
  set_abnormal_to_na(name, ranges)
})

#################################################################
# Clean data  : remove outliers -----------------------
#################################################################

# Set thresholds for outliers
thresholds <- list(
  ChristopheColomb_Louvain = 2500,
  CamillienHoude1 = 3000,
  Estacade = 500,
  SaintUrbain = 4000,
  SaintLaurent_Bellechasse = 13000,
  REV_Bellechasse_13eme = 4000,
  ReneLevesque_Wolfe = 6000,
  Rachel3_Angus = 5500,
  Rachel_Papineau = 9000,
  Rachel_HoteldeVille = 6000,
  PontLeGardeur = 1500,
  PontJacquesCartier = 6000,
  Parc_Duluth = 6000
)

# Set all data points above outlier thresholds to NA
for (sensor in names(thresholds)) {
  Sensor_List[[sensor]]$Pass[Sensor_List[[sensor]]$Pass >= thresholds[[sensor]]] <- NA
}

#################################################################
# Clean Data : Switch NA for 0 on low count sensors to allow for imputation to have two non-NA points--------------------------------------------------------
#################################################################

# Define range of data to switch Na to 0
date_ranges <- list(
  A25_Gouin = list(c("2021-10-01", "2023-12-31")),
  Estacade = list(c("2019-08-01", "2023-06-16")),
  MauriceDuplessis = list(c("2020-10-01", "2021-04-01"),c("2021-10-01", "2022-04-01")),
  PisteDesCarrieres = list(c("2019-10-01", "2020-04-01"),c("2020-10-01", "2021-04-01"),c("2021-10-01", "2022-04-01")),
  ParcStanley = list(c("2018-10-01", "2019-04-01"),c("2019-10-01", "2020-04-01"))
)

# Function to switch Na to 0
update_na_pass <- function(sensor_data, date_ranges) {
  for (date_range in date_ranges) {
    start_date <- as.Date(date_range[1])
    end_date <- as.Date(date_range[2])
    sensor_data$Pass[
      sensor_data$Date >= start_date & 
        sensor_data$Date <= end_date & 
        is.na(sensor_data$Pass)
    ] <- 0
  }
  return(sensor_data)
}

# Apply Na to 0 function
for (sensor_name in names(date_ranges)) {
  Sensor_List[[sensor_name]] <- update_na_pass(Sensor_List[[sensor_name]], date_ranges[[sensor_name]])
}

#################################################################
# Set start dates -----------------------------------------------------------
#################################################################

# If start day of the series is not the first of the month: 
# add all dates till the first of the month as NA values
for (df_name in names(Sensor_List)) {
  df <- Sensor_List[[df_name]]
  if (day(df$Date[1]) != 1) { 
    start_date <- as.Date(format(df$Date[1], "%Y-%m-01"))# Uses first entry as a reference for month and year 
    end_date <- df$Date[1] - 1 # Day before the first entry
    missing_dates <- seq.Date(start_date, end_date, by = "day")
    missing_df <- data.frame(Date = missing_dates, Pass = NA)
    Sensor_List[[df_name]] <- rbind(missing_df, df) %>%
      arrange(Date)
  }
}

# If start month of the series is not january:
# cut out any start year that is incomplete
adjust_start_to_january <- function(df) {
  start_year <- year(df$Date[1])
  start_month <- month(df$Date[1])
  if (start_month != 1) {
    df <- df %>% filter(year(Date) > start_year)
  }
  return(df)
}

# Apply the start year cutoff function to each sensor in the list
Sensor_List <- map(Sensor_List, adjust_start_to_january)

#################################################################
# Imputation : test efficiency --------------------------------------------------------------
#################################################################

# Extract test dataframe, and create a test version with 2016 data cut out
reference <- Sensor_List[["Berri1"]]
test <- reference %>%
  mutate(Pass = if_else(year(Date) == 2016, NA_real_, Pass))

start_year <- year(test$Date[1])
start_day_of_year <- yday(test$Date[1])

# Convert to Multiple Time Series object
test_msts <- msts(test$Pass,
                  start = c(start_year, start_day_of_year),
                  seasonal.periods =c(7,365.25))
reference_msts <- msts(reference$Pass,
                  start = c(start_year, start_day_of_year),
                  seasonal.periods =c(7,365.25))

# Apply interpolation
test_interpolation <- na_seasplit(test_msts)
reference_msts <- na_seasplit(reference_msts)

# Calculate RMSE
differences <- test_interpolation - reference_msts
sqrt(mean(differences^2))


#################################################################
# Imputation : apply to list of dataframes --------------------------------------------------------------
#################################################################

# Convert to Multiple Time Series objects and impute missing values
create_msts_and_impute <- function(df, df_name) {
  start_year <- year(df$Date[1])
  start_day_of_year <- yday(df$Date[1])
  # create time series object
  df_msts <- msts(df$Pass, start = c(start_year, start_day_of_year), seasonal.periods =c(7,365.25))
  # imputation
  if (any(is.na(df_msts))) {
    df_msts_imp <- na_seasplit(df_msts) 
    plot_title <- df_name
    plot <- ggplot_na_imputations(df_msts, df_msts_imp, title = plot_title) 
    ggsave(paste0("Outputs/Graphs/Imputed/Imp_", df_name, ".jpg"), plot, device = "jpg", width = 17, height = 9)
    assign(paste0("msts_", df_name), df_msts_imp, envir = .GlobalEnv)# Save imputed time series object in global environment
  } else {
    assign(paste0("msts_", df_name), df_msts, envir = .GlobalEnv) # Save original time series object in global environment if no imputation needed
  }
}

# Apply Imputation and plotting function
names_list <- names(Sensor_List)

for (i in seq_along(Sensor_List)) {
  create_msts_and_impute(Sensor_List[[i]], names_list[i])
}

#################################################################
# Imputation : adjustments for sensors with missing 2019 data--------------------------------------------------------------
#################################################################

# Manual adjustments for Brebeuf_Rachel (no data for 2019, means imputation is underestimating this year)
Brebeuf_Rachel_df <- Sensor_List[["Brebeuf_Rachel"]]
Brebeuf_Rachel_before <- Brebeuf_Rachel_df %>% filter(Date < "2020-04-01")
Brebeuf_Rachel_after <- Brebeuf_Rachel_df  %>% filter(Date >= "2020-04-01")

start_year_a <- year(Brebeuf_Rachel_before$Date[1])
start_day_of_year_a <- yday(Brebeuf_Rachel_before$Date[1])
msts <- msts(Brebeuf_Rachel_before$Pass, start = c(start_year_a, start_day_of_year_a), seasonal.periods =c(7,365.25))
msts_before <- na_seasplit(msts) 
ggplot_na_imputations(msts, msts_before, title = "Brebeuf_Rachel_before") 
ggsave(filename = "Outputs/Graphs/Imputed/Imp_Brebeuf_Rachel_before.jpg", width = 17, height = 9, device = "jpeg")

start_year <- year(Brebeuf_Rachel_after$Date[1])
start_day_of_year <- yday(Brebeuf_Rachel_after$Date[1])
msts_after <- msts(Brebeuf_Rachel_after$Pass, start = c(start_year, start_day_of_year), seasonal.periods =c(7,365.25))

msts_join <- msts(c(msts_before, msts_after), start = c(start_year_a, start_day_of_year_a), seasonal.periods =c(7,365.25))
msts_Brebeuf_Rachel <- na_seasplit(msts_join) 
ggplot_na_imputations(msts_join, msts_Brebeuf_Rachel, title = "Brebeuf_Rachel_after") 
ggsave(filename = "Outputs/Graphs/Imputed/Imp_Brebeuf_Rachel_after.jpg", width = 17, height = 9, device = "jpeg")


# Manual adjustments for University_Milton (partial data for 2019, means imputation is underestimating this year)
University_Milton_df <- Sensor_List[["University_Milton"]]
University_Milton_before <- University_Milton_df %>% filter(Date < "2020-01-01")
University_Milton_after <- University_Milton_df  %>% filter(Date >= "2020-01-01")

start_year_a <- year(University_Milton_before$Date[1])
start_day_of_year_a <- yday(University_Milton_before$Date[1])
msts <- msts(University_Milton_before$Pass, start = c(start_year_a, start_day_of_year_a), seasonal.periods =c(7,365.25))
msts_before <- na_seasplit(msts) 
ggplot_na_imputations(msts, msts_before, title = "University_Milton_before") 
ggsave(filename = "Outputs/Graphs/Imputed/Imp_University_Milton_before.jpg", width = 17, height = 9, device = "jpeg")

start_year <- year(University_Milton_after$Date[1])
start_day_of_year <- yday(University_Milton_after$Date[1])
msts_after <- msts(University_Milton_after$Pass, start = c(start_year, start_day_of_year), seasonal.periods =c(7,365.25))

msts_University_Milton <- msts(c(msts_before, msts_after), start = c(start_year_a, start_day_of_year_a), seasonal.periods =c(7,365.25))
jpeg(filename = "Outputs/Graphs/Imputed/Imp_University_Milton.jpg", 
     width = 17, height = 9, units = "in", res = 300)
plot <- plot(msts_University_Milton)
print(plot)
dev.off()

# Manual adjustments for ReneLevesque_Wolfe (partial data for 2019, means imputation is underestimating this year)
ReneLevesque_Wolfe_df <- Sensor_List[["ReneLevesque_Wolfe"]]
ReneLevesque_Wolfe_before <- ReneLevesque_Wolfe_df %>% filter(Date < "2020-04-01")
ReneLevesque_Wolfe_after <- ReneLevesque_Wolfe_df  %>% filter(Date >= "2020-04-01")

start_year_a <- year(ReneLevesque_Wolfe_before$Date[1])
start_day_of_year_a <- yday(ReneLevesque_Wolfe_before$Date[1])
msts <- msts(ReneLevesque_Wolfe_before$Pass, start = c(start_year_a, start_day_of_year_a), seasonal.periods =c(7,365.25))
msts_before <- na_seasplit(msts) 
ggplot_na_imputations(msts, msts_before, title = "ReneLevesque_Wolfe_before") 
ggsave(filename = "Outputs/Graphs/Imputed/Imp_ReneLevesque_Wolfe_before.jpg", width = 17, height = 9, device = "jpeg")

start_year <- year(ReneLevesque_Wolfe_after$Date[1])
start_day_of_year <- yday(ReneLevesque_Wolfe_after$Date[1])
msts_after <- msts(ReneLevesque_Wolfe_after$Pass, start = c(start_year, start_day_of_year), seasonal.periods =c(7,365.25))

msts_join <- msts(c(msts_before, msts_after), start = c(start_year_a, start_day_of_year_a), seasonal.periods =c(7,365.25))
msts_ReneLevesque_Wolfe <- na_seasplit(msts_join) 
ggplot_na_imputations(msts_join, msts_ReneLevesque_Wolfe, title = "ReneLevesque_Wolfe_after") 
ggsave(filename = "Outputs/Graphs/Imputed/Imp_ReneLevesque_Wolfe_after.jpg", width = 17, height = 9, device = "jpeg")


#################################################################
# Convert times series back to dataframe ----------------------------------
#################################################################

convert_msts_to_vector <- function(msts_object) {
  pass <- round(as.vector(msts_object))
  data_frame <- data.frame(Pass = pass)
  return(data_frame)
}

# Gather all elements of the environment into a list
msts_objects <- sapply(ls(), function(x) if (inherits(get(x), "msts")) x else NULL)

# Filter to keep only msts objects 
msts_objects <- msts_objects[!sapply(msts_objects, is.null) & 
                               names(msts_objects) != "ts" & 
                               names(msts_objects) != "no_outlier" & 
                               names(msts_objects) != "msts" & 
                               names(msts_objects) != "msts_before" & 
                               names(msts_objects) != "msts_after" & 
                               names(msts_objects) != "msts_after" & 
                               names(msts_objects) != "msts_join"]

# Change names in the list back into actual objects
Msts_List <- lapply(msts_objects, function(x) get(x))

# Convert msts objects to vector
Sensor_List_vector <- lapply(Msts_List, convert_msts_to_vector)
names(Sensor_List_vector) <- gsub("msts_", "", names(Sensor_List_vector))

# Paste Pass column from new vectors to old dataframes
Sensor_List_Final <- list()

for (name in names(Sensor_List)) {
  if (name %in% names(Sensor_List_vector)) {
    if (nrow(Sensor_List[[name]]) == nrow(Sensor_List_vector[[name]])) {
      Sensor_List[[name]]$Pass <- Sensor_List_vector[[name]]$Pass
    } else {
      cat(sprintf("The number of rows does not match for '%s'. Replacement was not performed.\n", name))
    }
  }
  Sensor_List_Final[[name]] <- Sensor_List[[name]]
}


#################################################################
# Attach geodata ----------------------------------------------------------
#################################################################

# Bind the geodata to each dataframe 
geodata <- SENSORS %>%distinct(ID, .keep_all = TRUE)

for(i in 1:nrow(geodata)) {
  current_name <- geodata$Name[i]
  current_longitude <- geodata$Longitude[i]
  current_latitude <- geodata$Latitude[i]
  target_df_index <- which(names(Sensor_List_Final) == current_name)
  if(length(target_df_index) == 1) {
    Sensor_List_Final[[target_df_index]]$Longitude <- current_longitude
    Sensor_List_Final[[target_df_index]]$Latitude <- current_latitude
  } else {
    cat("No match:", current_name, "\n")
  }
}

#################################################################
# Save -----------------------------------------------
#################################################################

NAMES <- as.data.frame(names(Sensor_List_Final))

save(Msts_List, NAMES,
     file = "DataTreated/SENSORS_imp_ts(day).Rda")

save(Sensor_List_Final, NAMES,
     file = "DataTreated/SENSORS_imp_df(day).Rda")

