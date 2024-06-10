# This script consolidates raw bike count data from 
# multiple years sourced from Montreal's open data portal at
# https://donnees.montreal.ca/en/dataset/velos-comptage
# It merges the data into a single dataframe, 
# standardizes time formats and sensor names for consistency, 
# and adds geographic coordinates for each sensor.

####################################################
# Install packages and load data ------------------------------------------
####################################################

if (!require(tidyverse)) install.packages("tidyverse");library(tidyverse)

V2009 <- read.csv("DataRaw/comptagevelo2009.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2010 <- read.csv("DataRaw/comptagevelo2010.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2011 <- read.csv("DataRaw/comptagevelo2011.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2012 <- read.csv("DataRaw/comptagevelo2012.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2013 <- read.csv("DataRaw/comptagevelo2013.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2014 <- read.csv("DataRaw/comptagevelo2014.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2015 <- read.csv("DataRaw/comptagevelo2015.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2016 <- read.csv("DataRaw/comptagevelo2016.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2017 <- read.csv("DataRaw/comptagevelo2017.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2018 <- read.csv("DataRaw/comptagevelo2018.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2019 <- read.csv("DataRaw/comptagevelo2019.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2020 <- read.csv("DataRaw/comptagevelo2020.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2021 <- read.csv("DataRaw/comptagevelo2021.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2022 <- read.csv("DataRaw/comptagevelo2022.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
V2023 <- read.csv("DataRaw/comptagevelo2023.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)
LOC <- read.csv("DataRaw/localisation_des_compteurs_velo.csv",header = TRUE, sep = ",", dec = ".",stringsAsFactors = FALSE)

#################################################################
# Standardize sensor names for the geographic coordinate file -----------------------------------
#################################################################

name_changes <- c(
  "Berri1" = "Berri1",
  "Maisonneuve / Berri" = "Maisonneuve_1",
  "Maisonneuve / Peel" = "Maisonneuve_2",
  "Brébeuf / Rachel" = "Brébeuf",
  "Rachel / Papineau" = "Rachel...Papineau",
  "Parc / Duluth" = "Parc",
  "Côte Sainte-Catherine / Stuart" = "CSC..Côte.Sainte.Catherine.",
  "Pierre-Dupuy" = "PierDup",
  "Pont Jacques-Cartier" = "Pont_Jacques_Cartier",
  "Eco-Display - Métro Laurier" = "Totem_Laurier",
  "Notre-Dame" = "Notre.Dame",
  "Rachel / HôteldeVille" = "Rachel...Hôtel.de.Ville",
  "Saint-Antoine" = "Saint.Antoine",
  "René-Lévesque / Wolfe" = "René.Lévesque",
  "Viger / Saint-Urbain" = "Viger",
  "Boyer / Rosemont" = "Boyer",
  "Maisonneuve / Marcil" = "Maisonneuve_3",
  "University / Milton" = "University",
  "Saint-Urbain" = "Saint.Urbain",
  "Boyer / Everett" = "Boyer.2",
  "Christophe-Colomb/Louvain" = "Christophe.Colomb",
  "Eco-Display - Métro Laurier" = "Eco.Totem...Métro.Laurier",
  "Pont Jacques-Cartier" = "Pont.Jacques.Cartier",
  "Saint-Laurent/Bellechasse" = "Saint.Laurent.Bellechasse",
  "Eco-Display Parc Stanley" = "Eco.Display.Parc.Stanley",
  "Edmond Valade" = "Edmond.Valade",
  "Gouin / Lajeunesse" = "Gouin...Lajeunesse"
)

LOC <- LOC %>% mutate(NewName = recode(Nom, !!!name_changes))
LOC$ID<- as.character(LOC$ID)

#################################################################
# Reformat time and dates and convert to long format ----------------------------------------------------------
#################################################################

T2023 <- V2023 %>%
  rename(ID = id_compteur, Pass = nb_passages, Date = date, Time = heure) %>%
  select(ID, Date, Time, Pass)%>%
  mutate(Date = as.Date(Date),Time = if_else(!is.na(Time), hms(Time), NA))

T2022 <- V2022 %>%
  rename(ID = id_compteur, Pass = nb_passages, Date = date, Time = heure) %>%
  select(ID, Date, Time, Pass)%>%
  mutate(Date = as.Date(Date),Time = if_else(!is.na(Time), hms(Time), NA))

T2021 <- V2021 %>%
  rename(ID = id_compteur, Pass = nb_passages, Date = date, Time = heure) %>%
  select(ID, Date, Time, Pass)%>%
  mutate(Date = as.Date(Date),Time = if_else(!is.na(Time), hms(Time), NA))

T2020 <- V2020 %>%
  rename(ID = id_compteur, Pass = nb_passages, Date = date, Time = heure) %>%
  select(ID, Date, Time, Pass)%>%
  mutate(Date = as.Date(Date),Time = if_else(!is.na(Time), hms(Time), NA))

T2019 <- V2019 %>%
  rename(ID = id_compteur, Pass = nb_passages, Date = date, Time = heure) %>%
  select(ID, Date, Time, Pass)%>%
  mutate(Date = as.Date(Date),Time = if_else(!is.na(Time), hms(Time), NA))

T2018 <- V2018 %>%
  gather(key = "ID", value = "Pass", Berri1:Viger) %>%
  rename(Time = X) %>%
  select(ID, Date, Time, Pass)%>%
  mutate(Date = dmy(Date),Time = NA)

T2017 <- V2017 %>%
  gather(key = "ID", value = "Pass", Berri1:Saint.Urbain) %>%
  rename(Time = X) %>%
  select(ID, Date, Time, Pass)%>%
  mutate(Date = dmy(Date),Time = NA)

T2016 <- V2016 %>%
  gather(key = "ID", value = "Pass", Berri1:Viger) %>%
  rename(Time = X) %>%
  select(ID, Date, Time, Pass)%>%
  mutate(Date = dmy(Date),Time = NA)

T2015 <- V2015 %>%
  gather(key = "ID", value = "Pass", Berri1:Viger) %>%
  rename(Time = X) %>%
  select(ID, Date, Time, Pass)%>%
  mutate(Date = dmy(Date),Time = NA)

T2014 <- V2014 %>%
  gather(key = "ID", value = "Pass", Rachel...Papineau:Saint.Urbain) %>%
  mutate(Time = NA) %>%
  select(ID, Date, Time, Pass)%>%
  mutate(Date = as.Date(Date),Time = NA)

T2013 <- V2013 %>%
  gather(key = "ID", value = "Pass", Rachel...Papineau:University) %>%
  mutate(Time = NA) %>%
  select(ID, Date, Time, Pass)%>%
  mutate(Date = as.Date(Date),Time = NA)

T2012 <- V2012 %>%
  gather(key = "ID", value = "Pass", Rachel...Papineau:Pont_Jacques_Cartier) %>%
  mutate(Time = NA) %>%
  select(ID, Date, Time, Pass) %>%
  mutate(Date = dmy(Date),Time = NA)

T2011 <- V2011 %>%
  gather(key = "ID", value = "Pass", Rachel...Papineau:Pont_Jacques_Cartier) %>%
  mutate(Time = NA) %>%
  select(ID, Date, Time, Pass) %>%
  mutate(Date = dmy(Date),Time = NA)

T2010 <- V2010 %>%
  gather(key = "ID", value = "Pass", Rachel...Papineau:PierDup) %>%
  mutate(Time = NA) %>%
  select(ID, Date, Time, Pass) %>%
  mutate(Date = dmy(Date),Time = NA)

T2009 <- V2009 %>%
  gather(key = "ID", value = "Pass", Berri1:Brébeuf) %>%
  mutate(Time = NA) %>%
  select(ID, Date, Time, Pass) %>%
  mutate(Date = dmy(Date),Time = NA)

#################################################################
# Combine datasets ----------------------------------------------------------
#################################################################

# Standardize sensor names across files
T2018$ID <- ifelse(T2018$ID == "Eco.Totem...Métro.Laurier", "Totem_Laurier", T2018$ID)
T2018$ID <- ifelse(T2018$ID == "Pont.Jacques.Cartier", "Pont_Jacques_Cartier", T2018$ID)
T2017$ID <- ifelse(T2017$ID == "Eco.Totem...Métro.Laurier", "Totem_Laurier", T2017$ID)
T2017$ID <- ifelse(T2017$ID == "Pont.Jacques.Cartier", "Pont_Jacques_Cartier", T2017$ID)

# Bind datasets
T_Old <- bind_rows(T2009, T2010, T2011, T2012, T2013, T2014, T2015, T2016, T2017, T2018)
T_Old$ID <- LOC$ID[match(T_Old$ID, LOC$NewName)]
T2023$ID <- as.character(T2023$ID) 
T2022$ID <- as.character(T2022$ID)
T2021$ID <- as.character(T2021$ID)
T2020$ID <- as.character(T2020$ID)
T2019$ID <- as.character(T2019$ID)

BINDED <- bind_rows(T_Old, T2019, T2020, T2021, T2022, T2023)

# Fix bad sensor ID issue
BINDED$ID <- ifelse(BINDED$ID == "mpteur_38", "38", BINDED$ID)
BINDED$ID <- ifelse(BINDED$ID == "mpteur_39", "39", BINDED$ID)

# Add geocoordinates to dataset
BINDED_GEO <- merge(BINDED, LOC[, c("ID", "Latitude", "Longitude", "Nom")], by = "ID")
BINDED_GEO$Year <- str_sub(BINDED_GEO$Date, 1, 4)
sum(is.na(BINDED_GEO$Latitude))

# Fix problematic characters
SENSORS <- BINDED_GEO %>%
  rename(Name = Nom) %>%
  mutate(
    Name = str_replace_all(Name, "Eco.Display|Eco.Display.|Eco-Display - ", ""),
    Name = str_replace_all(Name, " / |/", "_"),
    Name = str_replace_all(Name, " and ", "_"),
    Name = str_replace_all(Name, "-", ""),
    Name = str_replace_all(Name, "é|è|É", "e"),
    Name = str_replace_all(Name, "ô", "o"),
    Name = str_replace_all(Name, " ", ""),
    Name = str_replace_all(Name, "\\(", "_"),
    Name = str_replace_all(Name, "\\)", ""),
    Name = str_replace_all(Name, "dirnord", "NB"),
    Name = str_replace_all(Name, "dirsud", "SB"),
    Name = str_replace_all(Name, "REV", "REV_"),
  )

unique(SENSORS$Name)

#################################################################
# Save output -------------------------------------------------------------
#################################################################

save(SENSORS, file = "DataTreated/SENSORS.rda")
